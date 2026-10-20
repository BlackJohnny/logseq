(ns electron.llm
  "Chat requests to an OpenAI-compatible server (Ollama, llama.cpp, vLLM, LM Studio,
  a remote GPU box...). Made from the main process so CORS and mixed-content rules
  never get in the way of a server on the local network."
  (:require [clojure.string :as string]
            [electron.logger :as logger]
            [promesa.core :as p]))

(defonce ^:private *requests (atom {})) ; request id -> AbortController

;; The answer is streamed. A non-streaming request sends no response headers until the
;; whole answer is written, and Node's fetch gives up after 5 minutes without headers
;; ("fetch failed"), which a long merge step on a local model easily exceeds. So the only
;; limit left is silence: this long without a single token from the server.
(def ^:private idle-timeout-ms (* 15 60 1000))

(defn- describe-error
  "`fetch failed` alone says nothing: the reason is in the error's cause."
  [^js e]
  (let [cause (.-cause e)]
    (str (.-message e)
         (when cause (str " (" (or (.-code cause) (.-message cause)) ")")))))

(defn- base-url
  "`http://host:11434`, `http://host:11434/` and `http://host:11434/v1` all work."
  [url]
  (-> (str url) string/trim (string/replace #"/+$" "") (string/replace #"/v1$" "")))

(defn- headers [api-key]
  (cond-> {"Content-Type" "application/json"}
    (not (string/blank? api-key)) (assoc "Authorization" (str "Bearer " api-key))))

(defn- error-text [^js res]
  (-> (.text res)
      (p/then (fn [t] (str "HTTP " (.-status res) (when (seq t) (str ": " (subs t 0 (min 300 (count t))))))))))

(defn list-models!
  "Resolves {:ok true :models [ids]} or {:ok false :error ..}. Doubles as the connection test."
  [{:keys [url api-key]}]
  (if (string/blank? url)
    (p/resolved {:ok false :error "No server address set."})
    (let [ctl (js/AbortController.)
          timer (js/setTimeout #(.abort ctl) 10000)]
      (-> (js/fetch (str (base-url url) "/v1/models")
                    (clj->js {:headers (headers api-key) :signal (.-signal ctl)}))
          (p/then (fn [^js res]
                    (if (.-ok res)
                      (p/then (.json res)
                              (fn [^js body]
                                {:ok true
                                 :models (vec (sort (keep #(.-id ^js %) (or (.-data body) #js []))))}))
                      (p/then (error-text res) (fn [e] {:ok false :error e})))))
          (p/catch (fn [e] {:ok false :error (str "Cannot reach the server: " (describe-error e))}))
          (p/finally #(js/clearTimeout timer))))))

(defn- post-chat! [^js ctl {:keys [url api-key model]} messages {:keys [temperature json?]}]
  (js/fetch (str (base-url url) "/v1/chat/completions")
            (clj->js {:method "POST"
                      :headers (headers api-key)
                      :signal (.-signal ctl)
                      :body (js/JSON.stringify
                             (clj->js (cond-> {:model model :messages messages :stream true
                                               :temperature (or temperature 0.2)}
                                        json? (assoc :response_format {:type "json_object"}))))})))

(defn- event-text
  "Text carried by one `data: {...}` line of the stream (throws on an error event)."
  [line]
  (let [payload (string/trim (subs line 5))]
    (when (and (seq payload) (not= "[DONE]" payload))
      (let [^js event (js/JSON.parse payload)]
        (when (.-error event)
          (throw (js/Error. (str "The server reported an error: "
                                 (or (some-> event .-error .-message) (.-error event))))))
        (let [content (some-> event .-choices (aget 0) .-delta .-content)]
          (when (string? content) content))))))

(defn- read-stream!
  "Collect the text of a server-sent-events answer. `on-text` is called with the number
  of characters so far, `on-activity` for every chunk received."
  [^js res on-text on-activity]
  (let [reader (.getReader (.-body res))
        decoder (js/TextDecoder.)]
    (p/loop [buffer "" text ""]
      (p/let [^js r (.read reader)]
        (on-activity)
        (let [buffer (str buffer (if (.-done r)
                                   (.decode decoder)
                                   (.decode decoder (.-value r) #js {:stream true})))
              lines (string/split buffer #"\r?\n" -1)
              complete (if (.-done r) lines (butlast lines))
              rest-line (if (.-done r) "" (last lines))
              text (reduce (fn [acc line]
                             (if (string/starts-with? line "data:")
                               (str acc (event-text line))
                               acc))
                           text complete)]
          (on-text (count text))
          (if (.-done r)
            text
            (p/recur rest-line text)))))))

(defn chat!
  "One streamed chat completion. Resolves {:ok true :text ..} or {:ok false :error ..}.
  With :json? the server is asked for a JSON object; servers that reject that option
  are retried without it. `on-progress` gets the number of characters received so far."
  [request-id config messages opts on-progress]
  (let [ctl (js/AbortController.)
        timer (atom nil)
        touch! (fn []
                 (js/clearTimeout @timer)
                 (reset! timer (js/setTimeout #(.abort ctl) idle-timeout-ms)))
        finish (fn [^js res]
                 (cond
                   (not (.-ok res))
                   (p/then (error-text res) (fn [e] {:ok false :error e}))

                   ;; a server that ignores `stream` answers with a plain JSON body
                   (not (string/includes? (str (.get (.-headers res) "content-type")) "text/event-stream"))
                   (p/then (.json res)
                           (fn [^js body]
                             (let [text (some-> body .-choices (aget 0) .-message .-content)]
                               (if (string? text)
                                 {:ok true :text text}
                                 {:ok false :error "The server answered without any text."}))))

                   :else
                   (p/let [text (read-stream! res (or on-progress identity) touch!)]
                     (if (string/blank? text)
                       {:ok false :error "The server answered without any text."}
                       {:ok true :text text}))))]
    (swap! *requests assoc request-id ctl)
    (touch!)
    (-> (p/let [res (post-chat! ctl config messages opts)
                res (if (and (= 400 (.-status res)) (:json? opts))
                      (do (logger/debug "llm: server rejected response_format, retrying without it")
                          (post-chat! ctl config messages (assoc opts :json? false)))
                      res)]
          (finish res))
        (p/catch (fn [e]
                   (logger/error "llm request failed" e)
                   {:ok false
                    :error (if (= "AbortError" (.-name e))
                             (str "Cancelled, or the server sent nothing for "
                                  (quot idle-timeout-ms 60000) " minutes.")
                             (str "Cannot reach the server: " (describe-error e)))}))
        (p/finally (fn []
                     (js/clearTimeout @timer)
                     (swap! *requests dissoc request-id))))))

(defn cancel! [request-id]
  (when-let [^js ctl (get @*requests request-id)]
    (.abort ctl))
  nil)
