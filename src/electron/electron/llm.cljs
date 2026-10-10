(ns electron.llm
  "Chat requests to an OpenAI-compatible server (Ollama, llama.cpp, vLLM, LM Studio,
  a remote GPU box...). Made from the main process so CORS and mixed-content rules
  never get in the way of a server on the local network."
  (:require [clojure.string :as string]
            [electron.logger :as logger]
            [promesa.core :as p]))

(defonce ^:private *requests (atom {})) ; request id -> AbortController

(def ^:private timeout-ms (* 20 60 1000))

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
          (p/catch (fn [e] {:ok false :error (str "Cannot reach the server: " (.-message e))}))
          (p/finally #(js/clearTimeout timer))))))

(defn- post-chat! [^js ctl {:keys [url api-key model]} messages {:keys [temperature json?]}]
  (js/fetch (str (base-url url) "/v1/chat/completions")
            (clj->js {:method "POST"
                      :headers (headers api-key)
                      :signal (.-signal ctl)
                      :body (js/JSON.stringify
                             (clj->js (cond-> {:model model :messages messages :stream false
                                               :temperature (or temperature 0.2)}
                                        json? (assoc :response_format {:type "json_object"}))))})))

(defn chat!
  "One non-streaming chat completion. Resolves {:ok true :text ..} or {:ok false :error ..}.
  With :json? the server is asked for a JSON object; servers that reject that option
  are retried without it."
  [request-id config messages opts]
  (let [ctl (js/AbortController.)
        timer (js/setTimeout #(.abort ctl) timeout-ms)
        finish (fn [^js res]
                 (if (.-ok res)
                   (p/then (.json res)
                           (fn [^js body]
                             (let [text (some-> body .-choices (aget 0) .-message .-content)]
                               (if (string? text)
                                 {:ok true :text text}
                                 {:ok false :error "The server answered without any text."}))))
                   (p/then (error-text res) (fn [e] {:ok false :error e}))))]
    (swap! *requests assoc request-id ctl)
    (-> (p/let [res (post-chat! ctl config messages opts)
                res (if (and (= 400 (.-status res)) (:json? opts))
                      (do (logger/debug "llm: server rejected response_format, retrying without it")
                          (post-chat! ctl config messages (assoc opts :json? false)))
                      res)]
          (finish res))
        (p/catch (fn [e]
                   {:ok false
                    :error (if (= "AbortError" (.-name e))
                             "Cancelled or timed out."
                             (str "Cannot reach the server: " (.-message e)))}))
        (p/finally (fn []
                     (js/clearTimeout timer)
                     (swap! *requests dissoc request-id))))))

(defn cancel! [request-id]
  (when-let [^js ctl (get @*requests request-id)]
    (.abort ctl))
  nil)
