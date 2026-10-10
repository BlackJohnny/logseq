(ns frontend.handler.minutes
  "AI-written meeting minutes (Electron only). The transcript is sent to an
  OpenAI-compatible server chosen in the settings (Ollama on this computer, or a
  remote box), in parts when it is long, and the answer becomes a \"Minutes\" section
  at the top of the meeting page. Every item cites transcript segments, and its play
  button is computed from those citations (see frontend.util.minutes)."
  (:require [clojure.string :as string]
            [electron.ipc :as ipc]
            [frontend.db :as db]
            [frontend.handler.dictation :as dictation]
            [frontend.handler.editor :as editor-handler]
            [frontend.handler.speakers :as speakers]
            [frontend.state :as state]
            [frontend.util :as util]
            [frontend.util.minutes :as minutes]
            [promesa.core :as p]))

(def default-url "http://localhost:11434")
(def default-part-chars 14000)

(defonce *state
  (atom {:status :idle        ; :idle | :busy
         :message nil
         :error nil
         :last nil}))         ; {:model :generated-at} of the minutes already on the page

(defonce ^:private *request (atom nil)) ; {:id .. :cancelled? bool}

;; --- configuration (per device) ------------------------------------------------------------

(defn config []
  {:url (dictation/get-pref :llm-url default-url)
   :model (dictation/get-pref :llm-model "")
   :api-key (dictation/get-pref :llm-key "")
   :part-chars (or (let [n (js/parseInt (dictation/get-pref :llm-part-chars "") 10)] (when (pos? n) n))
                   default-part-chars)})

(defn local-server?
  "True when the address points at this computer or a private network."
  [url]
  (let [host (try (.-hostname (js/URL. url)) (catch :default _ ""))]
    (boolean (or (#{"localhost" "127.0.0.1" "::1" "[::1]"} host)
                 (re-find #"^(10\.|192\.168\.|172\.(1[6-9]|2\d|3[01])\.)" host)
                 (string/ends-with? host ".local")))))

(defn host-of [url]
  (try (.-host (js/URL. url)) (catch :default _ url)))

;; --- files next to the transcript -----------------------------------------------------------

(defn- minutes-rel [{:keys [transcript-rel]}]
  (string/replace transcript-rel #"transcript\.json$" "minutes.json"))

(defn- read-minutes-file [info]
  (p/let [text (ipc/ipc "meeting/read-text" (speakers/repo-dir) (minutes-rel info))]
    (when (seq text) (js->clj (js/JSON.parse text) :keywordize-keys true))))

(defn- write-minutes-file! [info data]
  (ipc/ipc "meeting/write-text" (speakers/repo-dir) (minutes-rel info)
           (js/JSON.stringify (clj->js data) nil 2)))

(defn load!
  "Show what is already on the page for this meeting."
  [info]
  (p/let [saved (read-minutes-file info)]
    (swap! *state assoc :status :idle :error nil :message nil
           :last (when saved (select-keys saved [:model :generated-at])))))

;; --- talking to the model -----------------------------------------------------------------

(defn- cancelled? [] (:cancelled? @*request))

(defn- chat! [messages]
  (let [id (str (random-uuid))
        {:keys [url api-key model]} (config)]
    (swap! *request assoc :id id)
    (p/let [r (dictation/<ipc "llm/chat" id {:url url :api-key api-key :model model}
                              messages {:json? true :temperature 0.2})]
      (when (cancelled?) (throw (js/Error. "Cancelled.")))
      (if (:ok r) (:text r) (throw (js/Error. (:error r)))))))

(defn- ask-json!
  "Ask for a JSON object; if the answer cannot be read, ask once more."
  [messages]
  (p/let [text (chat! messages)]
    (if-let [data (minutes/extract-json text)]
      data
      (p/let [text2 (chat! (into messages
                                 [{:role "assistant" :content text}
                                  {:role "user" :content "That was not valid JSON. Reply again with ONLY the JSON object, nothing else."}]))]
        (or (minutes/extract-json text2)
            (throw (js/Error. "The model did not return valid JSON. Try a larger model.")))))))

(defn- sequentially
  "Run (f item) for each item, one after another; resolves to the vector of results."
  [f items]
  (reduce (fn [acc item]
            (p/then acc (fn [results]
                          (when (cancelled?) (throw (js/Error. "Cancelled.")))
                          (p/then (f item) #(conj results %)))))
          (p/resolved [])
          items))

;; --- writing the section ---------------------------------------------------------------------------

(defn- first-content-block-uuid
  "uuid of the first top-level block that is not the page-properties block."
  [page-name]
  (when-let [page (db/entity [:block/name (util/page-name-sanity-lc page-name)])]
    (->> (db/sort-by-left (:block/_parent page) page)
         (remove :block/pre-block?)
         first
         :block/uuid)))

(defn- insert-tree!
  "Insert `node` (and its children) and return its uuid. `at` is {:page ..} or
  {:block-uuid .. :sibling? ..}."
  [node at]
  (let [u (db/new-block-id)]
    (editor-handler/api-insert-new-block! (:content node) (assoc at :custom-uuid u :edit-block? false))
    (doseq [child (:children node)]
      (insert-tree! child {:block-uuid u :sibling? false}))
    u))

(defn- write-section!
  "Replace the previous minutes block (if any) and put the new tree at the top of the page."
  [{:keys [page-name audio-ref] :as info} minutes-data known-names]
  (p/let [previous (read-minutes-file info)
          tree (minutes/blocks-tree minutes-data {:audio-ref audio-ref :known-names known-names})]
    (when-let [old (some->> (:block previous) uuid (conj [:block/uuid]) db/pull)]
      (editor-handler/delete-block-aux! old true))
    (let [anchor (first-content-block-uuid page-name)
          block-uuid (insert-tree! tree (if anchor
                                          {:block-uuid anchor :sibling? true :before? true}
                                          {:page page-name}))]
      block-uuid)))

;; --- generation ------------------------------------------------------------------------------------

(defn- transcript->segments [{:keys [segments speakers]}]
  (let [names (into {} (map (juxt :id #(or (not-empty (:name %)) (str "Speaker " (inc (:id %)))))) speakers)]
    (->> segments
         (filter #(not (string/blank? (:text %))))
         (mapv (fn [s] (assoc (select-keys s [:id :start :text])
                              :speaker-name (when (some? (:speaker s)) (get names (:speaker s)))))))))

(defn generate!
  "Generate (or regenerate) the minutes of the meeting page `info`."
  [info]
  (when (= :idle (:status @*state))
    (reset! *request {:cancelled? false})
    (swap! *state assoc :status :busy :error nil :message "Reading the transcript...")
    (-> (p/let [{:keys [model part-chars url]} (config)
                _ (when (string/blank? model)
                    (throw (js/Error. "No model set. Choose one in Settings → Dictation.")))
                transcript (speakers/read-transcript info)
                segments (transcript->segments transcript)
                _ (when (empty? segments) (throw (js/Error. "The meeting has no transcript yet.")))
                parts (minutes/chunk-segments segments part-chars)
                _ (swap! *state assoc :message
                         (str "Sending the transcript to " (host-of url) " (" (count parts) (if (= 1 (count parts)) " part" " parts") ")..."))
                data (if (= 1 (count parts))
                       (ask-json! (minutes/final-messages (first parts)))
                       (p/let [notes (sequentially
                                      (fn [[i part]]
                                        (swap! *state assoc :message (str "Summarizing part " (inc i) " of " (count parts) "..."))
                                        (p/let [d (ask-json! (minutes/part-messages part (inc i) (count parts)))]
                                          (js/JSON.stringify (clj->js d))))
                                      (map-indexed vector parts))]
                         (swap! *state assoc :message "Merging the parts...")
                         (ask-json! (minutes/merge-messages notes))))
                result (minutes/normalize data segments)
                _ (when (minutes/empty-minutes? result)
                    (throw (js/Error. "The model returned no usable minutes.")))
                _ (swap! *state assoc :message "Writing the minutes into the page...")
                known-names (keep #(not-empty (:name %)) (:speakers transcript))
                block-uuid (write-section! info result known-names)
                generated-at (.toISOString (js/Date.))
                _ (write-minutes-file! info (assoc result :block (str block-uuid) :model model
                                                   :generated-at generated-at))]
          (swap! *state assoc :status :idle :message nil :last {:model model :generated-at generated-at}))
        (p/catch (fn [e]
                   (swap! *state assoc :status :idle :message nil
                          :error (if (cancelled?) "Cancelled." (or (.-message e) (str e)))))))))

(defn cancel! []
  (when-let [{:keys [id]} @*request]
    (swap! *request assoc :cancelled? true)
    (when id (ipc/ipc "llm/cancel" id))))

;; --- settings helpers ----------------------------------------------------------------------------------

(defn test-connection!
  "Resolves {:ok true :models [...]} or {:ok false :error ..}."
  []
  (let [{:keys [url api-key]} (config)]
    (dictation/<ipc "llm/models" {:url url :api-key api-key})))
