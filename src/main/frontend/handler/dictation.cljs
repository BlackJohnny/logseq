(ns frontend.handler.dictation
  "Voice dictation (Electron only): records the microphone, sends a 16 kHz mono
  WAV to the main process (electron.whisper) and inserts the transcript."
  (:require [clojure.string :as string]
            [electron.ipc :as ipc]
            [frontend.commands :as commands]
            [frontend.date :as date]
            [frontend.db :as db]
            [frontend.handler.editor :as editor-handler]
            [frontend.handler.notification :as notification]
            [frontend.state :as state]
            [frontend.util :as util]
            [goog.dom :as gdom]
            [promesa.core :as p]))

(def ^:private sample-rate 16000)
(def ^:private min-seconds 0.5)

(defonce *state
  ;; :status is :idle | :starting | :recording | :transcribing
  (atom {:status :idle
         :elapsed 0
         :engine nil           ; result of :whisper/status
         :download nil}))      ; {:model .. :received .. :total ..} while downloading

(defonce ^:private *recording (atom nil))

;; --- preferences (per device, so plain localStorage) -----------------------------

(defn- pref-key [k] (str "dictation-" (name k)))

(defn get-pref [k default]
  (or (try (not-empty (.getItem js/localStorage (pref-key k))) (catch :default _ nil))
      default))

(defn set-pref! [k v]
  (try (.setItem js/localStorage (pref-key k) v) (catch :default _ nil)))

;; --- engine status / model download ----------------------------------------------

(defn- <ipc [& args]
  (p/let [r (apply ipc/ipc args)]
    (js->clj r :keywordize-keys true)))

(defn refresh-status! []
  (p/let [engine (<ipc "whisper/status" (get-pref :backend "auto"))]
    (swap! *state assoc :engine engine)
    engine))

(defn selected-model []
  (get-pref :model (get-in @*state [:engine :default-model])))

(defn download-model! [model]
  (swap! *state assoc :download {:model model :received 0 :total 0})
  (p/let [r (<ipc "whisper/download-model" model)]
    (swap! *state assoc :download nil)
    (if (:ok r)
      (refresh-status!)
      (notification/show! (str "Model download failed: " (:error r)) :error))))

(defn cancel-download! [model]
  (ipc/ipc "whisper/cancel-download" model))

(defn on-download-progress
  "Called by electron.listener with each `whisper-download-progress` event."
  [data]
  (let [{:keys [model received total]} (js->clj data :keywordize-keys true)]
    (swap! *state update :download #(when % (assoc % :model model :received received :total total)))))

;; --- audio -----------------------------------------------------------------------

(defn- encode-wav
  "Float32 chunks -> 16-bit PCM mono WAV (as a Blob)."
  [chunks]
  (let [n (reduce + (map #(.-length ^js %) chunks))
        buf (js/ArrayBuffer. (+ 44 (* 2 n)))
        view (js/DataView. buf)
        write-str (fn [offset s] (dotimes [i (count s)] (.setUint8 view (+ offset i) (.charCodeAt s i))))]
    (write-str 0 "RIFF")
    (.setUint32 view 4 (+ 36 (* 2 n)) true)
    (write-str 8 "WAVEfmt ")
    (.setUint32 view 16 16 true)
    (.setUint16 view 20 1 true)                         ; PCM
    (.setUint16 view 22 1 true)                         ; mono
    (.setUint32 view 24 sample-rate true)
    (.setUint32 view 28 (* 2 sample-rate) true)         ; byte rate
    (.setUint16 view 32 2 true)                         ; block align
    (.setUint16 view 34 16 true)                        ; bits per sample
    (write-str 36 "data")
    (.setUint32 view 40 (* 2 n) true)
    (loop [chunks chunks offset 44]
      (when-let [^js chunk (first chunks)]
        (dotimes [i (.-length chunk)]
          (let [s (-> (aget chunk i) (max -1) (min 1))]
            (.setInt16 view (+ offset (* 2 i)) (if (neg? s) (* s 0x8000) (* s 0x7FFF)) true)))
        (recur (rest chunks) (+ offset (* 2 (.-length chunk))))))
    (js/Blob. #js [buf] #js {:type "audio/wav"})))

(defn- blob->base64 [^js blob]
  (p/create
   (fn [resolve reject]
     (let [reader (js/FileReader.)]
       (set! (.-onload reader) #(resolve (second (string/split (.-result reader) #"," 2))))
       (set! (.-onerror reader) reject)
       (.readAsDataURL reader blob)))))

;; --- inserting the transcript ------------------------------------------------------

(defn- insert-at-cursor! [id text]
  (let [input (gdom/getElement id)
        before (when input (subs (.-value input) 0 (.-selectionStart input)))
        sep (if (and (seq before) (not (re-find #"\s$" before))) " " "")]
    (commands/simple-insert! id (str sep text) nil)))

(defn- insert-text!
  "Insert at the cursor when editing. Otherwise go back to the block (and cursor
  position) that was being edited when recording started, or fall back to
  today's journal."
  [text {:keys [uuid pos]}]
  (if-let [id (state/get-edit-input-id)]
    (insert-at-cursor! id text)
    (if-let [block (and uuid (db/pull [:block/uuid uuid]))]
      (do (editor-handler/edit-block! block (or pos :max) uuid)
          ;; wait for the editor textarea to mount
          (js/setTimeout #(if-let [id (state/get-edit-input-id)]
                            (insert-at-cursor! id text)
                            (notification/show! "Could not insert the transcript." :warning))
                         80))
      (when-not (editor-handler/api-insert-new-block! text {:page (date/today)})
        (notification/show! "Could not insert the transcript: open a block first." :warning)))))

;; --- recording ---------------------------------------------------------------------

(defn recording? [] (= :recording (:status @*state)))

(defn- beep!
  "Short tone so the user knows when to start (or stop) talking."
  [freq ms]
  (try
    (let [ctx (js/AudioContext.)
          osc (.createOscillator ctx)
          gain (.createGain ctx)]
      (set! (.-value (.-frequency osc)) freq)
      (set! (.-value (.-gain gain)) 0.1)
      (.connect osc gain)
      (.connect gain (.-destination ctx))
      (set! (.-onended osc) #(.close ctx))
      (.start osc)
      (.stop osc (+ (.-currentTime ctx) (/ ms 1000))))
    (catch :default _ nil)))

(defn- release! [{:keys [^js stream ^js ctx ^js proc timer]}]
  (js/clearInterval @timer)
  (set! (.-onaudioprocess proc) nil)
  (.disconnect proc)
  (doseq [^js track (.getTracks stream)] (.stop track))
  (.close ctx))

(defn- finish!
  "Stop capturing and transcribe what was recorded."
  [{:keys [chunks origin] :as rec}]
  (release! rec)
  (beep! 440 90)
  (let [seconds (/ (reduce + (map #(.-length ^js %) @chunks)) sample-rate)]
    (if (< seconds min-seconds)
      (swap! *state assoc :status :idle)
      (do
        (swap! *state assoc :status :transcribing)
        (-> (p/let [b64 (blob->base64 (encode-wav @chunks))
                    r (<ipc "whisper/transcribe" b64
                            {:model (selected-model)
                             :language (get-pref :language "auto")
                             :backend (get-pref :backend "auto")})]
              (if (:ok r)
                (when-not (string/blank? (:text r))
                  (insert-text! (string/trim (:text r)) origin))
                (notification/show! (str "Dictation failed: " (:error r)) :error)))
            (p/catch (fn [e] (notification/show! (str "Dictation failed: " e) :error)))
            (p/finally #(swap! *state assoc :status :idle)))))))

(defonce ^:private *last-edit (atom nil))

;; The editor clears its editing state (block + cursor) on any click outside the
;; textarea, and that happens before our click handler runs. So remember the
;; last cursor position seen in the editing textarea.
(defonce ^:private track-cursor!
  (when (util/electron?)
    (.addEventListener
     js/document "selectionchange"
     (fn [_]
       (let [^js el js/document.activeElement]
         (when (and el (= "TEXTAREA" (.-tagName el)) (= (.-id el) (state/get-edit-input-id)))
           (when-let [block (state/get-edit-block)]
             (reset! *last-edit {:uuid (:block/uuid block)
                                 :pos (.-selectionStart el)
                                 :at (js/Date.now)}))))))
    true))

(defn- capture-origin
  "Where the transcript should go: the block being edited, or else the one
  edited most recently (the mic button click may have ended editing)."
  []
  (let [origin (or (when-let [block (state/get-edit-block)]
                     {:uuid (:block/uuid block) :pos (state/get-edit-pos)})
                   (when-let [{:keys [at] :as last-edit} @*last-edit]
                     (when (< (- (js/Date.now) at) 60000)
                       (select-keys last-edit [:uuid :pos]))))]
    (js/console.debug "dictation origin:" (pr-str origin))
    origin))

(defn- fail! [msg level]
  (swap! *state assoc :status :idle)
  (notification/show! msg level))

(defn start! []
  (when (and (util/electron?) (= :idle (:status @*state)))
    (let [origin (capture-origin)]
      (swap! *state assoc :status :starting :elapsed 0)
      (-> (p/let [engine (or (:engine @*state) (refresh-status!))]
            (cond
              (not (:available engine))
              (fail! "Dictation is not available: whisper binaries are missing (run scripts/build-whisper.sh)." :error)

              (not (get-in engine [:models (keyword (selected-model)) :installed]))
              (do (fail! "Download a dictation model first (Settings → Dictation)." :warning)
                  (state/open-settings! :dictation))

              :else
              (p/let [stream (js/navigator.mediaDevices.getUserMedia
                              #js {:audio #js {:channelCount 1 :echoCancellation true :noiseSuppression true}})]
                (let [ctx (js/AudioContext. #js {:sampleRate sample-rate})
                      src (.createMediaStreamSource ctx stream)
                      proc (.createScriptProcessor ctx 4096 1 1)
                      chunks (atom [])
                      timer (atom nil)
                      started (atom nil)]
                  (set! (.-onaudioprocess proc)
                        (fn [^js e]
                          (let [now (js/Date.now)]
                            (when-not @started
                              ;; first audio is flowing: the user can talk now
                              (reset! started now)
                              (reset! timer (js/setInterval #(swap! *state assoc :elapsed (quot (- (js/Date.now) now) 1000)) 250))
                              (swap! *state assoc :status :recording)
                              (beep! 880 90))
                            ;; skip our own beep
                            (when (> now (+ @started 200))
                              (swap! chunks conj (js/Float32Array. (.getChannelData (.-inputBuffer e) 0)))))))
                  (.connect src proc)
                  (.connect proc (.-destination ctx))
                  (reset! *recording {:stream stream :ctx ctx :proc proc :timer timer
                                      :chunks chunks :origin origin})))))
          (p/catch (fn [e]
                     (fail! (str "Cannot access the microphone: " (.-message e)) :error)))))))

(defn stop! []
  (when-let [rec @*recording]
    (reset! *recording nil)
    (finish! rec)))

(defn toggle! []
  (case (:status @*state)
    :idle (start!)
    :recording (stop!)
    nil))
