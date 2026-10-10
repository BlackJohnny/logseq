(ns frontend.handler.meeting
  "Live meeting notes (Electron only).

  While a meeting runs the microphone is (1) recorded to an Opus file inside the
  graph and (2) cut into utterances at pauses, each transcribed by the persistent
  whisper-server (see electron.whisper) and appended as a block to the meeting
  page. `transcript.json` next to the audio is the source of truth: the segments
  carry the audio time of every utterance."
  (:require [clojure.string :as string]
            [electron.ipc :as ipc]
            [frontend.config :as config]
            [frontend.date :as date]
            [frontend.db :as db]
            [frontend.handler.dictation :as dictation]
            [frontend.handler.editor :as editor-handler]
            [frontend.handler.notification :as notification]
            [frontend.handler.page :as page-handler]
            [frontend.state :as state]
            [frontend.util :as util]
            [promesa.core :as p]))

(def ^:private frame-size 2048)                ; samples per audio callback (128 ms at 16 kHz)
(def ^:private frame-ms (/ (* 1000 frame-size) dictation/sample-rate))
(def ^:private min-speech-ms 300)
(def ^:private end-silence-ms 700)             ; pause that ends an utterance
(def ^:private max-utterance-s 25)             ; whisper works best on <30 s
(def ^:private min-threshold 0.015)            ; RMS floor for "speech"

(defonce *state
  (atom {:status :idle                         ; :idle | :starting | :recording | :stopping
         :elapsed 0
         :pending 0                            ; utterances waiting for transcription
         :segments 0}))

(defonce ^:private *session (atom nil))

;; --- helpers -------------------------------------------------------------------

(defn- rms [^js samples]
  (let [n (.-length samples)]
    (loop [i 0 acc 0.0]
      (if (< i n)
        (let [v (aget samples i)] (recur (inc i) (+ acc (* v v))))
        (js/Math.sqrt (/ acc (max 1 n)))))))

(defn- slugify [s]
  (-> (string/lower-case s)
      (string/replace #"[^a-z0-9]+" "-")
      (string/replace #"^-+|-+$" "")
      (#(if (string/blank? %) "meeting" %))))

(defn- two [n] (if (< n 10) (str "0" n) (str n)))

(defn- timestamp-str [^js d]
  (str (.getFullYear d) "-" (two (inc (.getMonth d))) "-" (two (.getDate d))))

(defn- unique-page-name [base]
  (loop [n 1]
    (let [candidate (if (= n 1) base (str base " (" n ")"))]
      (if (db/get-page candidate) (recur (inc n)) candidate))))

(defn- junk-text? [text]
  (or (string/blank? text)
      (re-matches #"[\W_]*" text)))

;; --- persistence ---------------------------------------------------------------

(defn- write-transcript! [{:keys [repo-dir transcript-path meta segments]} extra]
  (let [data (clj->js (merge meta extra {:segments @segments}))]
    (ipc/ipc "meeting/write-text" repo-dir transcript-path (js/JSON.stringify data nil 2))))

(defn- append-audio! [{:keys [repo-dir audio-path audio-chain]} ^js blob]
  (swap! audio-chain
         (fn [chain]
           (p/then chain
                   (fn [_]
                     (p/let [b64 (dictation/blob->base64 blob)]
                       (ipc/ipc "meeting/append-file" repo-dir audio-path b64)))))))

;; --- transcription queue ---------------------------------------------------------

(defn- insert-block!
  "Utterances are grouped under one parent block per minute of the meeting
  (\"05:00\"), so a long meeting stays a short, collapsible outline."
  [{:keys [page-name minute audio-ref]} {:keys [text start]}]
  (let [m (js/Math.floor (/ start 60))
        macro (fn [secs] (str "{{audio-timestamp " audio-ref ", " (js/Math.floor secs) "}}"))
        current @minute
        parent-uuid (if (= m (:minute current))
                      (:uuid current)
                      (let [u (db/new-block-id)]
                        (editor-handler/api-insert-new-block!
                         (macro (* m 60))
                         {:page page-name :custom-uuid u :edit-block? false})
                        (reset! minute {:minute m :uuid u})
                        u))]
    (editor-handler/api-insert-new-block!
     (str (macro start) " " text)
     {:block-uuid parent-uuid :sibling? false :edit-block? false})))

(defn- process-utterance!
  [{:keys [segments] :as session} {:keys [chunks start end]}]
  (-> (p/let [b64 (dictation/blob->base64 (dictation/encode-wav chunks))
              r (dictation/<ipc "whisper/server-transcribe" b64)]
        (if-not (:ok r)
          (js/console.warn "meeting: transcription failed" (:error r))
          (let [text (:text r)]
            (when-not (junk-text? text)
              (let [segment {:id (count @segments) :start start :end end :text text}]
                (swap! segments conj segment)
                (swap! *state assoc :segments (count @segments))
                (insert-block! session segment)
                (write-transcript! session {}))))))
      (p/catch (fn [e] (js/console.warn "meeting: utterance failed" e)))
      (p/finally #(swap! *state update :pending dec))))

(defn- enqueue! [{:keys [chain] :as session} utterance]
  (swap! *state update :pending inc)
  (swap! chain (fn [c] (p/then c #(process-utterance! session utterance)))))

;; --- voice activity detection ----------------------------------------------------

(defn- finalize-utterance!
  "Close the current utterance (if it contained enough speech) and queue it."
  [{:keys [vad time-offset] :as session}]
  (let [{:keys [frames speech-ms start-sample]} @vad
        samples (reduce + (map #(.-length ^js %) frames))]
    (when (and (seq frames) (>= speech-ms min-speech-ms))
      (let [start (+ time-offset (/ start-sample dictation/sample-rate))]
        (enqueue! session {:chunks frames :start start
                           :end (+ start (/ samples dictation/sample-rate))})))
    (swap! vad assoc :frames [] :in-speech? false :silence-ms 0 :speech-ms 0)))

(defn- on-frame! [{:keys [vad] :as session} ^js frame]
  (let [{:keys [in-speech? noise sample-pos preroll]} @vad
        n (.-length frame)
        level (rms frame)
        threshold (max min-threshold (* 3.5 noise))
        speech? (> level threshold)]
    (swap! vad assoc :sample-pos (+ sample-pos n))
    (cond
      speech?
      (do
        (if in-speech?
          (swap! vad #(-> % (update :frames conj frame) (assoc :silence-ms 0) (update :speech-ms + frame-ms)))
          (swap! vad assoc
                 :in-speech? true :silence-ms 0 :speech-ms frame-ms
                 :frames (conj (vec preroll) frame)
                 :start-sample (- sample-pos (reduce + (map #(.-length ^js %) preroll)))))
        (when (>= (reduce + (map #(.-length ^js %) (:frames @vad)))
                  (* max-utterance-s dictation/sample-rate))
          (finalize-utterance! session)))

      in-speech?
      (do (swap! vad #(-> % (update :frames conj frame) (update :silence-ms + frame-ms)))
          (when (>= (:silence-ms @vad) end-silence-ms)
            (finalize-utterance! session)))

      :else
      ;; track background noise and keep a short pre-roll so word onsets aren't clipped
      (swap! vad #(-> %
                      (assoc :noise (+ (* 0.95 noise) (* 0.05 level)))
                      (assoc :preroll (vec (take-last 2 (conj (vec preroll) frame)))))))))

;; --- start / stop ----------------------------------------------------------------

(defn recording? [] (= :recording (:status @*state)))

(defn- fail! [msg]
  (swap! *state assoc :status :idle)
  (swap! dictation/*state assoc :busy-elsewhere? false)
  (notification/show! msg :error))

(defn- recorder-mime []
  (first (filter #(.isTypeSupported js/MediaRecorder %)
                 ["audio/webm;codecs=opus" "audio/ogg;codecs=opus" "audio/webm"])))

(defn- open-capture!
  "Start the microphone: Opus recording to disk + PCM frames for transcription.
  `*session` is the session atom; returns the capture handles to merge into it."
  [*session]
  (p/let [stream (js/navigator.mediaDevices.getUserMedia
                  #js {:audio #js {:channelCount 1 :echoCancellation true :noiseSuppression true}})]
    (let [ctx (js/AudioContext. #js {:sampleRate dictation/sample-rate})
          src (.createMediaStreamSource ctx stream)
          proc (.createScriptProcessor ctx frame-size 1 1)
          mime (recorder-mime)
          recorder (js/MediaRecorder. stream #js {:mimeType mime})
          rec-start (js/Date.now)
          first-frame? (atom true)]
      (set! (.-ondataavailable recorder)
            (fn [^js e] (when (pos? (.-size (.-data e))) (append-audio! @*session (.-data e)))))
      (.start recorder 3000)
      (set! (.-onaudioprocess proc)
            (fn [^js e]
              (when @first-frame?
                (reset! first-frame? false)
                ;; audio-file time of PCM sample 0 (this callback delivers the first frame)
                (swap! *session assoc :time-offset (/ (- (js/Date.now) frame-ms rec-start) 1000))
                (swap! *state assoc :status :recording)
                (dictation/beep! 880 90))
              (on-frame! @*session (js/Float32Array. (.getChannelData (.-inputBuffer e) 0)))))
      (.connect src proc)
      (.connect proc (.-destination ctx))
      (let [timer (js/setInterval #(swap! *state assoc :elapsed (quot (- (js/Date.now) rec-start) 1000)) 500)]
        {:stream stream :ctx ctx :proc proc :recorder recorder :timer timer :rec-start rec-start}))))

(defn start! [title]
  (when (and (util/electron?) (= :idle (:status @*state)) (= :idle (:status @dictation/*state)))
    (swap! *state assoc :status :starting :elapsed 0 :pending 0 :segments 0)
    (swap! dictation/*state assoc :busy-elsewhere? true)
    (-> (p/let [engine (or (:engine @dictation/*state) (dictation/refresh-status!))
                model (dictation/selected-model)]
          (cond
            (not (:available engine))
            (fail! "Live notes are not available: whisper binaries are missing (run scripts/build-whisper.sh).")

            (not (get-in engine [:models (keyword model) :installed]))
            (do (fail! "Download a dictation model first (Settings → Dictation).")
                (state/open-settings! :dictation))

            :else
            (p/let [server (dictation/<ipc "whisper/server-start"
                                           {:model model
                                            :language (dictation/get-pref :language "auto")
                                            :backend (dictation/get-pref :backend "auto")})]
              (if-not (:ok server)
                (fail! (str "Could not start the speech engine: " (:error server)))
                (let [now (js/Date.now)
                      d (js/Date. now)
                      title (if (string/blank? title) "Meeting" (string/trim title))
                      repo (state/get-current-repo)
                      repo-dir (config/get-repo-dir repo)
                      dir (str "assets/meetings/" (timestamp-str d) "-" (two (.getHours d)) (two (.getMinutes d)) "-" (slugify title))
                      page-name (unique-page-name (str "meetings/" (timestamp-str d) " " title))
                      audio-path (str dir "/audio.webm")
                      transcript-path (str dir "/transcript.json")
                      meta {:title title :page page-name :audio "audio.webm"
                            :started-at (.toISOString d)
                            :language (dictation/get-pref :language "auto")
                            :model model :backend (:backend server)}
                      session (atom {:repo-dir repo-dir :page-name page-name
                                     :audio-path audio-path :transcript-path transcript-path
                                     :audio-chain (atom (p/resolved nil))
                                     :chain (atom (p/resolved nil))
                                     :segments (atom [])
                                     :minute (atom nil)
                                     :audio-ref (str "../" audio-path)
                                     :meta meta
                                     :time-offset 0
                                     :vad (atom {:frames [] :preroll [] :in-speech? false :silence-ms 0
                                                 :speech-ms 0 :sample-pos 0 :start-sample 0 :noise 0.005})})]
                  (page-handler/create! page-name
                                        {:redirect? true
                                         :create-first-block? false
                                         :properties {:type "meeting"
                                                      :audio (str "../" audio-path)
                                                      :transcript (str "../" transcript-path)}})
                  (write-transcript! @session {})
                  (reset! *session session)
                  (p/let [opened (open-capture! session)]
                    (swap! session merge opened)))))))
        (p/catch (fn [e] (fail! (str "Cannot start the meeting: " (or (.-message e) e))))))))

(defn stop! []
  (when-let [session @*session]
    (reset! *session nil)
    (let [{:keys [^js stream ^js ctx ^js proc ^js recorder timer chain audio-chain rec-start] :as s} @session]
      (swap! *state assoc :status :stopping)
      (js/clearInterval timer)
      (set! (.-onaudioprocess proc) nil)
      (.disconnect proc)
      (finalize-utterance! s)
      (dictation/beep! 440 90)
      (-> (p/let [_ (p/create (fn [resolve _]
                                (set! (.-onstop recorder) #(resolve true))
                                (.stop recorder)))
                  _ (do (doseq [^js track (.getTracks stream)] (.stop track))
                        (.close ctx))
                  _ @audio-chain
                  _ @chain
                  _ (write-transcript! s {:ended-at (.toISOString (js/Date.))
                                          :duration (/ (- (js/Date.now) rec-start) 1000)})
                  _ (ipc/ipc "whisper/server-stop")]
            (notification/show! (str "Meeting saved: " (:page-name s)) :success))
          (p/catch (fn [e] (notification/show! (str "Meeting stopped with an error: " (or (.-message e) e)) :error)))
          (p/finally (fn []
                       (swap! *state assoc :status :idle)
                       (swap! dictation/*state assoc :busy-elsewhere? false)))))))
