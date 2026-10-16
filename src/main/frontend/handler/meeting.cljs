(ns frontend.handler.meeting
  "Live meeting notes (Electron only).

  While a meeting runs the microphone (and, when enabled, the system audio, i.e. the
  other participants, see electron.system-audio) is (1) recorded as one mixed Opus
  file inside the graph and (2) cut into utterances at pauses, separately per source,
  each transcribed by the persistent
  whisper-server (see electron.whisper) and appended as a block to the meeting
  page. `transcript.json` next to the audio is the source of truth: the segments
  carry the audio time of every utterance.

  A meeting can have several recordings (`continue!`): each has its own audio file
  (audio.webm, audio-2.webm, ...) and its own clock starting at 0, and a section
  block (\"Înregistrarea 2\") on the page that holds its minutes; it is created with
  the first utterance, so a silent recording leaves no empty section. (Meetings
  recorded before this have their first recording's minutes straight in the page.)
  Segments name their `:audio` file; segments without one belong to the page's audio."
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
            [frontend.util.meeting :as meeting-util]
            [promesa.core :as p]))

(def ^:private frame-size 2048)                ; samples per audio callback (128 ms at 16 kHz)
(def ^:private frame-ms (/ (* 1000 frame-size) dictation/sample-rate))
(def ^:private min-speech-ms 300)
(def ^:private end-silence-ms 700)             ; pause that ends an utterance
(def ^:private max-utterance-s 25)             ; whisper works best on <30 s
(def ^:private min-threshold 0.015)            ; RMS floor for "speech"
(def ^:private max-hold-ms 20000)              ; longest a mic segment waits for its system twin

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

(defn- read-json [repo-dir path]
  (p/let [text (ipc/ipc "meeting/read-text" repo-dir path)]
    (when (seq text)
      (try (js->clj (js/JSON.parse text) :keywordize-keys true) (catch :default _ nil)))))

(defn- write-transcript!
  "Write transcript.json. Keys this session does not own (the speakers, set after the
  meeting) are kept from what is on disk."
  [{:keys [repo-dir transcript-path meta segments]} extra]
  (p/let [current (read-json repo-dir transcript-path)
          data (clj->js (merge current meta extra {:segments @segments}))]
    (ipc/ipc "meeting/write-text" repo-dir transcript-path (js/JSON.stringify data nil 2))))

(defn- append-audio! [{:keys [repo-dir audio-path audio-chain]} ^js blob]
  (swap! audio-chain
         (fn [chain]
           (p/then chain
                   (fn [_]
                     (p/let [b64 (dictation/blob->base64 blob)]
                       (ipc/ipc "meeting/append-file" repo-dir audio-path b64)))))))

;; --- transcription queue ---------------------------------------------------------

(defn audio-macro
  "The play-button macro for `seconds` into the meeting audio at `audio-ref`."
  [audio-ref seconds]
  (str "{{audio-timestamp " audio-ref ", " (js/Math.floor seconds) "}}"))

(defn- insert-block!
  "Utterances are grouped under one parent block per minute of the meeting
  (\"05:00\"), so a long meeting stays a short, collapsible outline."
  [{:keys [page-name minute audio-ref section section-title]} {:keys [text start block]}]
  (when-let [title (and section @section-title)]
    (reset! section-title nil)
    (editor-handler/api-insert-new-block!
     title {:page page-name :custom-uuid section :edit-block? false}))
  (let [m (js/Math.floor (/ start 60))
        macro (partial audio-macro audio-ref)
        ;; two sources finish utterances out of order, so remember every minute's parent
        parent-uuid (or (get @minute m)
                        (let [u (db/new-block-id)]
                          (editor-handler/api-insert-new-block!
                           (macro (* m 60))
                           (merge (if section
                                    {:block-uuid section :sibling? false}
                                    {:page page-name})
                                  {:custom-uuid u :edit-block? false}))
                          (swap! minute assoc m u)
                          u))]
    (editor-handler/api-insert-new-block!
     (str (macro start) " " text)
     {:block-uuid parent-uuid :sibling? false :edit-block? false
      :custom-uuid (uuid block)})))

(defn- commit-segment! [{:keys [segments] :as session} segment]
  (let [segment (assoc segment :id (count @segments))]
    ;; held mic segments are committed late, so keep the current recording in audio-time
    ;; order (earlier recordings come first and each has its own clock)
    (swap! segments (fn [all]
                      (let [{same true other false} (group-by #(= (:audio segment) (:audio %)) all)]
                        (vec (concat other (sort-by :start (conj (vec same) segment)))))))
    (swap! *state assoc :segments (count @segments))
    (insert-block! session segment)
    (write-transcript! session {})))

(defn- system-busy?
  "True while the system channel may still produce a segment that a mic segment
  repeats: an utterance is open or still being transcribed."
  [{:keys [system-vad system-pending]}]
  (boolean (and system-vad (or (:in-speech? @system-vad) (pos? @system-pending)))))

(defn- settle-mic!
  "The headset microphone also hears the headphones, so a mic segment that only
  repeats what the system channel said is dropped (the system copy is the clean one)."
  [{:keys [segments] :as session} segment]
  ;; each recording has its own clock, so only compare within the same one
  (if (meeting-util/leak? segment (filter #(= (:audio segment) (:audio %)) @segments))
    (js/console.debug "meeting: dropped a mic segment that repeats the system audio" (:text segment))
    (commit-segment! session segment)))

(defn- flush-held!
  "Decide the mic segments that were waiting for the system channel to go quiet
  (all of them when `force?`, and any that waited longer than max-hold-ms)."
  [{:keys [held] :as session} force?]
  (when (seq @held)
    (let [busy? (system-busy? session)
          now (js/Date.now)
          ready? #(or force? (not busy?) (> (- now (:held-at %)) max-hold-ms))
          ready (filterv ready? @held)]
      (when (seq ready)
        (reset! held (vec (remove ready? @held)))
        (doseq [segment ready]
          (settle-mic! session (dissoc segment :held-at)))))))

(defn- process-utterance!
  [{:keys [system-pending held] :as session} {:keys [chunks start end source]}]
  (-> (p/let [b64 (dictation/blob->base64 (dictation/encode-wav chunks))
              r (dictation/<ipc "whisper/server-transcribe" b64)]
        (if-not (:ok r)
          (js/console.warn "meeting: transcription failed" (:error r))
          (let [text (:text r)]
            (when-not (junk-text? text)
              (let [segment {:start start :end end :text text :source (name source)
                             :audio (:audio-file session)
                             :block (str (db/new-block-id))}]
                (cond
                  (= :system source) (commit-segment! session segment)
                  (system-busy? session) (swap! held conj (assoc segment :held-at (js/Date.now)))
                  :else (settle-mic! session segment)))))))
      (p/catch (fn [e] (js/console.warn "meeting: utterance failed" e)))
      (p/finally (fn []
                   (when (= :system source) (swap! system-pending dec))
                   (swap! *state update :pending dec)))))

(defn- enqueue! [{:keys [chain system-pending] :as session} {:keys [source] :as utterance}]
  (swap! *state update :pending inc)
  (when (= :system source) (swap! system-pending inc))
  (swap! chain (fn [c] (p/then c #(process-utterance! session utterance)))))

;; --- voice activity detection ----------------------------------------------------

(defn- new-vad []
  (atom {:frames [] :preroll [] :in-speech? false :silence-ms 0
         :speech-ms 0 :sample-pos 0 :start-sample 0 :noise 0.005}))

(defn- channel
  "The session as seen by one audio source (:mic or :system): its own VAD state and
  time offset, so finalize-utterance!/on-frame! work the same for both."
  [session source]
  (if (= source :system)
    (assoc session :source :system
           :vad (:system-vad session) :time-offset (:system-time-offset session 0))
    (assoc session :source :mic)))

(defn- finalize-utterance!
  "Close the current utterance (if it contained enough speech) and queue it."
  [{:keys [vad time-offset source] :as session}]
  (let [{:keys [frames speech-ms start-sample]} @vad
        samples (reduce + (map #(.-length ^js %) frames))]
    (when (and (seq frames) (>= speech-ms min-speech-ms))
      (let [start (+ time-offset (/ start-sample dictation/sample-rate))]
        (enqueue! session {:chunks frames :start start :source source
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
  (ipc/ipc "system-audio/stop")
  (swap! *state assoc :status :idle)
  (swap! dictation/*state assoc :busy-elsewhere? false)
  (notification/show! msg :error))

(defn- recorder-mime []
  (first (filter #(.isTypeSupported js/MediaRecorder %)
                 ["audio/webm;codecs=opus" "audio/ogg;codecs=opus" "audio/webm"])))

(defn- system-audio-enabled? []
  (not= "off" (dictation/get-pref :system-audio "on")))

(defn- open-system-stream!
  "MediaStream of what the computer plays, or nil (after telling the user why) so the
  meeting carries on with the microphone only."
  []
  (let [warn! (fn [why]
                (ipc/ipc "system-audio/stop")
                (notification/show!
                 (str "System audio is unavailable, recording the microphone only: " why)
                 :warning)
                nil)]
    (-> (p/let [r (dictation/<ipc "system-audio/start")]
          (if-not (:ok r)
            (warn! (:error r))
            (p/let [devices (.enumerateDevices js/navigator.mediaDevices)
                    dev (last (filter #(and (= "audioinput" (.-kind ^js %)) (= (:label r) (.-label ^js %)))
                                      (array-seq devices)))]
              (if-not dev
                (warn! "the capture device was not found")
                ;; it is not a microphone: no echo cancellation, noise suppression or gain control
                (js/navigator.mediaDevices.getUserMedia
                 #js {:audio #js {:deviceId #js {:exact (.-deviceId ^js dev)}
                                  :channelCount 1
                                  :echoCancellation false :noiseSuppression false
                                  :autoGainControl false}})))))
        (p/catch (fn [e] (warn! (or (.-message e) e)))))))

(defn- open-capture!
  "Start the capture: one Opus recording to disk (microphone and system audio mixed)
  plus 16 kHz PCM frames per source for transcription. `*session` is the session
  atom; returns the capture handles to merge into it."
  [*session]
  (p/let [stream (js/navigator.mediaDevices.getUserMedia
                  #js {:audio #js {:channelCount 1 :echoCancellation true :noiseSuppression true}})
          sys-stream (when (system-audio-enabled?) (open-system-stream!))]
    (let [ctx (js/AudioContext. #js {:sampleRate dictation/sample-rate})
          mime (recorder-mime)
          rec-start (js/Date.now)
          src (.createMediaStreamSource ctx stream)
          sys-src (some->> sys-stream (.createMediaStreamSource ctx))
          ;; the recording is one mono mix: speaker analysis reads a single channel
          rec-stream (if sys-src
                       (let [dest (.createMediaStreamDestination ctx)]
                         (.connect src dest)
                         (.connect sys-src dest)
                         (.-stream dest))
                       stream)
          recorder (js/MediaRecorder. rec-stream #js {:mimeType mime})
          processor (fn [source node on-first]
                      (let [proc (.createScriptProcessor ctx frame-size 1 1)
                            first-frame? (atom true)]
                        (set! (.-onaudioprocess proc)
                              (fn [^js e]
                                (when @first-frame?
                                  (reset! first-frame? false)
                                  ;; audio-file time of PCM sample 0 (this callback delivers the first frame)
                                  (swap! *session assoc (if (= source :system) :system-time-offset :time-offset)
                                         (/ (- (js/Date.now) frame-ms rec-start) 1000))
                                  (on-first))
                                (on-frame! (channel @*session source)
                                           (js/Float32Array. (.getChannelData (.-inputBuffer e) 0)))))
                        (.connect node proc)
                        (.connect proc (.-destination ctx))
                        proc))
          proc (processor :mic src (fn []
                                     (swap! *state assoc :status :recording)
                                     (dictation/beep! 880 90)))
          sys-proc (when sys-src (processor :system sys-src (fn [])))]
      (set! (.-ondataavailable recorder)
            (fn [^js e] (when (pos? (.-size (.-data e))) (append-audio! @*session (.-data e)))))
      (.start recorder 3000)
      (let [timer (js/setInterval (fn []
                                    (swap! *state assoc :elapsed (quot (- (js/Date.now) rec-start) 1000))
                                    (flush-held! @*session false))
                                  500)]
        {:stream stream :sys-stream sys-stream :ctx ctx :proc proc :sys-proc sys-proc
         :recorder recorder :timer timer :rec-start rec-start}))))

(defn- make-session
  "The session atom of one recording. `dir` is the meeting's folder in the graph."
  [{:keys [repo-dir page-name dir audio-file meta segments section section-title]}]
  (atom {:repo-dir repo-dir :page-name page-name
         :audio-file audio-file
         :audio-path (str dir "/" audio-file)
         :audio-ref (str "../" dir "/" audio-file)
         :transcript-path (str dir "/transcript.json")
         :audio-chain (atom (p/resolved nil))
         :chain (atom (p/resolved nil))
         :segments (atom (vec segments))
         :minute (atom {})                     ; minute -> parent block, per recording
         :section section                      ; this recording's parent block
         :section-title (atom section-title)   ; its text until the block is created
         :meta meta
         :time-offset 0
         :vad (new-vad)
         :system-vad (new-vad)
         :system-pending (atom 0)
         :held (atom [])}))

(defn- new-meeting-session!
  "A new meeting: creates its page; the first recording gets the section \"Înregistrarea 1\"."
  [title model server]
  (let [d (js/Date.)
        title (if (string/blank? title) "Meeting" (string/trim title))
        repo-dir (config/get-repo-dir (state/get-current-repo))
        dir (str "assets/meetings/" (timestamp-str d) "-" (two (.getHours d)) (two (.getMinutes d)) "-" (slugify title))
        page-name (unique-page-name (str "meetings/" (timestamp-str d) " " title))
        session (make-session
                 {:repo-dir repo-dir :page-name page-name :dir dir :audio-file "audio.webm"
                  :section (db/new-block-id) :section-title "Înregistrarea 1"
                  :meta {:title title :page page-name :audio "audio.webm"
                         :started-at (.toISOString d)
                         :language (dictation/get-pref :language "auto")
                         :model model :backend (:backend server)
                         :recordings [{:audio "audio.webm" :started-at (.toISOString d)}]}})]
    (page-handler/create! page-name
                          {:redirect? true
                           :create-first-block? false
                           :properties {:type "meeting"
                                        :audio (str "../" dir "/audio.webm")
                                        :transcript (str "../" dir "/transcript.json")}})
    (p/resolved session)))

(defn- continued-session!
  "Another recording of an existing meeting: its own audio file, and a section block
  (\"Înregistrarea N\") at the end of the page that will hold its minutes."
  [{:keys [page-name transcript-rel]}]
  (let [repo-dir (config/get-repo-dir (state/get-current-repo))
        dir (string/replace-first transcript-rel #"/[^/]*$" "")]
    (p/let [t (read-json repo-dir transcript-rel)]
      (when-not t (throw (js/Error. "this meeting has no transcript.json")))
      (let [;; meetings recorded before there could be several have no :recordings
            recordings (or (not-empty (vec (:recordings t)))
                           [(into {:audio (or (:audio t) "audio.webm")}
                                  (filter (comp some? val))
                                  (select-keys t [:started-at :ended-at :duration]))])
            n (inc (count recordings))
            audio-file (str "audio-" n ".webm")
            section (db/new-block-id)]
        (make-session
         {:repo-dir repo-dir :page-name page-name :dir dir :audio-file audio-file
          :section section :section-title (str "Înregistrarea " n)
          :segments (:segments t)
          :meta {:recordings (conj recordings {:audio audio-file
                                               :started-at (.toISOString (js/Date.))})}})))))

(defn- begin!
  "Start recording. `continue-info` (from speakers/current-meeting) adds a recording to
  that meeting, otherwise a new meeting called `title` is created."
  [title continue-info]
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
                (p/let [session (if continue-info
                                  (continued-session! continue-info)
                                  (new-meeting-session! title model server))
                        _ (write-transcript! @session {})]
                  (swap! *state assoc :segments (count @(:segments @session)))
                  (reset! *session session)
                  (p/let [opened (open-capture! session)]
                    (swap! session merge opened)))))))
        (p/catch (fn [e] (fail! (str "Cannot start the meeting: " (or (.-message e) e))))))))

(defn start! [title] (begin! title nil))

(defn continue!
  "Record another part of the meeting page described by `info` (speakers/current-meeting)."
  [info]
  (begin! nil info))

(defn stop! []
  (when-let [session @*session]
    (reset! *session nil)
    (let [{:keys [^js stream ^js sys-stream ^js ctx ^js proc ^js sys-proc ^js recorder
                  timer chain audio-chain rec-start] :as s} @session]
      (swap! *state assoc :status :stopping)
      (js/clearInterval timer)
      (doseq [^js node [proc sys-proc] :when node]
        (set! (.-onaudioprocess node) nil)
        (.disconnect node))
      (finalize-utterance! (channel s :mic))
      (when sys-proc (finalize-utterance! (channel s :system)))
      (dictation/beep! 440 90)
      (-> (p/let [_ (p/create (fn [resolve _]
                                (set! (.-onstop recorder) #(resolve true))
                                (.stop recorder)))
                  _ (do (doseq [^js st [stream sys-stream] :when st
                                ^js track (.getTracks st)]
                          (.stop track))
                        (ipc/ipc "system-audio/stop")
                        (.close ctx))
                  _ @audio-chain
                  _ @chain
                  _ (flush-held! s true)
                  _ (let [seconds (/ (- (js/Date.now) rec-start) 1000)
                          recordings (:recordings (:meta s))
                          recordings (conj (vec (butlast recordings))
                                           (assoc (last recordings) :duration seconds
                                                  :ended-at (.toISOString (js/Date.))))]
                      (write-transcript! s {:ended-at (:ended-at (last recordings))
                                            :recordings recordings
                                            :duration (reduce + (keep :duration recordings))}))
                  _ (ipc/ipc "whisper/server-stop")]
            (notification/show! (str "Meeting saved: " (:page-name s)) :success))
          (p/catch (fn [e] (notification/show! (str "Meeting stopped with an error: " (or (.-message e) e)) :error)))
          (p/finally (fn []
                       (swap! *state assoc :status :idle)
                       (swap! dictation/*state assoc :busy-elsewhere? false)))))))
