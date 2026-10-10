(ns frontend.handler.speakers
  "Who said what in a meeting (Electron only): diarizes the recorded audio with the
  main-process `speaker-tool`, recognizes saved voice profiles, lets the user name
  the speakers, and rewrites the meeting page's blocks with the speaker labels.
  The labels also go into transcript.json; voice profiles stay in the app's user
  data (never in the graph)."
  (:require [clojure.string :as string]
            [electron.ipc :as ipc]
            [frontend.config :as config]
            [frontend.db :as db]
            [frontend.handler.dictation :as dictation]
            [frontend.handler.editor :as editor-handler]
            [frontend.handler.meeting :as meeting]
            [frontend.state :as state]
            [promesa.core :as p]))

(defonce *state
  (atom {:status :idle        ; :idle | :busy
         :message nil
         :speakers nil        ; [{:id :name :seconds}] from transcript.json
         :segments nil        ; transcript segments (for the sample buttons)
         :profiles []}))      ; saved voice profiles [{:name :count}]

(defn- set-message! [msg] (swap! *state assoc :message msg))

;; --- locating the meeting -----------------------------------------------------------

(defn- prop [page k]
  (let [v (get-in page [:block/properties k])]
    (if (coll? v) (first v) v)))

(defn- strip-up [path] (string/replace (str path) #"^(\.\./)+" ""))

(defn current-meeting
  "Info about the meeting page being viewed, or nil."
  []
  (when-let [page (some-> (state/get-current-page) db/get-page)]
    (when (and (= "meeting" (prop page :type)) (prop page :audio) (prop page :transcript))
      {:page-name (:block/original-name page)
       :audio-ref (prop page :audio)                 ; as written on the page, "../assets/..."
       :audio-rel (strip-up (prop page :audio))
       :transcript-rel (strip-up (prop page :transcript))})))

(defn- repo-dir [] (config/get-repo-dir (state/get-current-repo)))

(defn- cache-key [{:keys [audio-rel]}] (str (repo-dir) "::" audio-rel))

;; --- transcript.json --------------------------------------------------------------------

(defn- read-transcript [{:keys [transcript-rel]}]
  (p/let [text (ipc/ipc "meeting/read-text" (repo-dir) transcript-rel)]
    (when (seq text) (js->clj (js/JSON.parse text) :keywordize-keys true))))

(defn- write-transcript! [{:keys [transcript-rel]} data]
  (ipc/ipc "meeting/write-text" (repo-dir) transcript-rel (js/JSON.stringify (clj->js data) nil 2)))

;; --- labels on the page ------------------------------------------------------------------

(defn- label-for
  "Label to show for speaker `id`, or nil when there is no analysis yet (or the
  utterance has no speaker)."
  [speakers id]
  (when-let [speaker (and (some? id) (first (filter #(= id (:id %)) speakers)))]
    (let [nm (:name speaker)]
      (if (and nm (not (string/blank? nm)))
        (str "[[" nm "]]:")
        (str "**Vorbitor " (inc id) ":**")))))

(defn- block-content [audio-ref segment label]
  (str (meeting/audio-macro audio-ref (:start segment)) " "
       (when label (str label " "))
       (:text segment)))

(defn- relabel!
  "Give every segment block the label of its current speaker, in one transaction.
  A block is rewritten only if it still holds what we wrote (the old label or none);
  blocks edited by hand are left alone. Returns the number of skipped blocks."
  [{:keys [audio-ref]} segments old-speakers new-speakers]
  (let [skipped (atom 0)
        changes (vec
                 (for [seg segments
                       :when (:block seg)
                       :let [block (db/pull [:block/uuid (uuid (:block seg))])
                             current (some-> block :block/content string/trim)
                             target (block-content audio-ref seg (label-for new-speakers (:speaker seg)))
                             known #{(string/trim (block-content audio-ref seg (label-for old-speakers (:speaker seg))))
                                     (string/trim (block-content audio-ref seg nil))}]
                       :when (and block (not= current (string/trim target)))
                       :let [_ (when-not (contains? known current) (swap! skipped inc))]
                       :when (contains? known current)]
                   [block target]))]
    (when (seq changes)
      (editor-handler/save-blocks! changes))
    @skipped))

;; --- audio -> 16 kHz PCM ------------------------------------------------------------------

(defn- decode-audio [audio-rel]
  (p/let [b64 (ipc/ipc "meeting/read-file" (repo-dir) audio-rel)
          bin (js/atob b64)
          bytes (let [a (js/Uint8Array. (.-length bin))]
                  (dotimes [i (.-length bin)] (aset a i (.charCodeAt bin i)))
                  a)
          ctx (js/OfflineAudioContext. 1 1 dictation/sample-rate) ; decodes+resamples to 16 kHz
          audio (.decodeAudioData ctx (.-buffer bytes))]
    (.getChannelData audio 0)))

(defn- upload-pcm! [token ^js samples]
  (let [chunk 1000000
        total (.-length samples)]
    (reduce (fn [acc from]
              (p/then acc
                      (fn [_]
                        (let [part (.subarray samples from (min total (+ from chunk)))
                              pcm (js/Int16Array. (.-length part))]
                          (dotimes [i (.-length part)]
                            (aset pcm i (js/Math.round (* 32767 (max -1 (min 1 (aget part i)))))))
                          (p/let [b64 (dictation/blob->base64 (js/Blob. #js [pcm]))]
                            (dictation/<ipc "speakers/append-pcm" token b64))))))
            (p/resolved nil)
            (range 0 total chunk))))

;; --- analysis -------------------------------------------------------------------------------

(defn- speaker-at
  "Diarization speaker with the most overlap with [a, b]."
  [diarization a b]
  (let [overlap (reduce (fn [acc {:keys [start end speaker]}]
                          (let [o (- (min b end) (max a start))]
                            (if (pos? o) (update acc speaker (fnil + 0) o) acc)))
                        {} diarization)]
    (when (seq overlap) (key (apply max-key val overlap)))))

(defn refresh-profiles! []
  (p/let [profiles (dictation/<ipc "speakers/list-profiles")]
    (swap! *state assoc :profiles (or profiles []))))

(defn load!
  "Load the saved labels of the meeting page (no analysis)."
  [info]
  (p/let [transcript (read-transcript info)
          _ (refresh-profiles!)]
    (swap! *state assoc :speakers (:speakers transcript) :segments (:segments transcript)
           :status :idle :message nil)))

(defn analyze!
  "Identify the speakers of the meeting. `num-speakers` may be nil (automatic)."
  [info num-speakers]
  (when (= :idle (:status @*state))
    (swap! *state assoc :status :busy :message "Preparing...")
    (let [token (str (random-uuid))]
      (-> (p/let [status (dictation/<ipc "speakers/status")
                  _ (when-not (:available status)
                      (throw (js/Error. "Speaker tools are missing (run scripts/build-speaker-tools.sh).")))
                  _ (when-not (:models-installed status)
                      (set-message! "Downloading the speaker models (about 33 MB)...")
                      (p/let [r (dictation/<ipc "speakers/download-models")]
                        (when-not (:ok r) (throw (js/Error. (str "Model download failed: " (:error r)))))))
                  _ (set-message! "Reading the recording...")
                  samples (decode-audio (:audio-rel info))
                  _ (set-message! "Preparing the audio...")
                  _ (upload-pcm! token samples)
                  _ (set-message! "Identifying the speakers (this can take a few minutes for long meetings)...")
                  result (dictation/<ipc "speakers/diarize" token
                                         {:num-speakers num-speakers :cache-key (cache-key info)})
                  _ (when-not (:ok result) (throw (js/Error. (str (:error result)))))
                  transcript (read-transcript info)
                  old-speakers (:speakers transcript)
                  segments (mapv #(assoc % :speaker (speaker-at (:segments result) (:start %) (:end %)))
                                 (:segments transcript))
                  speakers (mapv (fn [{:keys [id seconds match]}]
                                   {:id id :seconds seconds :name (:name match)})
                                 (:speakers result))
                  skipped (relabel! info segments old-speakers speakers)
                  _ (write-transcript! info (assoc transcript :segments segments :speakers speakers))]
            (swap! *state assoc :speakers speakers :segments segments :status :idle
                   :message (when (pos? skipped)
                              (str skipped " blocks were edited by hand and were not relabeled.")))
            (refresh-profiles!))
          (p/catch (fn [e]
                     (swap! *state assoc :status :idle :message (str "Failed: " (or (.-message e) e)))))))))

(defn rename!
  "Name speaker `id`; relabels the page and saves/updates the voice profile."
  [info id new-name]
  (let [new-name (some-> new-name string/trim not-empty)]
    (-> (p/let [transcript (read-transcript info)
                old-speakers (:speakers transcript)
                speakers (mapv #(if (= id (:id %)) (assoc % :name new-name) %) old-speakers)
                skipped (relabel! info (:segments transcript) old-speakers speakers)
                _ (write-transcript! info (assoc transcript :speakers speakers))
                _ (when new-name
                    (dictation/<ipc "speakers/save-profile" new-name (cache-key info) id))]
          (swap! *state assoc :speakers speakers
                 :message (when (pos? skipped)
                            (str skipped " blocks were edited by hand and were not relabeled.")))
          (refresh-profiles!))
        (p/catch (fn [e] (swap! *state assoc :message (str "Failed: " (or (.-message e) e))))))))

(defn delete-profile! [profile-name]
  (p/let [_ (dictation/<ipc "speakers/delete-profile" profile-name)]
    (refresh-profiles!)))

(defn sample-start
  "Start (seconds) of the longest utterance of speaker `id`, for the preview button."
  [id]
  (->> (:segments @*state)
       (filter #(= id (:speaker %)))
       (sort-by #(- (:start %) (:end %)))
       first
       :start))
