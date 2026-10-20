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
            [frontend.util.meeting :as meeting-util]
            [frontend.util.property :as property]
            [frontend.util.speakers :as speakers-util]
            [promesa.core :as p]))

(defonce *state
  (atom {:status :idle        ; :idle | :busy
         :message nil
         :speakers nil        ; [{:id :name :seconds}] from transcript.json
         :segments nil        ; transcript segments (for the sample buttons)
         :selection []        ; uuids (strings) of the blocks selected on the page
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

(defn repo-dir [] (config/get-repo-dir (state/get-current-repo)))

(defn- cache-key [{:keys [audio-rel]}] (str (repo-dir) "::" audio-rel))

;; --- transcript.json --------------------------------------------------------------------

(defn read-transcript [{:keys [transcript-rel]}]
  (p/let [text (ipc/ipc "meeting/read-text" (repo-dir) transcript-rel)]
    (when (seq text) (js->clj (js/JSON.parse text) :keywordize-keys true))))

(defn write-transcript! [{:keys [transcript-rel]} data]
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
  A block is rewritten only if it is still the block we wrote for that utterance
  (see `speakers-util/ours?`: any label, same text); blocks whose text was edited by
  hand are left alone. Returns the number of skipped blocks."
  [{:keys [audio-ref]} segments _old-speakers new-speakers]
  (let [skipped (atom 0)
        changes (vec
                 (for [seg segments
                       :when (:block seg)
                       :let [block (db/pull [:block/uuid (uuid (:block seg))])
                             current (some-> block :block/content
                                             (->> (property/remove-built-in-properties :markdown)))
                             ref (meeting-util/audio-ref-for audio-ref (:audio seg))
                             target (block-content ref seg (label-for new-speakers (:speaker seg)))]
                       :when block
                       :when (not= (some-> current string/trim) (string/trim target))
                       :let [ours? (speakers-util/ours? current (:text seg))
                             _ (when-not ours?
                                 (swap! skipped inc)
                                 (js/console.debug "speakers: block not relabeled" (:block seg)
                                                   (pr-str current) (pr-str (:text seg))))]
                       :when ours?]
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

(defn- file-name [path] (last (string/split (str path) #"/")))

(defn- audio-files
  "Recording files of the meeting in order (just the page's audio for meetings
  recorded before there could be several)."
  [transcript {:keys [audio-rel]}]
  (let [listed (vec (keep :audio (:recordings transcript)))]
    (if (seq listed) listed [(file-name audio-rel)])))

(defn- upload-recordings!
  "Send every recording to the speaker tool as one continuous stream. Resolves to
  {file -> offset in seconds}: where each recording starts in that stream (computed
  from the decoded sample counts, so it is exact and never stored)."
  [token {:keys [audio-rel]} files]
  (let [dir (string/replace-first audio-rel #"[^/]*$" "")]
    (p/then
     (reduce (fn [acc f]
               (p/then acc
                       (fn [{:keys [offsets total]}]
                         (p/let [samples (decode-audio (str dir f))
                                 _ (upload-pcm! token samples)]
                           {:offsets (assoc offsets f total)
                            :total (+ total (/ (.-length samples) dictation/sample-rate))}))))
             (p/resolved {:offsets {} :total 0})
             files)
     :offsets)))

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
                  transcript (read-transcript info)
                  files (audio-files transcript info)
                  offsets (upload-recordings! token info files)
                  _ (set-message! "Identifying the speakers (this can take a few minutes for long meetings)...")
                  result (dictation/<ipc "speakers/diarize" token
                                         {:num-speakers num-speakers :cache-key (cache-key info)})
                  _ (when-not (:ok result) (throw (js/Error. (str (:error result)))))
                  transcript (read-transcript info)
                  old-speakers (:speakers transcript)
                  segments (mapv (fn [s]
                                   (let [off (get offsets (or (:audio s) (first files)) 0)]
                                     (assoc s :speaker (speaker-at (:segments result)
                                                                   (+ off (:start s)) (+ off (:end s))))))
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

(defn sample-segment
  "The longest utterance of speaker `id` (its :audio file and :start), for the preview button."
  [id]
  (->> (:segments @*state)
       (filter #(= id (:speaker %)))
       (sort-by #(- (:start %) (:end %)))
       first))

;; --- corrections from the user's selection ------------------------------------------------

(defn capture-selection!
  "Remember the blocks selected on the page (or the one being edited), before the
  dialog takes over."
  []
  (let [ids (or (seq (state/get-selection-block-ids))
                (some-> (state/get-edit-block) :block/uuid vector))]
    (swap! *state assoc :selection (mapv str ids))))

(defn selected-segments
  "Transcript segments whose block is in the captured selection."
  []
  (let [sel (set (:selection @*state))]
    (filterv #(contains? sel (:block %)) (:segments @*state))))

(defn- find-speaker [speakers id-or-name]
  (if (string? id-or-name)
    (first (filter #(= (string/lower-case id-or-name) (some-> (:name %) string/lower-case)) speakers))
    (first (filter #(= id-or-name (:id %)) speakers))))

(defn- with-seconds
  "Speakers with :seconds recomputed from the segments."
  [speakers segments]
  (let [totals (reduce (fn [m {:keys [speaker start end]}]
                         (if (some? speaker) (update m speaker (fnil + 0) (- end start)) m))
                       {} segments)]
    (mapv #(assoc % :seconds (get totals (:id %) 0)) speakers)))

(defn assign-selection!
  "The selected blocks were said by `who`: a speaker id, or a name. A name that no
  speaker has yet creates a new speaker (so this works before any analysis, and for
  a voice the analysis did not separate). The blocks are pinned to that speaker, then
  every other utterance that clearly sounds more like the pinned examples than like
  the speaker it was given is moved too."
  [info who]
  (let [sel (set (:selection @*state))
        who (if (string? who) (some-> who string/trim not-empty) who)]
    (when (and (= :idle (:status @*state)) (seq sel) (some? who))
      (swap! *state assoc :status :busy :message "Reading the recording...")
      (let [token (str (random-uuid))]
        (-> (p/let [transcript (read-transcript info)
                    speakers (vec (:speakers transcript))
                    existing (find-speaker speakers who)
                    new? (nil? existing)
                    id (if existing
                         (:id existing)
                         (inc (reduce max -1 (map :id speakers))))
                    speakers (if new? (conj speakers {:id id :name who :seconds 0}) speakers)
                    pinned (reduce (fn [m b] (assoc m b id)) (or (:pinned transcript) {}) sel)
                    segments (:segments transcript)
                    files (audio-files transcript info)
                    offsets (upload-recordings! token info files)
                    off (fn [s] (get offsets (or (:audio s) (first files)) 0))
                    _ (set-message! "Comparing voices...")
                    result (dictation/<ipc "speakers/embed-ranges" token
                                           {:ranges (mapv (fn [s] [(+ (off s) (:start s)) (+ (off s) (:end s))]) segments)
                                            :cache-key (cache-key info)})
                    _ (when-not (:ok result) (throw (js/Error. (str (:error result)))))
                    pins (mapv #(get pinned (:block %)) segments)
                    diarization (into {} (map (juxt :id :embedding)) (:speakers result))
                    new-ids (speakers-util/reassign segments (:embeddings result) pins diarization)
                    new-segments (mapv (fn [s sp] (assoc s :speaker sp)) segments new-ids)
                    moved (count (filter true? (map #(not= (:speaker %1) (:speaker %2)) new-segments segments)))
                    speakers (with-seconds speakers new-segments)
                    skipped (relabel! info new-segments speakers speakers)
                    _ (write-transcript! info (assoc transcript :segments new-segments :speakers speakers :pinned pinned))
                    ;; a new named voice is remembered for future meetings
                    voice (when new? (get (speakers-util/centroids segments (:embeddings result) pins diarization) id))
                    _ (when (seq voice) (dictation/<ipc "speakers/save-embedding" who (clj->js voice)))]
              (swap! *state assoc :speakers speakers :segments new-segments :selection [] :status :idle
                     :message (str (when new? (str "New speaker: " who ". "))
                                   moved " utterances changed speaker."
                                   (when (pos? skipped)
                                     (str " " skipped " blocks were edited by hand and were not relabeled."))))
              (refresh-profiles!))
            (p/catch (fn [e]
                       (swap! *state assoc :status :idle :message (str "Failed: " (or (.-message e) e))))))))))
