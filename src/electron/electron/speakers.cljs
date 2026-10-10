(ns electron.speakers
  "Speaker diarization and voice profiles for meeting notes, on top of the
  `speaker-tool` helper (scripts/build-speaker-tools.sh). Voice profiles live in
  the app's user data, not in any graph."
  (:require ["child_process" :as child-process]
            ["crypto" :as crypto]
            ["electron" :refer [app]]
            ["fs" :as node-fs]
            ["fs-extra" :as fs]
            ["os" :as os]
            ["path" :as node-path]
            [clojure.string :as string]
            [electron.logger :as logger]
            [electron.whisper :as whisper]
            [promesa.core :as p]))

(def ^:private segmentation-model
  {:file "pyannote-segmentation-3-0.onnx"
   :url "https://huggingface.co/csukuangfj/sherpa-onnx-pyannote-segmentation-3-0/resolve/main/model.onnx"})

(def ^:private embedding-model
  {:id "wespeaker_en_voxceleb_resnet34"
   :file "wespeaker_en_voxceleb_resnet34.onnx"
   :url "https://github.com/k2-fsa/sherpa-onnx/releases/download/speaker-recongition-models/wespeaker_en_voxceleb_resnet34.onnx"})

;; Cosine similarity above which a speaker is recognized as a saved voice profile.
;; Measured on a two-speaker sample: same speaker 0.83, different speakers 0.54.
(def ^:private match-threshold 0.65)

(def ^:private sample-rate 16000)

;; --- paths ---------------------------------------------------------------------

(defn- data-dir [& parts]
  (apply node-path/join (.getPath ^js app "userData") "speakers" parts))

(defn- model-path [{:keys [file]}] (data-dir "models" file))

(defn- speaker-dir []
  (->> [js/process.env.LOGSEQ_SPEAKERS_DIR
        (node-path/join (or js/process.resourcesPath "") "speakers")
        (node-path/join (.getAppPath ^js app) ".." ".whisper-build" "dist" "speakers")]
       (filter seq)
       (filter #(fs/pathExistsSync (node-path/join % "bin" "speaker-tool")))
       first))

(defn- tool-path []
  (some-> (speaker-dir) (node-path/join "bin" "speaker-tool")))

(defn- models-installed? []
  (and (fs/pathExistsSync (model-path segmentation-model))
       (fs/pathExistsSync (model-path embedding-model))))

;; --- voice profiles ----------------------------------------------------------------

(defn- profiles-file [] (data-dir "voiceprints.json"))

(defn- read-profiles []
  (let [f (profiles-file)]
    (if (fs/pathExistsSync f)
      (try (js->clj (js/JSON.parse (fs/readFileSync f "utf8")) :keywordize-keys true)
           (catch :default _ {:model (:id embedding-model) :people []}))
      {:model (:id embedding-model) :people []})))

(defn- write-profiles! [data]
  (fs/ensureDirSync (data-dir))
  (fs/writeFileSync (profiles-file) (js/JSON.stringify (clj->js data) nil 2)))

(defn- usable-people
  "Profiles made with the current embedding model (others are not comparable)."
  []
  (let [{:keys [model people]} (read-profiles)]
    (if (= model (:id embedding-model)) people [])))

(defn- cosine [a b]
  (let [dot (reduce + (map * a b))
        na (js/Math.sqrt (reduce + (map #(* % %) a)))
        nb (js/Math.sqrt (reduce + (map #(* % %) b)))]
    (if (or (zero? na) (zero? nb)) 0 (/ dot (* na nb)))))

(defn list-profiles []
  (mapv (fn [{:keys [name count updated]}] {:name name :count count :updated updated})
        (usable-people)))

(defn delete-profile! [name]
  (let [data (read-profiles)]
    (write-profiles! (update data :people (fn [ps] (vec (remove #(= name (:name %)) ps)))))
    true))

(defn match-profiles
  "For each embedding, the best matching saved person above the threshold, or nil."
  [embeddings]
  (let [people (usable-people)]
    (mapv (fn [e]
            (when (seq e)
              (let [best (->> people
                              (map (fn [p] {:name (:name p) :score (cosine e (:embedding p))}))
                              (sort-by :score >)
                              first)]
                (when (and best (>= (:score best) match-threshold)) best))))
          embeddings)))

(defn save-profile!
  "Add `embedding` to the profile of `name` (running mean, so profiles improve
  with every meeting)."
  [name embedding]
  (let [data (read-profiles)
        people (if (= (:model data) (:id embedding-model)) (:people data) [])
        existing (first (filter #(= name (:name %)) people))
        merged (if existing
                 (let [n (:count existing)]
                   (assoc existing
                          :embedding (mapv (fn [old new] (/ (+ (* old n) new) (inc n))) (:embedding existing) embedding)
                          :count (inc n)
                          :updated (.toISOString (js/Date.))))
                 {:name name :embedding (vec embedding) :count 1 :updated (.toISOString (js/Date.))})]
    (write-profiles! {:model (:id embedding-model)
                      :people (conj (vec (remove #(= name (:name %)) people)) merged)})
    true))

;; --- analysis cache (embeddings of one meeting, kept outside the graph) ----------------

(defn- cache-file [key]
  (data-dir "cache" (str (-> (crypto/createHash "sha1") (.update (str key)) (.digest "hex")) ".json")))

(defn cache-speakers! [key speakers]
  (fs/ensureDirSync (data-dir "cache"))
  (fs/writeFileSync (cache-file key) (js/JSON.stringify (clj->js speakers)))
  true)

(defn cached-embedding [key speaker-id]
  (let [f (cache-file key)]
    (when (fs/pathExistsSync f)
      (->> (js->clj (js/JSON.parse (fs/readFileSync f "utf8")) :keywordize-keys true)
           (filter #(= speaker-id (:id %)))
           first
           :embedding))))

;; --- status / models -----------------------------------------------------------------

(defn status []
  {:available (boolean (tool-path))
   :models-installed (models-installed?)
   :profiles (list-profiles)})

(defn download-models! []
  (fs/ensureDirSync (data-dir "models"))
  (let [fetch! (fn [{:keys [url] :as model}]
                 (let [dest (model-path model)
                       part (str dest ".part")]
                   (if (fs/pathExistsSync dest)
                     (p/resolved true)
                     (-> (whisper/download-file! url part (fn [_ _]) (:file model))
                         (p/then (fn [_] (fs/moveSync part dest) true))
                         (p/catch (fn [e] (fs/removeSync part) (throw e)))))))]
    (-> (p/let [_ (fetch! segmentation-model)
                _ (fetch! embedding-model)]
          true)
        (p/then (fn [_] {:ok true}))
        (p/catch (fn [e] {:ok false :error (str (.-message e))})))))

;; --- diarization ----------------------------------------------------------------------

(defn- temp-pcm [token]
  (node-path/join (os/tmpdir) (str "logseq-speakers-" (string/replace token #"[^a-zA-Z0-9-]" "") ".pcm")))

(defn append-pcm!
  "Append a chunk (base64, 16 kHz mono signed 16-bit PCM) to the temp audio for `token`."
  [token b64]
  (fs/appendFileSync (temp-pcm token) (js/Buffer.from b64 "base64"))
  true)

(defn- pcm->wav!
  "Prefix the raw PCM with a WAV header (streamed copy, the file can be large)."
  [pcm-file wav-file]
  (let [size (.-size (fs/statSync pcm-file))
        header (js/Buffer.alloc 44)]
    (.write header "RIFF" 0) (.writeUInt32LE header (+ 36 size) 4)
    (.write header "WAVEfmt " 8) (.writeUInt32LE header 16 16)
    (.writeUInt16LE header 1 20) (.writeUInt16LE header 1 22)
    (.writeUInt32LE header sample-rate 24) (.writeUInt32LE header (* 2 sample-rate) 28)
    (.writeUInt16LE header 2 32) (.writeUInt16LE header 16 34)
    (.write header "data" 36) (.writeUInt32LE header size 40)
    (let [out (node-fs/openSync wav-file "w")
          in (node-fs/openSync pcm-file "r")
          buf (js/Buffer.alloc (* 4 1024 1024))]
      (node-fs/writeSync out header)
      (loop []
        (let [n (node-fs/readSync in buf 0 (.-length buf) nil)]
          (when (pos? n)
            (node-fs/writeSync out buf 0 n)
            (recur))))
      (node-fs/closeSync in)
      (node-fs/closeSync out))))

(defn- run-tool! [args]
  (p/create
   (fn [resolve reject]
     (child-process/execFile
      (tool-path) (clj->js args)
      #js {:maxBuffer (* 64 1024 1024) :timeout (* 60 60 1000)}
      (fn [err stdout stderr]
        (if err
          (reject (js/Error. (str (.-message err) "\n" (subs (str stderr) 0 (min 500 (count (str stderr)))))))
          (resolve (js->clj (js/JSON.parse (str stdout)) :keywordize-keys true))))))))

(defn diarize!
  "Diarize the PCM uploaded under `token`. opts: {:num-speakers n :cache-key k}.
  Resolves {:ok true :segments [{:start :end :speaker}] :speakers [{:id :seconds :match}]}
  where :match is a recognized saved profile {:name :score} or nil."
  [token {:keys [num-speakers cache-key]}]
  (let [pcm (temp-pcm token)
        wav (str pcm ".wav")]
    (cond
      (not (tool-path))
      (p/resolved {:ok false :error "speaker-tool not found (run scripts/build-speaker-tools.sh)"})

      (not (models-installed?))
      (p/resolved {:ok false :error "speaker models are not installed"})

      (not (fs/pathExistsSync pcm))
      (p/resolved {:ok false :error "no audio uploaded"})

      :else
      (-> (p/let [_ (pcm->wav! pcm wav)
                  result (run-tool! (concat ["diarize" "--wav" wav
                                             "--segmentation" (model-path segmentation-model)
                                             "--embedding" (model-path embedding-model)
                                             "--threads" (str (max 1 (min 8 (.-length (os/cpus)))))]
                                            (when (and num-speakers (pos? num-speakers))
                                              ["--num-speakers" (str num-speakers)])))]
            result)
          (p/then (fn [{:keys [segments speakers]}]
                    (let [matches (match-profiles (map :embedding speakers))]
                      (when cache-key
                        (cache-speakers! cache-key (map #(select-keys % [:id :embedding]) speakers)))
                      {:ok true
                       :segments segments
                       :speakers (mapv (fn [s m] {:id (:id s) :seconds (:seconds s) :match m}) speakers matches)})))
          (p/catch (fn [e]
                     (logger/error "speaker diarization failed" e)
                     {:ok false :error (str (.-message e))}))
          (p/finally (fn []
                       (fs/removeSync pcm)
                       (fs/removeSync wav)))))))
