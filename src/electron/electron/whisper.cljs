(ns electron.whisper
  "Local speech-to-text (dictation) via whisper.cpp.

  Two prebuilt binaries are used, see scripts/build-whisper.sh:
  `whisper-cli-cuda` (NVIDIA GPU, preferred when usable) and `whisper-cli-cpu`.
  Models are downloaded on demand into <userData>/whisper/models."
  (:require ["child_process" :as child-process]
            ["electron" :refer [app]]
            ["fs-extra" :as fs]
            ["https" :as https]
            ["os" :as os]
            ["path" :as node-path]
            [clojure.string :as string]
            [electron.logger :as logger]
            [promesa.core :as p]))

(def models
  "Models from https://huggingface.co/ggerganov/whisper.cpp (quantized, q5_0)."
  {"large-v3-turbo-q5_0" {:file "ggml-large-v3-turbo-q5_0.bin" :size-mb 548}
   "medium-q5_0"         {:file "ggml-medium-q5_0.bin" :size-mb 515}
   "small-q5_1"          {:file "ggml-small-q5_1.bin" :size-mb 181}
   "base-q5_1"           {:file "ggml-base-q5_1.bin" :size-mb 57}})

(def default-model "large-v3-turbo-q5_0")

(def ^:private model-url-prefix "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/")

(defonce ^:private *gpu-broken? (atom false))
(defonce ^:private *downloads (atom {})) ; model id -> http request

;; --- paths -----------------------------------------------------------------

(defn- models-dir []
  (node-path/join (.getPath ^js app "userData") "whisper" "models"))

(defn- model-path [model]
  (when-let [{:keys [file]} (get models model)]
    (node-path/join (models-dir) file)))

(defn- bin-dir
  "First existing directory holding the whisper binaries: an env override, the
  packaged resources dir, or the repo's resources/ dir (dev)."
  []
  (->> [js/process.env.LOGSEQ_WHISPER_BIN_DIR
        (node-path/join (or js/process.resourcesPath "") "whisper" "bin")
        (node-path/join (.getAppPath ^js app) ".." ".whisper-build" "dist" "whisper" "bin")]
       (filter seq)
       (filter #(fs/pathExistsSync %))
       first))

(defn- binary [backend]
  (when-let [dir (bin-dir)]
    (let [f (node-path/join dir (str "whisper-cli-" (name backend)))]
      (when (fs/pathExistsSync f) f))))

;; --- backend detection -------------------------------------------------------

(defn- nvidia-free-vram-mb
  "Free VRAM of the first NVIDIA GPU in MiB, or nil when there is no usable one."
  []
  (p/create
   (fn [resolve _]
     (child-process/execFile
      "nvidia-smi" #js ["--query-gpu=memory.free" "--format=csv,noheader,nounits"]
      #js {:timeout 5000}
      (fn [err stdout _]
        (resolve (when-not err
                   (let [n (js/parseInt (first (string/split-lines (str stdout))) 10)]
                     (when-not (js/isNaN n) n)))))))))

(defn- pick-backend
  "`pref` is \"auto\" | \"cpu\" | \"gpu\". Returns :cuda or :cpu."
  [pref]
  (p/let [vram (when (and (not= pref "cpu") (binary :cuda) (not @*gpu-broken?))
                 (nvidia-free-vram-mb))]
    ;; ~1 GiB covers the largest default model (large-v3-turbo q5_0) plus buffers.
    (if (and vram (>= vram 1024)) :cuda :cpu)))

(defn status
  "Everything the UI needs to render the dictation settings/state."
  [pref]
  (p/let [backend (pick-backend (or pref "auto"))]
    {:available (boolean (binary :cpu))
     :backend (name backend)
     :cuda-binary (boolean (binary :cuda))
     :default-model (if (= backend :cuda) default-model "small-q5_1")
     :models (into {}
                   (map (fn [[id {:keys [size-mb]}]]
                          [id {:size-mb size-mb
                               :installed (fs/pathExistsSync (model-path id))}]))
                   models)}))

;; --- model download ----------------------------------------------------------

(defn- send-progress! [^js win data]
  (when-not (.isDestroyed win)
    (.send (.-webContents win) "whisper-download-progress" (clj->js data))))

(defn- download-file!
  "GET `url` (following redirects) into `dest`, reporting progress. Resolves on success."
  [url dest on-progress model]
  (p/create
   (fn [resolve reject]
     (let [get! (fn get! [url redirects]
                  (let [req (.get https url
                                  (fn [^js res]
                                    (let [code (.-statusCode res)]
                                      (cond
                                        (and (<= 300 code 399) (.. res -headers -location))
                                        (do (.resume res)
                                            (if (< redirects 5)
                                              (get! (.. res -headers -location) (inc redirects))
                                              (reject (js/Error. "too many redirects"))))

                                        (not= 200 code)
                                        (do (.resume res)
                                            (reject (js/Error. (str "download failed: HTTP " code))))

                                        :else
                                        (let [total (js/parseInt (.. res -headers -content-length) 10)
                                              out (fs/createWriteStream dest)
                                              received (atom 0)]
                                          (.on res "data"
                                               (fn [chunk]
                                                 (swap! received + (.-length chunk))
                                                 (on-progress @received total)))
                                          (.on res "error" reject)
                                          (.on out "error" reject)
                                          (.on out "finish" #(if (and (not (js/isNaN total)) (not= total @received))
                                                               (reject (js/Error. "download incomplete"))
                                                               (resolve true)))
                                          (.pipe res out))))))]
                    (swap! *downloads assoc model req)
                    (.on req "error" reject)))]
       (get! url 0)))))

(defn download-model!
  [^js win model]
  (if-let [{:keys [file]} (get models model)]
    (let [dest (model-path model)
          part (str dest ".part")]
      (cond
        (fs/pathExistsSync dest) (p/resolved {:ok true})
        (contains? @*downloads model) (p/resolved {:ok false :error "already downloading"})
        :else
        (do
          (fs/ensureDirSync (models-dir))
          (-> (download-file! (str model-url-prefix file) part
                              (fn [received total]
                                (send-progress! win {:model model :received received :total total}))
                              model)
              (p/then (fn [_] (fs/moveSync part dest) {:ok true}))
              (p/catch (fn [e]
                         (fs/removeSync part)
                         (logger/error "whisper model download failed" e)
                         {:ok false :error (str (.-message e))}))
              (p/finally #(swap! *downloads dissoc model))))))
    (p/resolved {:ok false :error (str "unknown model " model)})))

(defn cancel-download! [model]
  (when-let [^js req (get @*downloads model)]
    (.destroy req)))

;; --- transcription -----------------------------------------------------------

(defn- run-whisper!
  [bin model-file wav-file language]
  (p/create
   (fn [resolve reject]
     (child-process/execFile
      bin
      #js ["-m" model-file "-f" wav-file "-l" (or language "auto")
           "-t" (str (max 1 (min 8 (.-length (os/cpus)))))
           "-nt"                        ; no timestamps
           "-np"]                       ; only print the transcript
      #js {:timeout 600000 :maxBuffer (* 16 1024 1024)}
      (fn [err stdout stderr]
        (if err
          (reject (js/Error. (str (.-message err) "\n" (some-> stderr str (subs 0 (min 500 (count stderr)))))))
          (resolve (-> (str stdout) string/split-lines
                       (->> (map string/trim) (remove string/blank?))
                       (->> (string/join " "))))))))))

(defn transcribe!
  "`audio-b64` is a 16 kHz mono 16-bit PCM WAV, base64 encoded.
  opts: {:model :language :backend}. Resolves to {:ok true :text .. :backend ..}
  or {:ok false :error ..}. A failing CUDA run falls back to the CPU binary."
  [audio-b64 {:keys [model language backend]}]
  (let [model (or model default-model)
        model-file (model-path model)
        wav (node-path/join (os/tmpdir) (str "logseq-dictation-" (js/Date.now) ".wav"))]
    (cond
      (not (binary :cpu))
      (p/resolved {:ok false :error "whisper binaries not found (run scripts/build-whisper.sh)"})

      (or (nil? model-file) (not (fs/pathExistsSync model-file)))
      (p/resolved {:ok false :error (str "model not installed: " model)})

      :else
      (do
        (fs/writeFileSync wav (js/Buffer.from audio-b64 "base64"))
        (-> (p/let [chosen (pick-backend (or backend "auto"))
                    text (if (= chosen :cuda)
                           (-> (run-whisper! (binary :cuda) model-file wav language)
                               (p/catch (fn [e]
                                          (logger/error "whisper CUDA run failed, falling back to CPU" e)
                                          (reset! *gpu-broken? true)
                                          (run-whisper! (binary :cpu) model-file wav language))))
                           (run-whisper! (binary :cpu) model-file wav language))]
            {:ok true :text text :backend (name (if @*gpu-broken? :cpu chosen))})
            (p/catch (fn [e] {:ok false :error (str (.-message e))}))
            (p/finally #(fs/removeSync wav)))))))
