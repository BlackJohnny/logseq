(ns electron.audio-import
  "Offline meetings: turn any audio/video file the user picks into what a live
  meeting produces, using the system's `ffmpeg`. One decode gives (1) the Opus
  recording that goes into the graph and (2) a temporary 16 kHz mono PCM file the
  renderer reads in chunks for voice-activity detection and transcription."
  (:require ["child_process" :as child-process]
            ["electron" :refer [BrowserWindow dialog]]
            ["fs" :as node-fs]
            ["fs-extra" :as fs]
            ["os" :as os]
            ["path" :as node-path]
            [clojure.string :as string]
            [promesa.core :as p]))

(def ^:private extensions
  ["m4a" "mp3" "wav" "ogg" "oga" "opus" "flac" "aac" "wma" "aiff" "amr" "webm" "mp4" "mkv" "mov" "m4v"])

(defn- pcm-file [token]
  (node-path/join (os/tmpdir) (str "logseq-import-" (string/replace token #"[^a-zA-Z0-9-]" "") ".pcm")))

(defn pick-file!
  "Ask for a file. Resolves {:path :name :mtime-ms} or nil when cancelled."
  []
  (p/let [win (.getFocusedWindow BrowserWindow)
          ^js result (.showOpenDialog dialog win
                                  (clj->js {:properties ["openFile"]
                                            :filters [{:name "Audio / video" :extensions extensions}
                                                      {:name "All files" :extensions ["*"]}]}))
          path (first (js->clj (.-filePaths result)))]
    (when (and path (not (.-canceled result)))
      {:path path
       :name (node-path/basename path)
       :mtime-ms (.-mtimeMs (fs/statSync path))})))

(defn- run-ffmpeg! [args]
  (p/create
   (fn [resolve reject]
     (child-process/execFile
      "ffmpeg" (clj->js args)
      #js {:maxBuffer (* 16 1024 1024) :timeout (* 4 60 60 1000)}
      (fn [err _stdout stderr]
        (cond
          (and err (= "ENOENT" (.-code err)))
          (reject (js/Error. "ffmpeg is not installed. Install it (e.g. `sudo apt install ffmpeg`, `brew install ffmpeg`) and try again."))

          err
          (let [lines (->> (string/split-lines (str stderr)) (remove string/blank?) (take-last 3))]
            (reject (js/Error. (str "ffmpeg failed: " (string/join " " lines)))))

          :else (resolve true)))))))

(defn convert!
  "Decode `src` (any format ffmpeg reads; the first audio stream) into `dest` (Opus in
  WebM) and a temporary 16 kHz PCM file. Resolves {:ok true :token :seconds} or
  {:ok false :error}."
  [src dest]
  (let [token (str (random-uuid))
        pcm (pcm-file token)]
    (fs/ensureDirSync (node-path/dirname dest))
    (-> (p/let [_ (run-ffmpeg! ["-y" "-v" "error" "-i" src
                                "-vn" "-map" "0:a:0" "-ac" "1" "-c:a" "libopus" "-b:a" "32k" "-f" "webm" dest
                                "-vn" "-map" "0:a:0" "-ac" "1" "-ar" "16000" "-f" "s16le" pcm])]
          {:ok true :token token :seconds (/ (.-size (fs/statSync pcm)) 32000)})
        (p/catch (fn [e]
                   (fs/removeSync pcm)
                   (fs/removeSync dest)
                   {:ok false :error (str (.-message e))})))))

(defn read-pcm
  "`len` bytes of the converted audio from byte `offset`, base64 (empty at the end)."
  [token offset len]
  (let [fd (node-fs/openSync (pcm-file token) "r")
        buf (js/Buffer.alloc len)]
    (try
      (let [n (node-fs/readSync fd buf 0 len offset)]
        (.toString (.subarray buf 0 n) "base64"))
      (finally (node-fs/closeSync fd)))))

(defn cleanup! [token]
  (fs/removeSync (pcm-file token))
  true)
