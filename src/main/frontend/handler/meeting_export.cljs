(ns frontend.handler.meeting-export
  "Export a meeting page as something another person can open without Logseq:
  a single HTML file (audio embedded) that plays the recording and follows the
  transcript, or a zip with that page plus the audio, the transcript (JSON, WebVTT)
  and the minutes. Voice profiles are never exported."
  (:require ["jszip" :as JSZip]
            [clojure.string :as string]
            [electron.ipc :as ipc]
            [frontend.handler.dictation :as dictation]
            [frontend.handler.notification :as notification]
            [frontend.handler.speakers :as speakers]
            [frontend.util.meeting-export :as export]
            [promesa.core :as p]
            [shadow.resource :as rc]))

(def ^:private template (rc/inline "templates/meeting-export.html"))

(defonce *busy? (atom false))

(defn- read-json [rel]
  (p/let [text (ipc/ipc "meeting/read-text" (speakers/repo-dir) rel)]
    (when (seq text)
      (try (js->clj (js/JSON.parse text) :keywordize-keys true) (catch :default _ nil)))))

(defn- dir-of [rel] (string/replace rel #"/[^/]*$" ""))

(defn- slug [s]
  (let [s (-> (str s) string/trim (string/replace #"[\\/:*?\"<>|]+" "-") (string/replace #"\s+" " "))]
    (if (string/blank? s) "meeting" s)))

(defn- read-audio
  "{recording-index base64} of the recordings' audio files."
  [dir recordings]
  (p/let [pairs (p/all (for [{:keys [index file]} recordings]
                         (p/let [b64 (ipc/ipc "meeting/read-file" (speakers/repo-dir) (str dir "/" file))]
                           [index b64])))]
    (into {} pairs)))

(defn- build-html [data audio-b64]
  (export/render-html template data audio-b64))

(defn- build-zip
  "Base64 of the zip: index.html (linking the audio files), transcript and minutes."
  [folder data audio-b64 minutes]
  (let [zip (JSZip.)
        root (.folder zip folder)
        data (update data :recordings (fn [recs] (mapv #(assoc % :src (when (seq audio-b64) (:file %))) recs)))]
    (.file root "index.html" (build-html data {}))
    (doseq [[i b64] audio-b64]
      (.file root (:file (nth (:recordings data) i)) b64 #js {:base64 true :compression "STORE"}))
    (.file root "transcript.json" (js/JSON.stringify (clj->js (dissoc data :minutes)) nil 2))
    (doseq [{:keys [index file]} (:recordings data)]
      (.file root (str "transcript-" (string/replace (str file) #"\.[^.]+$" "") ".vtt") (export/vtt data index)))
    (when minutes
      (.file root "minutes.json" (js/JSON.stringify (clj->js (:minutes data)) nil 2)))
    (.generateAsync zip #js {:type "base64" :compression "DEFLATE"})))

(defn export!
  "`opts`: {:format :html | :zip, :audio? bool, :minutes? bool}."
  [{:keys [page-name transcript-rel] :as _info} {:keys [format audio? minutes?]}]
  (when-not @*busy?
    (reset! *busy? true)
    (let [name (slug (last (string/split page-name #"/")))
          [ext label] (if (= format :zip) ["zip" "Zip archive"] ["html" "HTML page"])]
      (-> (p/let [path (dictation/<ipc "meeting-export/pick-save" (str name "." ext) label ext)]
            (when path
              (notification/show! "Exporting the meeting..." :info)
              (p/let [transcript (read-json transcript-rel)
                      _ (when-not transcript (throw (js/Error. "this meeting has no transcript.json")))
                      minutes (when minutes? (read-json (str (dir-of transcript-rel) "/minutes.json")))
                      data (export/export-data {:transcript transcript :minutes minutes :title name})
                      audio-b64 (if audio? (read-audio (dir-of transcript-rel) (:recordings data)) {})
                      zip? (= format :zip)
                      out (if zip?
                            (build-zip name data audio-b64 minutes)
                            (build-html data audio-b64))
                      _ (ipc/ipc "meeting-export/write-file" path out (if zip? "base64" "utf8"))]
                (notification/show! (str "Meeting exported: " path) :success))))
          (p/catch (fn [e] (notification/show! (str "Cannot export the meeting: " (or (.-message e) e)) :error)))
          (p/finally #(reset! *busy? false))))))
