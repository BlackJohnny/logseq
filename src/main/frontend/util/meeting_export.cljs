(ns frontend.util.meeting-export
  "Pure helpers to export a meeting as a page that plays the audio and follows the
  transcript (HTML), plus WebVTT subtitles. Only what is needed to read the meeting
  is exported: the voice profiles / embeddings never are."
  (:require [clojure.string :as string]))

(def data-marker "<!--MEETING-DATA-->")

(defn speaker-name [{:keys [id name]}]
  (if (string/blank? name) (str "Vorbitor " (inc id)) name))

(defn- recordings-of [transcript]
  (let [recs (vec (:recordings transcript))]
    (if (seq recs) recs [{:audio (or (:audio transcript) "audio.webm")}])))

(defn- rec-index
  "Index of the recording that holds `file` (nil = the first one)."
  [recs file]
  (or (first (keep-indexed (fn [i r] (when (= file (:audio r)) i)) recs)) 0))

(defn- mime-of [file]
  (condp #(string/ends-with? %2 %1) (string/lower-case (str file))
    ".webm" "audio/webm" ".ogg" "audio/ogg" ".m4a" "audio/mp4" ".mp3" "audio/mpeg" "audio/webm"))

(defn export-data
  "What the page needs, from transcript.json (`transcript`) and minutes.json
  (`minutes`, may be nil). `title` is the fallback when the transcript has none."
  [{:keys [transcript minutes title]}]
  (let [recs (recordings-of transcript)
        item (fn [m] (-> (select-keys m [:title :summary :text :owner :due :start])
                         (assoc :rec (rec-index recs (:audio m)))))]
    {:title (or (not-empty (:title transcript)) title "Meeting")
     :startedAt (:started-at transcript)
     :duration (or (:duration transcript) (some->> (keep :duration recs) seq (reduce +)))
     :recordings (vec (map-indexed (fn [i r] {:index i :file (:audio r) :duration (:duration r)
                                              :mime (mime-of (:audio r))})
                                   recs))
     :speakers (mapv (fn [s] {:id (:id s) :name (speaker-name s)}) (:speakers transcript))
     :segments (->> (:segments transcript)
                    (remove #(string/blank? (:text %)))
                    (map (fn [s] {:start (:start s) :end (:end s) :text (string/trim (:text s))
                                  :speaker (:speaker s) :rec (rec-index recs (:audio s))}))
                    (sort-by (juxt :rec :start))
                    vec)
     :minutes (when minutes
                (-> (select-keys minutes [:language :summary])
                    (into (map (fn [k] [k (mapv item (get minutes k))])) [:topics :decisions :actions :questions])))}))

(defn- json-script [id content type]
  (str "<script type=\"" type "\" id=\"" id "\">" content "</script>"))

(defn render-html
  "The page. `audio` is {recording-index base64} to embed, or {} to link
  (`:src` of each recording) / omit the audio."
  [template data audio]
  (let [json (-> (js/JSON.stringify (clj->js data))
                 (string/replace "<" "\\u003c")) ; cannot close the script tag
        content (str (json-script "data" json "application/json")
                     (apply str (for [[i b64] (sort audio)] (json-script (str "audio-" i) b64 "text/plain"))))
        at (string/index-of template data-marker)]
    (str (subs template 0 at) content (subs template (+ at (count data-marker))))))

;; --- WebVTT -----------------------------------------------------------------------------

(defn vtt-time [seconds]
  (let [ms (js/Math.round (* 1000 (max 0 seconds)))
        two #(if (< % 10) (str "0" %) (str %))
        three #(cond (< % 10) (str "00" %) (< % 100) (str "0" %) :else (str %))]
    (str (two (quot ms 3600000)) ":" (two (mod (quot ms 60000) 60)) ":"
         (two (mod (quot ms 1000) 60)) "." (three (mod ms 1000)))))

(defn- vtt-escape [s]
  (-> (str s) (string/replace "&" "&amp;") (string/replace "<" "&lt;") (string/replace ">" "&gt;")))

(defn vtt
  "WebVTT subtitles of recording `rec` (an index into :recordings)."
  [{:keys [segments speakers]} rec]
  (let [names (into {} (map (juxt :id :name)) speakers)]
    (str "WEBVTT\n\n"
         (apply str
                (for [{:keys [start end text speaker] :as seg} segments
                      :when (= rec (:rec seg))]
                  (str (vtt-time start) " --> " (vtt-time (if (and end (> end start)) end (+ start 3))) "\n"
                       (when-let [n (and (some? speaker) (get names speaker))]
                         (str "<v " (string/replace (vtt-escape n) "\n" " ") ">"))
                       (vtt-escape text) "\n\n"))))))
