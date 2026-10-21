(ns frontend.util.meeting
  "Pure helpers for live meeting notes."
  (:require [clojure.string :as string]))

(defn audio-ref-for
  "Path (as written in the audio-timestamp macro) of the recording `file` of the
  meeting whose first recording is `audio-ref`. Segments without a file (meetings
  recorded before there could be several recordings) belong to `audio-ref` itself."
  [audio-ref file]
  (if (string/blank? file)
    audio-ref
    (string/replace-first audio-ref #"[^/]*$" file)))

(def ^:private min-speech-level 0.015) ; RMS floor for "speech"
(def ^:private max-speech-level 0.05)  ; ... and the most the noise floor may raise it to

(defn speech-threshold
  "RMS above which a frame counts as speech, given the running estimate of the
  background `noise`. It follows the noise (3.5x) so a fan does not read as speech,
  but never goes above `max-speech-level`: with a loud, steady noise (a generator, a
  crowd) the estimate keeps rising until it exceeds the voices themselves, and
  everything after that would be dropped as \"silence\". Past the cap the audio is
  simply cut into long chunks and whisper separates speech from noise itself."
  [noise]
  (min max-speech-level (max min-speech-level (* 3.5 noise))))

(def ^:private word-re (js/RegExp. "[\\p{L}\\p{N}]+" "gu"))

(def ^:private time-margin 1.5)     ; seconds: the two sources never line up exactly
(def ^:private min-coverage 0.5)    ; share of the mic segment inside system segments
(def ^:private min-containment 0.6) ; share of its words found in those system segments

(defn- words
  "Lower-case words without diacritics, so \"Septembre\" and \"SEPTEMBRE\" or ș/ş match."
  [text]
  (let [plain (-> (string/lower-case (or text ""))
                  (.normalize "NFD")
                  (string/replace (js/RegExp. "[\\u0300-\\u036f]" "g") ""))]
    (vec (array-seq (or (.match plain word-re) #js [])))))

(defn- same-word?
  "Equal, or one is the start of the other (\"noiembri\" / \"noiembrie\")."
  [a b]
  (or (= a b)
      (and (>= (count a) 4) (>= (count b) 4)
           (or (string/starts-with? a b) (string/starts-with? b a)))))

(defn- covered-seconds
  "Length of [start, end] covered by the union of `others`' time ranges."
  [start end others]
  (let [ranges (->> others
                    (map (fn [{s :start e :end}] [(max start s) (min end e)]))
                    (filter (fn [[s e]] (< s e)))
                    (sort-by first))]
    (:total
     (reduce (fn [{:keys [total reach]} [s e]]
               (let [s (max s reach)]
                 {:total (+ total (max 0 (- e s))) :reach (max reach e)}))
             {:total 0 :reach start}
             ranges))))

(defn leak?
  "True when mic `segment` only repeats what the system channel said at the same
  time, i.e. the headset microphone picked up the headphones. The system channel is
  the clean copy, so it always wins. `segments` are the already accepted segments."
  [{:keys [start end text]} segments]
  (let [system (filter #(and (= "system" (:source %))
                             (< (- start time-margin) (:end %))
                             (> (+ end time-margin) (:start %)))
                       segments)
        mine (words text)
        pool (mapcat (comp words :text) system)
        duration (- end start)]
    (boolean
     (and (seq mine) (seq pool) (pos? duration)
          (>= (/ (covered-seconds start end system) duration) min-coverage)
          (>= (/ (count (filter (fn [w] (some #(same-word? w %) pool)) mine))
                 (count mine))
              min-containment)))))
