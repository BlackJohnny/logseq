(ns frontend.util.speakers
  "Pure helpers to correct a meeting's speaker labels from examples the user pinned
  (\"this text was said by speaker A\"), by comparing voice embeddings."
  (:require [clojure.string :as string]))

(defn- squash [s] (string/trim (string/replace (str s) #"\s+" " ")))

(defn ours?
  "True when `content` is a block this feature wrote for an utterance saying `text`:
  the audio-timestamp macro, then optionally a speaker label (\"**Vorbitor 2:**\" or
  \"[[Name]]:\"), then the text. Whitespace differences are ignored, and so is whatever
  label is there now, so a block is recognized whoever it was last labeled as."
  [content text]
  (if-let [[_ body] (re-matches #"(?s)\s*\{\{audio-timestamp[^}]*\}\}(.*)" (str content))]
    (let [body (-> (string/triml body)
                   (string/replace-first #"^(?:\*\*Vorbitor \d+:\*\*|\[\[[^\]]+\]\]:)" ""))]
      (= (squash body) (squash text)))
    false))

(def ^:private min-score 0.4)   ; a segment must resemble the new speaker at least this much
(def ^:private min-margin 0.05) ; ... and beat its current speaker by this much
(def ^:private min-score-unassigned 0.65) ; an utterance with no speaker yet needs a clear match

(defn- norm [v] (js/Math.sqrt (reduce + (map #(* % %) v))))

(defn- unit [v]
  (let [n (norm v)] (if (zero? n) v (mapv #(/ % n) v))))

(defn- cosine [a b]
  (let [na (norm a) nb (norm b)]
    (if (or (zero? na) (zero? nb)) 0 (/ (reduce + (map * a b)) (* na nb)))))

(defn- centroid
  "Normalized mean of the normalized `embeddings`."
  [embeddings]
  (let [us (map unit embeddings)
        n (count us)]
    (unit (apply mapv (fn [& xs] (/ (reduce + xs) n)) us))))

(defn centroids
  "{speaker-id -> embedding}. A speaker with pinned examples is represented by their
  mean; the others by their embedding from the diarization (`speaker-embeddings`,
  {id -> embedding}). `segment-embeddings` is parallel to `segments`."
  [segments segment-embeddings pinned-speakers speaker-embeddings]
  (let [examples (reduce (fn [acc [seg e pinned]]
                           (if (and pinned e) (update acc pinned (fnil conj []) e) acc))
                         {} (map vector segments segment-embeddings pinned-speakers))]
    (merge (into {} (keep (fn [[id e]] (when (seq e) [id (unit e)])) speaker-embeddings))
           (update-vals examples centroid))))

(defn reassign
  "New speaker per segment (a vector parallel to `segments`). `pinned-speakers` is
  parallel too: the speaker the user fixed for a segment, or nil. Pinned segments
  keep that speaker; the others move to the closest speaker only when it clearly
  resembles the segment more than the current one. Segments without an embedding
  (too short) keep their speaker."
  [segments segment-embeddings pinned-speakers speaker-embeddings]
  (let [cs (centroids segments segment-embeddings pinned-speakers speaker-embeddings)]
    (mapv (fn [seg e pinned]
            (cond
              pinned pinned
              (empty? e) (:speaker seg)
              :else
              (let [scores (update-vals cs #(cosine e %))
                    [best best-score] (when (seq scores) (apply max-key val scores))
                    current (:speaker seg)
                    current-score (get scores current 0)]
                (if (and best
                         (not= best current)
                         (>= best-score (if (nil? current) min-score-unassigned min-score))
                         (>= (- best-score current-score) min-margin))
                  best
                  current))))
          segments segment-embeddings pinned-speakers)))
