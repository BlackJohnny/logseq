(ns frontend.util.minutes
  "Pure logic for AI-written meeting minutes: splitting a transcript into parts,
  building the prompts, reading the model's JSON, and turning the result into a
  block tree. The model only *cites* transcript segment numbers; timestamps are
  computed here from those citations, never taken from the model."
  (:require [clojure.string :as string]))

;; --- transcript -> text ---------------------------------------------------------

(defn segment-line
  "`[12] Ionut: text` (the speaker part is left out when unknown)."
  [{:keys [id speaker-name text]}]
  (str "[" id "] " (when speaker-name (str speaker-name ": "))
       (string/replace (string/trim (str text)) #"\s*\n\s*" " ")))

(defn chunk-segments
  "Group consecutive segments into parts of at most `max-chars` characters of
  transcript text (a single longer segment gets its own part)."
  [segments max-chars]
  (->> segments
       (reduce (fn [{:keys [parts size] :as acc} seg]
                 (let [n (inc (count (segment-line seg)))]
                   (if (and (seq (peek parts)) (> (+ size n) max-chars))
                     {:parts (conj parts [seg]) :size n}
                     (-> acc
                         (update :parts (fn [ps] (if (seq ps) (conj (pop ps) (conj (peek ps) seg)) [[seg]])))
                         (update :size + n)))))
               {:parts [] :size 0})
       :parts))

(defn transcript-text [segments]
  (string/join "\n" (map segment-line segments)))

;; --- prompts ----------------------------------------------------------------------

(def ^:private system-prompt
  (str "You write minutes of meetings from automatic speech-recognition transcripts. "
       "The transcript can contain recognition mistakes, filler words and fragments. "
       "Use ONLY what is stated in the transcript: never invent facts, names, dates, "
       "numbers or decisions, and leave a field empty when the transcript does not say. "
       "Write in the same language as the transcript. "
       "Reply with one JSON object and nothing else."))

(def ^:private item-shape
  (str "\"topics\": [{\"title\": \"short title\", \"summary\": \"1-3 sentences\", \"refs\": [segment numbers]}], "
       "\"decisions\": [{\"text\": \"what was decided\", \"refs\": [segment numbers]}], "
       "\"actions\": [{\"text\": \"what must be done\", \"owner\": \"person or null\", \"due\": \"deadline as stated or null\", \"refs\": [segment numbers]}], "
       "\"questions\": [{\"text\": \"unresolved question\", \"refs\": [segment numbers]}]"))

(def ^:private rules
  (str "`refs` lists the numbers (the [n] at the start of each line) of the transcript segments that "
       "support the item, at most 6, only numbers that appear in the transcript. "
       "`owner` is a person named in the transcript, otherwise null. "
       "Do not repeat the same point in several sections."))

(defn part-messages
  "Prompt for one part of a long transcript (notes only, no overall summary)."
  [segments part-number part-count]
  [{:role "system" :content system-prompt}
   {:role "user"
    :content (str "This is part " part-number " of " part-count " of one meeting transcript. "
                  "Each line is `[segment number] Speaker: text`.\n\n"
                  (transcript-text segments)
                  "\n\nExtract the notes of this part as JSON of this shape: {" item-shape "}. "
                  rules " Include only items that are clearly present in this part.")}])

(defn final-messages
  "Prompt for a transcript that fits in one part (notes plus overall summary)."
  [segments]
  [{:role "system" :content system-prompt}
   {:role "user"
    :content (str "Each line of this meeting transcript is `[segment number] Speaker: text`.\n\n"
                  (transcript-text segments)
                  "\n\nWrite the minutes as JSON of this shape: {\"language\": \"ISO 639-1 code of the transcript language\", "
                  "\"summary\": \"3-5 sentences on what the meeting was about and its outcome\", " item-shape "}. "
                  rules)}])

(defn merge-messages
  "Prompt that merges the notes of consecutive parts into the minutes of the whole meeting."
  [part-notes]
  [{:role "system" :content system-prompt}
   {:role "user"
    :content (str "Below are notes extracted, in order, from consecutive parts of ONE meeting, as JSON.\n\n"
                  (string/join "\n" (map-indexed (fn [i notes] (str "Part " (inc i) ": " notes)) part-notes))
                  "\n\nMerge them into the minutes of the whole meeting as JSON of this shape: "
                  "{\"language\": \"ISO 639-1 code of the transcript language\", "
                  "\"summary\": \"3-5 sentences on what the whole meeting was about and its outcome\", " item-shape "}. "
                  "Merge topics that continue across parts and drop duplicates; keep every decision and action item; "
                  "keep the segment numbers in `refs` exactly as given. "
                  rules)}])

;; --- reading the model's answer ------------------------------------------------------

(defn strip-reasoning
  "Remove <think>...</think> blocks emitted by reasoning models."
  [s]
  (string/replace (str s) #"(?s)<think>.*?</think>" ""))

(defn extract-json
  "Parse the JSON object in a model answer (it may be wrapped in text or a code
  fence). Returns a keywordized map, or nil."
  [s]
  (let [s (strip-reasoning s)
        start (string/index-of s "{")
        end (string/last-index-of s "}")]
    (when (and start end (> end start))
      (try (js->clj (js/JSON.parse (subs s start (inc end))) :keywordize-keys true)
           (catch :default _ nil)))))

(defn- ->text [v] (some-> v str string/trim not-empty))

(defn- clean-refs [refs valid-ids]
  (->> (if (sequential? refs) refs [])
       (keep (fn [r] (let [n (if (number? r) r (js/parseInt (str r) 10))]
                       (when (and (number? n) (not (js/isNaN n)) (contains? valid-ids n)) (int n)))))
       distinct sort vec))

(defn- start-of [refs starts]
  (when (seq refs) (apply min (map starts refs))))

(defn normalize
  "Validate the model's JSON against the transcript `segments`: drop empty items and
  citations of segments that do not exist, and compute each item's :start (seconds)
  from the segments it cites."
  [data segments]
  (let [valid (set (map :id segments))
        starts (into {} (map (juxt :id :start)) segments)
        items (fn [k text-key]
                (->> (get data k)
                     (keep (fn [item]
                             (let [item (if (string? item) {text-key item} item)
                                   text (->text (get item text-key))
                                   refs (clean-refs (:refs item) valid)]
                               (when text
                                 (assoc (select-keys item [:summary :owner :due])
                                        text-key text
                                        :refs refs
                                        :start (start-of refs starts))))))
                     vec))]
    {:language (or (->text (:language data)) "en")
     :summary (->text (:summary data))
     :topics (items :topics :title)
     :decisions (items :decisions :text)
     :actions (->> (items :actions :text)
                   (mapv #(-> % (update :owner ->text) (update :due ->text))))
     :questions (items :questions :text)}))

(defn empty-minutes? [{:keys [summary topics decisions actions questions]}]
  (and (nil? summary) (empty? topics) (empty? decisions) (empty? actions) (empty? questions)))

;; --- block tree ------------------------------------------------------------------------

(def ^:private labels
  {"ro" {:minutes "Minută" :summary "Rezumat" :topics "Subiecte" :decisions "Decizii"
         :actions "Acțiuni" :questions "Întrebări deschise"}
   "en" {:minutes "Minutes" :summary "Summary" :topics "Topics" :decisions "Decisions"
         :actions "Action items" :questions "Open questions"}})

(defn audio-macro [audio-ref seconds]
  (str "{{audio-timestamp " audio-ref ", " (js/Math.floor seconds) "}}"))

(defn blocks-tree
  "Minutes as a tree of {:content :children}. Items with citations get a play button;
  action items become TODO blocks; owners are linked only when they are in
  `known-names` (the named speakers), so the model cannot create arbitrary pages."
  [{:keys [language summary topics decisions actions questions]} {:keys [audio-ref known-names]}]
  (let [l (get labels language (get labels "en"))
        play (fn [{:keys [start]}] (when start (str (audio-macro audio-ref start) " ")))
        section (fn [k children] (when (seq children) {:content (str "**" (get l k) "**") :children children}))
        owner (fn [o] (when o (if (contains? (set known-names) o) (str "[[" o "]]: ") (str o ": "))))]
    {:content (str "## " (:minutes l))
     :children
     (vec (remove nil?
                  [(section :summary (when summary [{:content summary}]))
                   (section :topics (mapv (fn [t] {:content (str (play t) "**" (:title t) "**")
                                                   :children (when (:summary t) [{:content (:summary t)}])})
                                          topics))
                   (section :decisions (mapv (fn [d] {:content (str (play d) (:text d))}) decisions))
                   (section :actions (mapv (fn [a] {:content (str "TODO " (play a) (owner (:owner a)) (:text a)
                                                                  (when (:due a) (str " (" (:due a) ")")))})
                                           actions))
                   (section :questions (mapv (fn [q] {:content (str (play q) (:text q))}) questions))]))}))
