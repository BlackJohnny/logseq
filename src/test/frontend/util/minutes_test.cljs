(ns frontend.util.minutes-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as string]
            [frontend.util.minutes :as minutes]))

(def segs
  [{:id 0 :start 1.5 :end 5 :speaker-name "Ana" :text "Începem ședința."}
   {:id 1 :start 10.2 :end 14 :speaker-name "Paul" :text "Livrăm vineri\nversiunea nouă."}
   {:id 2 :start 61 :end 65 :text "Cine rezolvă testele?"}])

(deftest segment-line-test
  (is (= "[0] Ana: Începem ședința." (minutes/segment-line (first segs))))
  (is (= "[1] Paul: Livrăm vineri versiunea nouă." (minutes/segment-line (second segs))) "newlines collapse")
  (is (= "[2] Cine rezolvă testele?" (minutes/segment-line (nth segs 2))) "no speaker, no prefix"))

(deftest chunk-segments-test
  (testing "everything fits in one part"
    (is (= [segs] (minutes/chunk-segments segs 10000))))
  (testing "parts respect the size limit and keep order"
    (let [parts (minutes/chunk-segments segs 60)]
      (is (< 1 (count parts)))
      (is (= segs (vec (apply concat parts))))
      (is (every? seq parts))))
  (testing "a segment longer than the limit still gets a part"
    (is (= [[(first segs)]] (minutes/chunk-segments [(first segs)] 5))))
  (is (= [] (minutes/chunk-segments [] 100))))

(deftest extract-json-test
  (is (= {:a 1} (minutes/extract-json "{\"a\": 1}")))
  (is (= {:a 1} (minutes/extract-json "Here you go:\n```json\n{\"a\": 1}\n```\nDone.")))
  (is (= {:a 1} (minutes/extract-json "<think>{\"x\": 2} hmm</think>{\"a\": 1}")) "reasoning is ignored")
  (is (nil? (minutes/extract-json "no json here")))
  (is (nil? (minutes/extract-json "{broken"))))

(deftest normalize-test
  (let [data {:language "ro"
              :summary "  Rezumat  "
              :topics [{:title "Lansare" :summary "Se lansează." :refs [1 2]}
                       {:title "  " :refs [0]}]
              :decisions [{:text "Livrăm vineri" :refs [1 99 "0"]}]
              :actions [{:text "Rezolvă testele" :owner " Paul " :due nil :refs [2]}
                        "Ceva fără structură"]
              :questions []}
        n (minutes/normalize data segs)]
    (testing "empty titles are dropped"
      (is (= ["Lansare"] (map :title (:topics n)))))
    (testing "the start comes from the cited segments, not from the model"
      (is (= 10.2 (:start (first (:topics n)))))
      (is (= 61 (:start (first (:actions n))))))
    (testing "unknown segment numbers are dropped, string numbers are accepted"
      (is (= [0 1] (:refs (first (:decisions n)))))
      (is (= 1.5 (:start (first (:decisions n))))))
    (testing "items without citations have no start"
      (is (nil? (:start (second (:actions n)))))
      (is (= "Ceva fără structură" (:text (second (:actions n))))))
    (is (= "Paul" (:owner (first (:actions n)))))
    (is (= "Rezumat" (:summary n)))
    (is (= "ro" (:language n)))))

(deftest normalize-garbage-test
  (let [n (minutes/normalize {} segs)]
    (is (minutes/empty-minutes? n))
    (is (= "en" (:language n)))))

(deftest blocks-tree-test
  (let [n (minutes/normalize {:language "ro" :summary "S"
                              :topics [{:title "T" :summary "ts" :refs [1]}]
                              :decisions [{:text "D" :refs [0]}]
                              :actions [{:text "A" :owner "Paul" :due "vineri" :refs [2]}
                                        {:text "B" :owner "Necunoscut"}]
                              :questions [{:text "Q"}]}
                             segs)
        tree (minutes/blocks-tree n {:audio-ref "../assets/m/audio.webm" :known-names ["Paul" "Ana"]})
        contents (fn contents [t] (cons (:content t) (mapcat contents (:children t))))
        all (contents tree)]
    (is (= "## Minută" (:content tree)))
    (testing "sections in order, in the meeting language"
      (is (= ["**Rezumat**" "**Subiecte**" "**Decizii**" "**Acțiuni**" "**Întrebări deschise**"]
             (map :content (:children tree)))))
    (testing "play buttons carry the computed time"
      (is (some #(string/includes? % "{{audio-timestamp ../assets/m/audio.webm, 10}} **T**") all))
      (is (some #(string/includes? % "{{audio-timestamp ../assets/m/audio.webm, 61}}") all)))
    (testing "actions are TODO blocks, only known people are linked"
      (is (some #(string/starts-with? % "TODO {{audio-timestamp ../assets/m/audio.webm, 61}} [[Paul]]: A (vineri)") all))
      (is (some #(= "TODO Necunoscut: B" %) all)))
    (testing "an item without citation has no button"
      (is (some #(= "Q" %) all)))))

(deftest blocks-tree-skips-empty-sections-test
  (let [tree (minutes/blocks-tree (minutes/normalize {:summary "Doar rezumat"} segs) {:audio-ref "x"})]
    (is (= ["**Summary**"] (map :content (:children tree))))))
