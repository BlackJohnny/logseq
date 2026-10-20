(ns frontend.util.meeting-export-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as string]
            [frontend.util.meeting-export :as export]))

(def transcript
  {:title "Weekly" :started-at "2026-10-20T09:00:00.000Z"
   :recordings [{:audio "audio.webm" :duration 60} {:audio "audio-2.webm" :duration 30}]
   :speakers [{:id 0 :name "Ana" :seconds 10} {:id 1 :name "" :seconds 5}]
   :segments [{:id 1 :start 5 :end 8 :text " Salut " :speaker 0 :audio "audio.webm" :block "b1"}
              {:id 0 :start 1 :end 2 :text "A < B" :speaker 1 :audio "audio.webm"}
              {:id 2 :start 0 :end 4 :text "Din nou" :audio "audio-2.webm"}
              {:id 3 :start 9 :end 10 :text "  " :audio "audio.webm"}]
   :pinned {"b1" 0}})

(deftest export-data-test
  (let [d (export/export-data {:transcript transcript
                               :minutes {:language "ro" :summary "S"
                                         :decisions [{:text "Da" :start 0 :audio "audio-2.webm" :refs [2]}]}})]
    (is (= "Weekly" (:title d)))
    (is (= 90 (:duration d)))
    (is (= [{:id 0 :name "Ana"} {:id 1 :name "Vorbitor 2"}] (:speakers d)))
    (testing "blank segments are dropped, the rest ordered by recording then time, speakers by index"
      (is (= [["A < B" 0] ["Salut" 0] ["Din nou" 1]] (map (juxt :text :rec) (:segments d)))))
    (testing "minutes items point at their recording"
      (is (= 1 (get-in d [:minutes :decisions 0 :rec])))
      (is (nil? (get-in d [:minutes :decisions 0 :refs]))))
    (testing "nothing that identifies a voice or a block leaves the app"
      (is (not (string/includes? (pr-str d) "pinned")))
      (is (not (string/includes? (pr-str d) "b1"))))))

(deftest render-html-test
  (let [d (export/export-data {:transcript transcript})
        out (export/render-html (str "<body>" export/data-marker "</body>") d {0 "QUJD"})]
    (is (string/includes? out "id=\"audio-0\">QUJD</script>"))
    (is (not (string/includes? out export/data-marker)))
    (testing "text cannot close the script block"
      (is (not (string/includes? out "A < B"))))))

(deftest vtt-test
  (let [d (export/export-data {:transcript transcript})]
    (is (= "00:01:01.500" (export/vtt-time 61.5)))
    (is (= "WEBVTT\n\n00:00:01.000 --> 00:00:02.000\n<v Vorbitor 2>A &lt; B\n\n00:00:05.000 --> 00:00:08.000\n<v Ana>Salut\n\n"
           (export/vtt d 0)))
    (is (= "WEBVTT\n\n00:00:00.000 --> 00:00:04.000\nDin nou\n\n" (export/vtt d 1)))))
