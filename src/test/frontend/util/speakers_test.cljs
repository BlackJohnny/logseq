(ns frontend.util.speakers-test
  (:require [cljs.test :refer [deftest is testing]]
            [frontend.util.speakers :as speakers]))

(deftest ours?-test
  (let [m "{{audio-timestamp ../assets/m/audio.webm, 12}}"]
    (is (speakers/ours? (str m "  Salut, ce faceți?") " Salut, ce faceți?"))
    (is (speakers/ours? (str m " **Vorbitor 2:** Salut,\nce faceți?") "Salut, ce faceți?"))
    (is (speakers/ours? (str m " [[Ana Pop]]: Salut, ce faceți?") "Salut, ce faceți?"))
    (is (not (speakers/ours? (str m " [[Ana]]: Altceva") "Salut, ce faceți?")) "hand-edited text")
    (is (not (speakers/ours? "Salut, ce faceți?" "Salut, ce faceți?")) "no macro")))

(def voice-a [1 0 0])
(def voice-b [0 1 0])

(deftest reassign-test
  (let [segs [{:speaker 0} {:speaker 0} {:speaker 1} {:speaker 0}]
        embs [voice-a [0.1 0.9 0] voice-b nil]
        spk-embs {0 voice-a 1 voice-b}]
    (testing "a segment that sounds like another speaker moves; short ones stay"
      (is (= [0 1 1 0] (speakers/reassign segs embs [nil nil nil nil] spk-embs))))
    (testing "pinned segments keep the user's speaker"
      (is (= [1 0 1 0] (speakers/reassign segs embs [1 nil nil nil] spk-embs))))
    (testing "pinned examples redefine the speaker"
      (let [segs [{:speaker 0} {:speaker 0}]
            embs [[0 0 1] [0.1 0 0.9]]]
        (is (= [1 1] (speakers/reassign segs embs [1 nil] {0 voice-a 1 voice-b})))))
    (testing "an utterance with no speaker needs a clear match to be assigned"
      (is (= [1] (speakers/reassign [{:speaker nil}] [[0.6 0.8 0]] [nil] {1 voice-b})) "0.8 is enough")
      (is (= [nil] (speakers/reassign [{:speaker nil}] [[0.8 0.5 0.3]] [nil] {1 voice-b})) "0.5 is not"))
    (testing "no clear winner leaves the segment alone"
      (is (= [0] (speakers/reassign [{:speaker 0}] [[1 1 0]] [nil] spk-embs))))))
