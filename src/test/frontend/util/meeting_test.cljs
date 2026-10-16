(ns frontend.util.meeting-test
  (:require [cljs.test :refer [deftest is testing]]
            [frontend.util.meeting :as meeting]))

(deftest audio-ref-for-test
  (is (= "../assets/meetings/m/audio-2.webm"
         (meeting/audio-ref-for "../assets/meetings/m/audio.webm" "audio-2.webm")))
  (is (= "../assets/meetings/m/audio.webm"
         (meeting/audio-ref-for "../assets/meetings/m/audio.webm" "audio.webm")))
  (testing "segments recorded before there could be several recordings have no file"
    (is (= "../assets/meetings/m/audio.webm" (meeting/audio-ref-for "../assets/meetings/m/audio.webm" nil)))
    (is (= "x" (meeting/audio-ref-for "x" "")))))

;; Taken from a real recording: a podcast played on headphones, whose sound the
;; headset microphone picked up as well.
(def system
  [{:source "system" :start 0 :end 18
    :text "Cam asta e ce face politicul cu noi.\n Deci, ăștia sunt responsabili.\n Ca să revin la întrebare."}
   {:source "system" :start 31.5 :end 37.8
    :text "Păi numai când, să zicem că lucrezi, să zicem că ești salariat, cum sunt mulți sau, da?"}
   {:source "system" :start 40.3 :end 42 :text "Întâi."}
   {:source "system" :start 44.3 :end 48.9 :text "Și să zicem că ți iei salariul pe 30 noiembrie."}])

(defn- mic [start end text] {:source "mic" :start start :end end :text text})

(deftest leak-test
  (testing "the microphone repeating the system audio is a leak"
    (is (meeting/leak? (mic 3.7 9.3 "Așa sunt responsabili. Ca să revin la întrebarea") system))
    (is (meeting/leak? (mic 31.3 34.4 "Păi numai când, să zicem, că lucrezi.") system))
    (is (meeting/leak? (mic 40.4 42 "ÎNTÂI.") system) "even a single word, when it lines up")
    (is (meeting/leak? (mic 44.3 47.2 "Și să zicem că ți-ai salariul pe") system)))
  (testing "diacritics and word endings do not matter"
    (is (meeting/leak? (mic 45 48 "si sa zicem ca ti iei salariul pe noiembri") system)))
  (testing "the user's own speech is kept"
    (is (not (meeting/leak? (mic 3.7 9.3 "Da, sunt de acord, hai să trecem la buget") system)))
    (is (not (meeting/leak? (mic 20 24 "Întâi.") system)) "same word but nothing said then"))
  (testing "nothing to compare with"
    (is (not (meeting/leak? (mic 3.7 9.3 "Așa sunt responsabili") [])))
    (is (not (meeting/leak? (mic 3.7 9.3 "Așa sunt responsabili")
                            [(assoc (first system) :source "mic")]))
        "only system segments count")))
