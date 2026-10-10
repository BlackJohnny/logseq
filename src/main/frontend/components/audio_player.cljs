(ns frontend.components.audio-player
  "One shared audio player for `{{audio-timestamp <audio path>, <seconds>}}` macros.
  Each macro renders a small play button; they all drive the same <audio> element,
  so a long meeting never mounts dozens of players."
  (:require [clojure.string :as string]
            [frontend.config :as config]
            [frontend.handler.editor :as editor-handler]
            [frontend.ui :as ui]
            [logseq.graph-parser.config :as gp-config]
            [promesa.core :as p]
            [rum.core :as rum]))

;; Start a little before the requested time: recorded timestamps point at the
;; detected start of speech, which can clip the first word.
(def ^:private lead-in-s 0.5)

(defonce *player (atom {:path nil :open? false :playing? false :time 0}))

(defonce ^:private audio-el
  (when (exists? js/Audio)
    (let [^js a (js/Audio.)]
      (.addEventListener a "timeupdate" #(swap! *player assoc :time (.-currentTime a)))
      (.addEventListener a "play" #(swap! *player assoc :playing? true))
      (.addEventListener a "pause" #(swap! *player assoc :playing? false))
      a)))

(defn- clock [seconds]
  (let [s (js/Math.floor seconds)
        h (quot s 3600) m (quot (mod s 3600) 60) sec (mod s 60)
        two #(if (< % 10) (str "0" %) (str %))]
    (if (pos? h) (str h ":" (two m) ":" (two sec)) (str (two m) ":" (two sec)))))

(defn- asset-url [path]
  (p/let [url (editor-handler/make-asset-url (config/get-local-asset-absolute-path path))]
    ;; the asset protocol cannot serve media fragments/ranges; use file:// like audio-cp
    (string/replace-first url gp-config/asset-protocol "file://")))

(defn- start-playing! [^js a seconds]
  (set! (.-currentTime a) seconds)
  (-> (.play a) (p/catch (fn [e] (js/console.warn "audio-player: play failed" e)))))

(defn play!
  "Play `path` (as written in the macro) from `seconds`."
  [path seconds]
  (when audio-el
    (let [start (max 0 (- seconds lead-in-s))]
      (swap! *player assoc :open? true)
      (if (and (= path (:path @*player)) (.-src audio-el))
        (start-playing! audio-el start)
        (p/let [url (asset-url path)]
          (swap! *player assoc :path path :time start)
          (.addEventListener audio-el "loadedmetadata" #(start-playing! audio-el start) #js {:once true})
          (set! (.-src audio-el) url)
          (.load audio-el))))))

(defn toggle! []
  (when audio-el
    (if (.-paused audio-el) (.play audio-el) (.pause audio-el))))

(defn skip! [delta]
  (when audio-el
    (set! (.-currentTime audio-el) (max 0 (+ (.-currentTime audio-el) delta)))))

(defn close! []
  (when audio-el
    (.pause audio-el)
    (.removeAttribute audio-el "src")
    (.load audio-el))
  (reset! *player {:path nil :open? false :playing? false :time 0}))

(rum/defc timestamp-button < rum/reactive
  [path seconds]
  (let [{current :path playing? :playing? time :time} (rum/react *player)
        here? (and (= path current) playing?
                   (<= (- seconds 1) time (+ seconds 60)))]
    [:a.inline-flex.items-center.px-1.mr-1.rounded.cursor-pointer
     {:title "Play the recording from here"
      :style {:background "var(--ls-tertiary-background-color)" :font-size "0.85em"}
      :on-click (fn [e] (.stopPropagation e) (play! path seconds))}
     (ui/icon (if here? "player-pause" "player-play") {:size 12})
     [:span.ml-1 (clock seconds)]]))

(rum/defc mini-player < rum/reactive
  []
  (let [{:keys [open? playing? time path]} (rum/react *player)]
    (when open?
      [:div.flex.items-center.gap-2.px-3.py-2.rounded-lg
       {:style {:position "fixed" :bottom "1rem" :left "50%" :transform "translateX(-50%)"
                :z-index 60 :background "var(--ls-secondary-background-color)"
                :border "1px solid var(--ls-border-color)"
                :box-shadow "0 4px 16px rgba(0,0,0,.25)"}}
       [:button.button.inline.text-xs {:title "Back 10 seconds" :on-click #(skip! -10)} "−10s"]
       [:button.button.icon.inline {:title (if playing? "Pause" "Play") :on-click toggle!}
        (ui/icon (if playing? "player-pause" "player-play"))]
       [:button.button.inline.text-xs {:title "Forward 10 seconds" :on-click #(skip! 10)} "+10s"]
       [:span.text-sm.tabular-nums {:style {:min-width "3.5rem"}} (clock time)]
       [:span.text-xs.opacity-60.truncate {:style {:max-width "14rem"}}
        (some-> path (string/split #"/") butlast last)]
       [:button.button.icon.inline {:title "Close" :on-click close!} (ui/icon "x")]])))
