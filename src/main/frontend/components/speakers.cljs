(ns frontend.components.speakers
  "Dialog and header button for naming the speakers of a meeting."
  (:require [clojure.string :as string]
            [frontend.components.audio-player :as audio-player]
            [frontend.handler.speakers :as speakers]
            [frontend.state :as state]
            [frontend.ui :as ui]
            [frontend.util :as util]
            [logseq.shui.ui :as shui-ui]
            [rum.core :as rum]))

(defn- clock [seconds]
  (let [s (js/Math.round seconds)]
    (str (quot s 60) ":" (when (< (mod s 60) 10) "0") (mod s 60))))

(rum/defcs speaker-row < (rum/local nil ::name)
  [state info {:keys [id seconds] :as speaker}]
  (let [*name (::name state)
        value (or @*name (:name speaker) "")
        save! #(speakers/rename! info id value)]
    [:div.flex.items-center.gap-2.py-1
     [:button.button.icon.inline
      {:title "Listen to this speaker"
       :on-click #(when-let [start (speakers/sample-start id)]
                    (audio-player/play! (:audio-ref info) start))}
      (ui/icon "player-play")]
     [:span.text-sm.opacity-70 {:style {:min-width "8rem"}}
      (str "Vorbitor " (inc id) " · " (clock seconds))]
     [:input.form-input.is-small.flex-1
      {:list "known-speakers"
       :placeholder "Name"
       :value value
       :on-change #(reset! *name (util/evalue %))
       :on-key-down (fn [e] (when (= "Enter" (.-key e)) (save!)))}]
     (shui-ui/button {:size :sm :variant :outline :on-click save!} "Save")]))

(rum/defcs dialog < rum/reactive
  (rum/local "" ::num)
  {:did-mount (fn [state] (speakers/load! (first (:rum/args state))) state)}
  [state info]
  (let [{:keys [status message speakers profiles]} (rum/react speakers/*state)
        *num (::num state)
        busy? (= :busy status)
        n (let [v (js/parseInt @*num 10)] (when (pos? v) v))]
    [:div.p-4 {:style {:min-width "30rem" :max-width "38rem"}}
     [:h2.text-lg.font-medium.mb-1 "Speakers"]
     [:p.text-xs.opacity-70.mb-3 (:page-name info)]

     [:div.flex.items-center.gap-2.mb-3
      [:label.text-sm "Number of speakers"]
      [:input.form-input.is-small
       {:style {:width "4.5rem"} :placeholder "auto"
        :value @*num :on-change #(reset! *num (util/evalue %))}]
      (shui-ui/button {:size :sm :disabled busy?
                       :on-click #(speakers/analyze! info n)}
                      (if (seq speakers) "Analyze again" "Identify speakers"))]

     (when message
       [:div.text-sm.mb-3.flex.items-center
        (when busy? (ui/icon "loader-2" {:class "animate-spin mr-2"}))
        message])

     [:datalist#known-speakers
      (for [{:keys [name]} profiles] [:option {:key name :value name}])]

     (for [s speakers]
       (rum/with-key (speaker-row info s) (str "speaker-" (:id s))))

     (when (seq speakers)
       [:p.text-xs.opacity-70.mt-2
        "Naming a speaker saves their voice on this computer, so they are recognized in future meetings. "
        "Blocks you edited by hand are not relabeled."])

     (when (seq profiles)
       [:div.mt-4
        [:div.text-xs.opacity-70.mb-1 "Saved voices (stored only on this computer)"]
        [:div.flex.flex-wrap.gap-2
         (for [{:keys [name count]} profiles]
           [:span.inline-flex.items-center.text-xs.px-2.py-1.rounded
            {:key name :style {:background "var(--ls-tertiary-background-color)"}}
            (str name (when (> count 1) (str " ×" count)))
            [:a.ml-2.cursor-pointer {:title "Forget this voice"
                                     :on-click #(speakers/delete-profile! name)}
             (ui/icon "x" {:size 12})]])]])]))

(rum/defc speakers-button < rum/reactive
  []
  (state/sub :route-match) ; re-evaluate when the route changes
  (when-let [info (speakers/current-meeting)]
    [:button.button.icon.inline.mx-1
     {:title "Who said what (identify and name the speakers)"
      :on-click #(state/set-modal! (fn [] (dialog info))
                                   {:id :meeting-speakers :center? true :close-btn? true})}
     (ui/icon "user-search")]))
