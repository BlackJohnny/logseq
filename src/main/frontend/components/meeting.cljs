(ns frontend.components.meeting
  "Header button and title dialog for live meeting notes."
  (:require [clojure.string :as string]
            [frontend.handler.meeting :as meeting]
            [frontend.state :as state]
            [frontend.ui :as ui]
            [frontend.util :as util]
            [logseq.shui.ui :as shui-ui]
            [rum.core :as rum]))

(rum/defcs start-dialog < (rum/local "" ::title)
  [state]
  (let [*title (::title state)
        submit! (fn []
                  (state/close-modal!)
                  (meeting/start! @*title))]
    [:div.p-4 {:style {:min-width "22rem"}}
     [:h2.text-lg.font-medium.mb-3 "New meeting"]
     [:input.form-input.w-full
      {:auto-focus true
       :placeholder "Meeting title"
       :value @*title
       :on-change #(reset! *title (util/evalue %))
       :on-key-down (fn [e] (when (= "Enter" (.-key e)) (submit!)))}]
     [:p.text-xs.mt-2.opacity-70
      "Records the microphone and writes the transcript to a new page under meetings/."]
     [:div.mt-3.flex.justify-end
      (shui-ui/button {:size :sm :on-click submit!} "Start")]]))

(defn- clock [seconds]
  (str (quot seconds 60) ":" (when (< (mod seconds 60) 10) "0") (mod seconds 60)))

(rum/defc meeting-button < rum/reactive
  []
  (let [{:keys [status elapsed pending]} (rum/react meeting/*state)]
    [:button.button.icon.inline.mx-1
     {:title (case status
               :starting "Starting the meeting..."
               :recording "Stop the meeting"
               :stopping "Finishing transcription..."
               "Take meeting notes (live transcription)")
      :disabled (contains? #{:starting :stopping} status)
      :on-click (fn []
                  (case status
                    :idle (state/set-modal! start-dialog {:id :meeting-start :center? true :close-btn? false})
                    :recording (meeting/stop!)
                    nil))}
     (case status
       :recording [:span.flex.items-center {:style {:color "#e5484d"}}
                   (ui/icon "player-stop")
                   [:span.ml-1.text-xs (str (clock elapsed) (when (pos? pending) (str " · " pending)))]]
       (:starting :stopping) (ui/icon "loader-2" {:class "animate-spin"})
       (ui/icon "users"))]))
