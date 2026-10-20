(ns frontend.components.meeting
  "Header button and title dialog for live meeting notes."
  (:require [clojure.string :as string]
            [frontend.handler.meeting :as meeting]
            [frontend.handler.meeting-export :as meeting-export]
            [frontend.handler.speakers :as speakers]
            [frontend.state :as state]
            [frontend.ui :as ui]
            [frontend.util :as util]
            [logseq.shui.ui :as shui-ui]
            [promesa.core :as p]
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

(defn- local-input-value
  "Value for <input type=datetime-local> (local time, minutes)."
  [^js d]
  (let [two #(if (< % 10) (str "0" %) (str %))]
    (str (.getFullYear d) "-" (two (inc (.getMonth d))) "-" (two (.getDate d))
         "T" (two (.getHours d)) ":" (two (.getMinutes d)))))

(defn- file-title [name]
  (string/replace name #"\.[^.]+$" ""))

(rum/defcs import-dialog < (rum/local nil ::file) (rum/local "" ::title) (rum/local "" ::date)
  [state]
  (let [*file (::file state) *title (::title state) *date (::date state)
        pick! (fn []
                (p/let [f (meeting/pick-import-file!)]
                  (when f
                    (reset! *file f)
                    (when (string/blank? @*title) (reset! *title (file-title (:name f))))
                    (reset! *date (local-input-value (js/Date. (:mtime-ms f)))))))
        date (when (seq @*date) (js/Date. @*date))
        ready? (and @*file date (not (js/isNaN (.getTime date))))
        submit! (fn []
                  (state/close-modal!)
                  (meeting/import! {:path (:path @*file) :title @*title :date date}))]
    [:div.p-4 {:style {:min-width "24rem"}}
     [:h2.text-lg.font-medium.mb-3 "Import a recording"]
     [:div.flex.items-center
      (shui-ui/button {:size :sm :variant :outline :on-click pick!} "Choose file…")
      [:span.ml-2.text-sm.opacity-80.truncate (or (:name @*file) "m4a, mp3, wav, ogg, flac, mp4…")]]
     [:input.form-input.w-full.mt-3
      {:placeholder "Meeting title"
       :value @*title
       :on-change #(reset! *title (util/evalue %))}]
     [:label.block.text-xs.mt-3.opacity-70 "When did the meeting take place?"]
     [:input.form-input.w-full
      {:type "datetime-local"
       :value @*date
       :on-change #(reset! *date (util/evalue %))}]
     [:p.text-xs.mt-2.opacity-70
      "Needs ffmpeg installed on this computer. The audio is converted into the graph and transcribed like a live meeting; run the speakers button afterwards to identify who spoke."]
     [:div.mt-3.flex.justify-end
      (shui-ui/button {:size :sm :disabled (not ready?) :on-click submit!} "Import")]]))

(rum/defc import-button < rum/reactive
  []
  (let [{:keys [status progress]} (rum/react meeting/*state)]
    (case status
      :importing
      [:button.button.icon.inline.mx-1
       {:title "Transcribing the imported recording (click to cancel)"
        :on-click #(meeting/cancel-import!)}
       [:span.flex.items-center {:style {:color "#e5484d"}}
        (ui/icon "loader-2" {:class "animate-spin"})
        [:span.ml-1.text-xs (str (js/Math.floor (* 100 (or progress 0))) "%")]]]

      :idle
      [:button.button.icon.inline.mx-1
       {:title "Import a recording (transcribe an audio file as a meeting)"
        :on-click #(state/set-modal! import-dialog {:id :meeting-import :center? true :close-btn? false})}
       (ui/icon "file-import")]

      nil)))

(defn- clock [seconds]
  (str (quot seconds 60) ":" (when (< (mod seconds 60) 10) "0") (mod seconds 60)))

(rum/defcs export-dialog < (rum/local :html ::format) (rum/local true ::audio) (rum/local true ::minutes)
  [state info]
  (let [*format (::format state) *audio (::audio state) *minutes (::minutes state)
        submit! (fn []
                  (state/close-modal!)
                  (meeting-export/export! info {:format @*format :audio? @*audio :minutes? @*minutes}))
        radio (fn [value label hint]
                [:label.flex.items-start.mt-2.cursor-pointer
                 [:input.mt-1 {:type "radio" :name "meeting-export-format" :checked (= value @*format)
                               :on-change #(reset! *format value)}]
                 [:span.ml-2 label [:span.block.text-xs.opacity-70 hint]]])
        check (fn [*v label]
                [:label.flex.items-center.mt-2.cursor-pointer
                 [:input {:type "checkbox" :checked @*v :on-change #(swap! *v not)}]
                 [:span.ml-2 label]])]
    [:div.p-4 {:style {:min-width "24rem"}}
     [:h2.text-lg.font-medium.mb-1 "Export meeting"]
     [:p.text-xs.opacity-70 "For someone without Logseq: they can listen and jump through the audio by clicking the text."]
     (radio :html "One HTML file" "Open it in a browser. The audio is inside the file, so it can be large.")
     (radio :zip "Zip archive" "The same page with the audio next to it, plus transcript.json, WebVTT subtitles and the minutes.")
     [:div.mt-3
      (check *audio "Include the audio (without it the text cannot be played)")
      (check *minutes "Include the AI minutes (when generated)")]
     [:p.text-xs.mt-3.opacity-70 "Speaker names are exported; voice profiles are not."]
     [:div.mt-3.flex.justify-end
      (shui-ui/button {:size :sm :on-click submit!} "Export")]]))

(rum/defc export-button < rum/reactive
  "On a meeting page: export it to share."
  []
  (state/sub :route-match)
  (when (rum/react meeting-export/*busy?) nil)
  (when-let [info (speakers/current-meeting)]
    [:button.button.icon.inline.mx-1
     {:title "Export this meeting (HTML page or zip) to share"
      :disabled (rum/react meeting-export/*busy?)
      :on-click #(state/set-modal! (fn [] (export-dialog info)) {:id :meeting-export :center? true :close-btn? false})}
     (ui/icon "share")]))

(rum/defc continue-button < rum/reactive
  "On a meeting page: record another part of it (adds a section, a new audio file)."
  []
  (state/sub :route-match) ; re-evaluate when the route changes
  (when (= :idle (:status (rum/react meeting/*state)))
    (when-let [info (speakers/current-meeting)]
      [:button.button.icon.inline.mx-1
       {:title "Continue recording this meeting (adds a new section to the page)"
        :on-click #(meeting/continue! info)}
       (ui/icon "player-record")])))

(rum/defc meeting-button < rum/reactive
  []
  (let [{:keys [status elapsed pending]} (rum/react meeting/*state)]
    [:button.button.icon.inline.mx-1
     {:title (case status
               :starting "Starting the meeting..."
               :recording "Stop the meeting"
               :stopping "Finishing transcription..."
               "Take meeting notes (live transcription)")
      :disabled (contains? #{:starting :stopping :importing} status)
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
