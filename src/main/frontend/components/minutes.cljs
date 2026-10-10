(ns frontend.components.minutes
  "Dialog and settings for AI-written meeting minutes."
  (:require [clojure.string :as string]
            [frontend.handler.dictation :as dictation]
            [frontend.handler.minutes :as minutes]
            [frontend.handler.speakers :as speakers]
            [frontend.state :as state]
            [frontend.ui :as ui]
            [frontend.util :as util]
            [logseq.shui.ui :as shui-ui]
            [rum.core :as rum]))

;; --- dialog ------------------------------------------------------------------------------

(rum/defc dialog < rum/reactive
  {:did-mount (fn [state] (minutes/load! (first (:rum/args state))) state)
   :will-unmount (fn [state] (when (= :busy (:status @minutes/*state)) (minutes/cancel!)) state)}
  [info]
  (let [{:keys [status message error last]} (rum/react minutes/*state)
        {:keys [url model]} (minutes/config)
        busy? (= :busy status)
        configured? (not (string/blank? model))
        remote? (not (minutes/local-server? url))]
    [:div.p-4 {:style {:min-width "30rem" :max-width "38rem"}}
     [:h2.text-lg.font-medium.mb-1 "Meeting minutes (AI)"]
     [:p.text-xs.opacity-70.mb-3 (:page-name info)]

     [:div.text-sm.mb-3
      (if configured?
        [:div
         [:div "Server: " [:b (minutes/host-of url)] " · Model: " [:b model]]
         (when remote?
           [:div.mt-1 {:style {:color "var(--rx-orange-10, #d9822b)"}}
            (ui/icon "alert-triangle" {:size 14})
            " The transcript will be sent to " [:b (minutes/host-of url)] ", not processed on this computer."])]
        [:div "No model is set. Choose a server and model in "
         [:a.cursor-pointer.underline {:on-click #(do (state/close-modal!) (state/open-settings! :dictation))}
          "Settings → Dictation"] "."])]

     (when last
       [:p.text-xs.opacity-70.mb-3
        "Minutes already on this page: generated "
        (.toLocaleString (js/Date. (:generated-at last))) " with " (:model last)
        ". Generating again replaces them."])

     [:div.flex.items-center.gap-2.mb-3
      (shui-ui/button {:size :sm :disabled (or busy? (not configured?))
                       :on-click #(minutes/generate! info)}
                      (if last "Regenerate minutes" "Generate minutes"))
      (when busy?
        (shui-ui/button {:size :sm :variant :outline :on-click minutes/cancel!} "Cancel"))]

     (when message
       [:div.text-sm.flex.items-center
        (when busy? (ui/icon "loader-2" {:class "animate-spin mr-2"}))
        message])
     (when error
       [:div.text-sm {:style {:color "var(--rx-red-10, #e5484d)"}} error])

     [:p.text-xs.opacity-70.mt-3
      "The minutes are written at the top of the page: summary, topics, decisions, action items (as TODOs) "
      "and open questions. Each item has a play button that jumps to where it was discussed."]]))

(rum/defc minutes-button < rum/reactive
  []
  (state/sub :route-match) ; re-evaluate when the route changes
  (when-let [info (speakers/current-meeting)]
    [:button.button.icon.inline.mx-1
     {:title "Write the meeting minutes with AI"
      :on-click #(state/set-modal! (fn [] (dialog info))
                                   {:id :meeting-minutes :center? true :close-btn? true})}
     (ui/icon "list-details")]))

;; --- settings section -----------------------------------------------------------------------

(rum/defcs settings-section < rum/reactive
  (rum/local nil ::result)   ; nil | :testing | {:ok .. :models .. :error ..}
  (rum/local 0 ::tick)       ; forces a re-render after a preference changes
  [state]
  (let [*result (::result state)
        *tick (::tick state)
        _ (rum/react *tick)
        {:keys [url model api-key part-chars]} (minutes/config)
        result @*result
        pref! (fn [k] (fn [e] (dictation/set-pref! k (util/evalue e)) (swap! *tick inc)))
        row (fn [label description input]
              [:div.it.sm:grid.sm:grid-cols-3.sm:gap-4.sm:items-center.py-2
               [:div.flex.flex-col
                [:label.block.text-sm.font-medium.leading-5.opacity-70 label]
                (when description [:div.text-xs.text-gray-10 description])]
               [:div.mt-1.sm:mt-0.sm:col-span-2 input]])
        test! (fn []
                (reset! *result :testing)
                (-> (minutes/test-connection!) (.then #(reset! *result %))))]
    [:div.mt-6
     [:h3.text-base.font-medium.mb-1 "Meeting minutes (AI)"]
     [:p.text-xs.opacity-70.mb-2
      "Any OpenAI-compatible server works: Ollama on this computer (for example "
      [:code "ollama pull gemma3:12b"] "), or a remote one. The transcript is sent only to the address below."]
     (row "Server address" "Ollama: http://localhost:11434"
          [:input.form-input.is-small.w-full
           {:value url :placeholder minutes/default-url :on-change (pref! :llm-url)}])
     (row "Model" "Name as the server knows it, e.g. gemma3:12b"
          [:div.flex.items-center.gap-2
           [:input.form-input.is-small.flex-1
            {:value model :list "llm-models" :placeholder "gemma3:12b" :on-change (pref! :llm-model)}]
           [:datalist#llm-models
            (for [m (when (map? result) (:models result))] [:option {:key m :value m}])]
           (shui-ui/button {:size :sm :variant :outline :disabled (= :testing result) :on-click test!}
                           "Test / list models")])
     (when result
       [:div.text-sm.mb-1
        (cond
          (= :testing result) "Connecting..."
          (:ok result) (str "Connected. " (count (:models result)) " models available"
                            (when (and (seq model) (not (some #{model} (:models result))))
                              " (the model above is not among them)")
                            ".")
          :else [:span {:style {:color "var(--rx-red-10, #e5484d)"}} (:error result)])])
     (row "API key" "Only for servers that need one. Stored on this computer."
          [:input.form-input.is-small.w-full
           {:type "password" :value api-key :on-change (pref! :llm-key)}])
     (row "Part size" "Long meetings are summarized in parts. Use smaller parts for small models."
          [:select.form-select.is-small
           {:value (str part-chars) :on-change (pref! :llm-part-chars)}
           (for [[v label] [["6000" "Small (about 6,000 characters)"]
                            [(str minutes/default-part-chars) "Medium (about 14,000)"]
                            ["30000" "Large (about 30,000)"]]]
             [:option {:key v :value v} label])])]))
