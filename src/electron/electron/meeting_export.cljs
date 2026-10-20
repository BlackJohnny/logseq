(ns electron.meeting-export
  "Saving an exported meeting: ask where, then write the file."
  (:require ["electron" :refer [BrowserWindow dialog]]
            ["fs" :as fs]
            [promesa.core :as p]))

(defn pick-save!
  "Ask where to save. Resolves the chosen path, or nil when cancelled."
  [default-name filter-name extension]
  (p/let [win (.getFocusedWindow BrowserWindow)
          ^js result (.showSaveDialog dialog win
                                      (clj->js {:defaultPath default-name
                                                :filters [{:name filter-name :extensions [extension]}]}))]
    (when-not (.-canceled result)
      (.-filePath result))))

(defn write-file!
  "`encoding` is \"utf8\" or \"base64\"."
  [path data encoding]
  (fs/writeFileSync path (js/Buffer.from data encoding))
  true)
