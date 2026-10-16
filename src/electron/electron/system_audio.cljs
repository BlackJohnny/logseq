(ns electron.system-audio
  "Capture of what the computer plays (the other meeting participants), Linux/PipeWire only.

  Chromium does not list the monitor of an output device as an input, and it cannot
  open a `Audio/Source/Virtual` node either. So while a meeting runs we keep a
  `pw-loopback` process alive that feeds the monitor of the default output into a
  regular `Audio/Source` node; the renderer then opens that node with getUserMedia
  like any other microphone."
  (:require ["child_process" :as child-process]
            ["electron" :refer [app]]
            [clojure.string :as string]
            [electron.logger :as logger]
            [promesa.core :as p]))

(def source-name "logseq_system_audio")
(def source-label "Logseq System Audio")

(defonce ^:private *proc (atom nil))
(defonce ^:private *quit-hook? (atom false))

(defn stop! []
  (when-let [^js proc @*proc]
    (reset! *proc nil)
    (try (.kill proc) (catch :default _ nil))))

(defn- run
  "Resolves to the trimmed stdout of `cmd`, or nil when it fails or is missing."
  [cmd args]
  (p/create
   (fn [resolve _]
     (child-process/execFile cmd (clj->js args) #js {:timeout 3000}
                             (fn [err stdout]
                               (resolve (when-not err (string/trim (str stdout)))))))))

(defn- source-listed? []
  (p/let [out (run "pactl" ["list" "short" "sources"])]
    (boolean (and out (string/includes? out source-name)))))

(defn- wait-for-source!
  "Resolves true once the node exists, false if the process died or we timed out."
  [^js proc]
  (let [deadline (+ (js/Date.now) 4000)]
    (letfn [(poll []
              (cond
                (some? (.-exitCode proc)) (p/resolved false)
                (> (js/Date.now) deadline) (p/resolved false)
                :else (p/let [ok? (source-listed?)]
                        (if ok? true (p/then (p/delay 150) poll)))))]
      (poll))))

(defn start!
  "Resolves to {:ok true :label ..} or {:ok false :error ..}."
  []
  (stop!)
  (if-not (= "linux" (.-platform js/process))
    (p/resolved {:ok false :error "only available on Linux (PipeWire)"})
    (p/let [sink (run "pactl" ["get-default-sink"])]
      (if (string/blank? sink)
        (p/resolved {:ok false :error "no PipeWire output found (is pactl installed?)"})
        (let [spawn-error (atom nil)
              proc (child-process/spawn
                    "pw-loopback"
                    #js ["-n" "logseq-system-audio"
                         "--capture-props" (str "stream.capture.sink=true node.target=" sink
                                                " node.passive=true")
                         "--playback-props" (str "media.class=Audio/Source node.name=" source-name
                                                 " node.description=\"" source-label "\"")]
                    #js {:stdio #js ["ignore" "ignore" "pipe"]})]
          (.on proc "error" (fn [e] (reset! spawn-error e)))
          (.on (.-stderr proc) "data" (fn [d] (logger/debug "pw-loopback:" (str d))))
          (reset! *proc proc)
          (when-not @*quit-hook?
            (reset! *quit-hook? true)
            (.on ^js app "will-quit" stop!))
          (p/let [ready? (wait-for-source! proc)]
            (if (and ready? (nil? @spawn-error))
              {:ok true :label source-label}
              (do (stop!)
                  {:ok false :error (if @spawn-error
                                      "pw-loopback not found (install pipewire-bin)"
                                      "could not create the capture node")}))))))))
