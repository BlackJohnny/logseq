(ns electron.llm-test
  (:require ["http" :as http]
            [cljs.test :refer [deftest is async]]
            [electron.llm :as llm]
            [promesa.core :as p]))

(defn- sse-server [handler]
  (p/create (fn [resolve _]
              (let [srv (.createServer http handler)]
                (.listen srv 0 "127.0.0.1" #(resolve srv))))))

(deftest streamed-answer
  (async done
    (p/let [srv (sse-server
                 (fn [^js req ^js res]
                   (.writeHead res 200 #js {"Content-Type" "text/event-stream"})
                   (.write res "data: {\"choices\":[{\"delta\":{\"content\":\"{\\\"a\\\":\"}}]}\n\n")
                   (js/setTimeout
                    (fn []
                      ;; one event split across two writes
                      (.write res "data: {\"choices\":[{\"delta\":{\"con")
                      (.write res "tent\":\" 1}\"}}]}\n\ndata: [DONE]\n\n")
                      (.end res))
                    50)))
            port (.-port (.address srv))
            seen (atom [])
            r (llm/chat! "t1" {:url (str "http://127.0.0.1:" port) :model "m"} [{:role "user" :content "hi"}]
                         {:json? true} #(swap! seen conj %))]
      (is (= {:ok true :text "{\"a\": 1}"} r))
      (is (= 8 (last @seen)))
      (.close srv)
      (done))))

(deftest plain-json-fallback-and-errors
  (async done
    (p/let [srv (sse-server
                 (fn [^js req ^js res]
                   (.writeHead res 200 #js {"Content-Type" "application/json"})
                   (.end res "{\"choices\":[{\"message\":{\"content\":\"plain\"}}]}")))
            port (.-port (.address srv))
            r (llm/chat! "t2" {:url (str "http://127.0.0.1:" port) :model "m"} [] {} nil)
            _ (p/create (fn [resolve _] (.close srv resolve)))
            bad (llm/chat! "t3" {:url (str "http://127.0.0.1:" port) :model "m"} [] {} nil)]
      (is (= {:ok true :text "plain"} r))
      (is (false? (:ok bad)))
      (is (re-find #"ECONNREFUSED" (:error bad)) (:error bad))
      (done))))
