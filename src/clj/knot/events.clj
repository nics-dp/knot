(ns knot.events
  "事件通知（SPEC 5.13）：報告每次寫入後，以 Server-Sent Events 通知所有開著的畫面。
  事件資料由 tx-report 產生（純函式 tx-event），SSE 只負責送出。"
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [integrant.core :as ig]
            [org.httpkit.server :as http])
  (:import (java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit)))

(defn tx-event
  "由一次交易的 datoms 與交易後的資料庫產生事件資料。
  交易含修改組時：{:type :change :changeset/id .. :origin .. :by .. :label .. :nodes [受影響的卡片 id ...]}；
  其他交易（例如改報告名稱）：{:type :reload}，畫面應重新載入整份報告。"
  [tx-data db-after]
  (if-let [cs-id (some #(when (= :changeset/id (d/datom-a %)) (d/datom-v %)) tx-data)]
    (let [cs (d/pull db-after [:changeset/origin :changeset/by :changeset/label] [:changeset/id cs-id])
          targets (d/q '[:find [?t ...]
                         :in $ ?cid
                         :where [?c :changeset/id ?cid] [?r :revision/changeset ?c] [?r :revision/target ?t]]
                       db-after cs-id)]
      {:type :change
       :changeset/id cs-id
       :origin (:changeset/origin cs)
       :by (:changeset/by cs)
       :label (:changeset/label cs)
       :nodes (vec (sort (keep #(when (str/starts-with? % "nodes/") (parse-uuid (subs % 6))) targets)))})
    {:type :reload}))

(defn format-event
  "SSE 訊息：event: change，data 為事件資料的 EDN。"
  [event]
  (str "event: change\ndata: " (pr-str event) "\n\n"))

(defn broadcast!
  "把訊息送給所有訂閱者；送不出去的連線不處理（關閉時 on-close 會移除）。"
  [subscribers msg]
  (doseq [ch @subscribers]
    (http/send! ch msg false)))

(def ping-interval-seconds 25)

(defmethod ig/init-key :knot/events [_ {:keys [db]}]
  (let [subscribers (atom #{})
        listen-key (keyword "knot.events" (str (random-uuid)))
        scheduler (Executors/newSingleThreadScheduledExecutor
                   (reify ThreadFactory
                     (newThread [_ r] (doto (Thread. ^Runnable r "knot-sse-ping") (.setDaemon true)))))]
    (d/listen! db listen-key
               (fn [{:keys [tx-data db-after]}]
                 (try
                   (broadcast! subscribers (format-event (tx-event tx-data db-after)))
                   (catch Throwable e
                     (println "事件通知失敗：" (ex-message e))))))
    (.scheduleAtFixedRate scheduler #(broadcast! subscribers ": ping\n\n")
                          ping-interval-seconds ping-interval-seconds TimeUnit/SECONDS)
    {:conn db :subscribers subscribers :listen-key listen-key :scheduler scheduler}))

(defmethod ig/halt-key! :knot/events [_ {:keys [conn subscribers listen-key ^ScheduledExecutorService scheduler]}]
  ;; 前一次停止只做了一半時，資料庫可能已經關閉；略過才不會讓 (user/reset) 卡住
  (when-not (d/closed? conn) (d/unlisten! conn listen-key))
  (.shutdownNow scheduler)
  (doseq [ch @subscribers] (http/close ch))
  (reset! subscribers #{}))

(defn handler
  "GET /api/events 的處理函式。events 為 :knot/events 的值。"
  [events]
  (fn [req]
    (http/as-channel
     req
     {:on-open (fn [ch]
                 (http/send! ch {:status 200
                                 :headers {"Content-Type" "text/event-stream; charset=utf-8"
                                           "Cache-Control" "no-cache"
                                           "X-Accel-Buffering" "no"}
                                 :body ": connected\n\n"}
                             false)
                 (swap! (:subscribers events) conj ch))
      :on-close (fn [ch _] (swap! (:subscribers events) disj ch))})))
