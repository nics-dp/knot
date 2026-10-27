(ns knot.events-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datalevin.core :as d]
            [integrant.core :as ig]
            [knot.db :as db]
            [knot.events :as events]
            [knot.seed :as seed]
            [knot.server :as server]
            [knot.sync :as sync]
            [knot.ui :as ui]
            [org.httpkit.server :as http])
  (:import (java.net URI)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)
           (java.util.concurrent LinkedBlockingQueue TimeUnit)))

(def ^:dynamic *conn* nil)

(use-fixtures :each
  (fn [t]
    (let [dir (str (java.nio.file.Files/createTempDirectory "knot-events" (make-array java.nio.file.attribute.FileAttribute 0)))
          conn (db/open-report dir)]
      (try
        (binding [*conn* conn] (t))
        (finally (db/close-report conn))))))

(def fixture (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn"))))

(defn- edit!
  "改某張卡片的意圖，回傳 [卡片 id 寫入結果]。"
  [conn title text opts]
  (let [loaded (db/load-state (d/db conn))
        para (some #(when (= title (:node/title %)) (:node/id %)) (:nodes loaded))
        state {:nodes (into {} (map (juxt :node/id identity)) (:nodes loaded))
               :relations (:relations loaded) :ui {} :history []}]
    [para (db/apply-changes! conn (merge {:label "編輯意圖"
                                          :changes (sync/diff state (ui/set-node-field state para :node/draft text))}
                                         opts))]))

(deftest tx-event-test
  (seed/seed! *conn* fixture)
  (let [report (atom nil)]
    (d/listen! *conn* :t (fn [r] (reset! report r)))
    (testing "修改組：id、origin、by、label、受影響的卡片"
      (let [[para res] (edit! *conn* "節能效益估算" "新的意圖" {:by :agent :origin :propagation})
            ev (events/tx-event (:tx-data @report) (:db-after @report))]
        (is (= {:type :change :changeset/id (:changeset/id res) :origin :propagation
                :by :agent :label "編輯意圖" :nodes [para]}
               ev))))
    (testing "沒有修改組的交易：重新載入"
      (db/update-report! *conn* {:report/title "新名稱"})
      (is (= {:type :reload} (events/tx-event (:tx-data @report) (:db-after @report)))))
    (d/unlisten! *conn* :t)))

(deftest sse-test
  (seed/seed! *conn* fixture)
  (let [system (ig/init {:knot/events {:db *conn*}})
        events (:knot/events system)
        server (http/run-server (server/handler *conn* events) {:port 0 :legacy-return-value? false})
        port (http/server-port server)
        client (-> (HttpClient/newBuilder) (.version java.net.http.HttpClient$Version/HTTP_1_1) .build)
        lines (LinkedBlockingQueue.)
        req (-> (HttpRequest/newBuilder (URI. (str "http://localhost:" port "/api/events"))) .build)
        resp (.sendAsync client req (HttpResponse$BodyHandlers/ofInputStream))]
    (try
      (let [res (.get resp 3 TimeUnit/SECONDS)]
        (is (= 200 (.statusCode res)))
        (is (= "text/event-stream; charset=utf-8" (.orElse (.firstValue (.headers res) "content-type") nil)))
        (doto (Thread. #(try (with-open [r (io/reader ^java.io.InputStream (.body res))] (doseq [l (line-seq r)] (.put lines l))) (catch Throwable e (println "reader:" e))))
          (.setDaemon true) (.start))
        (loop [n 0]                     ; 等訂閱者登記完成再寫入
          (when (and (empty? @(:subscribers events)) (< n 50)) (Thread/sleep 50) (recur (inc n))))
        (let [[_ r] (edit! *conn* "節能效益估算" "SSE 測試" {})
              got (loop [acc []]
                    (if-let [l (.poll lines 3 TimeUnit/SECONDS)]
                      (if (str/starts-with? l "data: ") (conj acc l) (recur (conj acc l)))
                      acc))]
          (is (some #{"event: change"} got))
          (is (some #(str/includes? % (str (:changeset/id r))) got))))
      (finally
        (ig/halt! system)
        (http/server-stop! server)
        (.shutdownNow client)))))
