(ns user
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [integrant.repl :as ir]
            [integrant.repl.state :as state]
            [knot.db :as db]
            [knot.seed :as seed]
            [knot.system :as system]))

(ir/set-prep! (fn [] (system/config)))

(defn go [] (ir/go))
(defn halt [] (ir/halt))
(defn reset [] (ir/reset))

(defn system [] state/system)

(defn store [] (:knot/store state/system))

(defn seed!
  "以範例報告建立一份新報告，回傳 report id。預設用 test-resources 的虛構範例；
  (seed! \"local/sample-report.edn\") 用本機測試報告。"
  ([] (seed/seed! (store) (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn")))))
  ([path] (seed/seed! (store) (edn/read-string (slurp path)))))

(defn db
  "最近修改的報告的 Datalevin 連線，方便直接下查詢；也可指定 report id。"
  ([] (some->> (db/list-reports (store)) first :report/id db))
  ([report-id] (db/conn (store) report-id)))

;; (import-file! "path") 於 M3 加入
