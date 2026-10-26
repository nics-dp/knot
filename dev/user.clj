(ns user
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [integrant.repl :as ir]
            [integrant.repl.state :as state]
            [knot.db :as kdb]
            [knot.seed :as seed]
            [knot.system :as system]))

(def report-dir
  "開發用的報告目錄（不進 git）。"
  "local/report")

(ir/set-prep! (fn [] (system/config {:report-dir report-dir})))

(defn go [] (ir/go))
(defn halt [] (ir/halt))
(defn reset [] (ir/reset))

(defn system [] state/system)

(defn db
  "目前報告的 Datalevin 連線，方便直接下查詢。"
  []
  (:knot/db state/system))

(defn seed!
  "把範例報告匯入目前的空白報告。預設用 test-resources 的虛構範例；
  (seed! \"local/sample-report.edn\") 用本機測試報告。"
  ([] (seed/seed! (db) (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn")))))
  ([path] (seed/seed! (db) (edn/read-string (slurp path)))))

(defn wipe!
  "刪除開發用報告目錄的資料庫並重新啟動，得到一份空白報告。"
  []
  (halt)
  (let [dir (kdb/db-dir report-dir)]
    (doseq [f (reverse (file-seq dir))] (.delete ^java.io.File f)))
  (go))

;; (import-file! "path") 於 M3 加入
