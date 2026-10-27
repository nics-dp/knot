(ns user
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [integrant.repl :as ir]
            [integrant.repl.state :as state]
            [knot.db :as kdb]
            [knot.mcp :as mcp]
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

;; (import-file! "path") 於 M5 加入

(defn mcp!
  "直接呼叫 MCP 工具的 handler（不經 HTTP），回傳結果（Clojure 資料）。
  (mcp! \"get_outline\")、(mcp! \"get_card\" {\"id\" \"...\"})"
  ([tool-name] (mcp! tool-name {}))
  ([tool-name args]
   (if-let [tool (some #(when (= tool-name (:name %)) %) mcp/tools)]
     ((:handler tool) (db) args)
     (throw (ex-info (str "沒有這個工具：" tool-name) {:tools (map :name mcp/tools)})))))
