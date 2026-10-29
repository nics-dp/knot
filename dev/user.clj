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

(defonce ^:private current-dir (atom report-dir))

(ir/set-prep! (fn [] (system/config {:report-dir @current-dir :dev? true})))

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

(defn use-report!
  "把執行中的系統切到另一個報告目錄（例如試寫用的 local/trial-report）並重新啟動；
  畫面重新整理後就是那份報告。(use-report!) 切回 local/report。"
  ([] (use-report! report-dir))
  ([dir]
   (halt)
   (reset! current-dir dir)
   (go)
   dir))

(defn wipe!
  "刪除目前報告目錄（預設 local/report）的資料庫並重新啟動，得到一份空白報告。"
  []
  (halt)
  (let [dir (kdb/db-dir @current-dir)]
    (doseq [f (reverse (file-seq dir))] (.delete ^java.io.File f)))
  (go))

;; (import-file! "path") 於 M5 加入

(defn- call-tool [conn tool-name args]
  (if-let [tool (some #(when (= tool-name (:name %)) %) mcp/tools)]
    ((:handler tool) conn args)
    (throw (ex-info (str "沒有這個工具：" tool-name) {:tools (map :name mcp/tools)}))))

(defn mcp!
  "直接呼叫 MCP 工具的 handler（不經 HTTP），回傳結果（Clojure 資料）。
  對 dev 報告（真實資料）寫入；試寫入請用 sb-mcp!。
  (mcp! \"get_outline\")、(mcp! \"get_card\" {\"id\" \"...\"})"
  ([tool-name] (mcp! tool-name {}))
  ([tool-name args] (call-tool (db) tool-name args)))

;; ---------------------------------------------------------------------------
;; 沙盒：暫存目錄裡的一份獨立報告，匯入虛構範例，可以任意寫入；不影響 dev 報告與畫面

(defonce ^:private sandbox-state (atom nil))

(defn sandbox-close!
  "關閉沙盒並刪除它的暫存目錄。"
  []
  (when-let [{:keys [conn dir]} @sandbox-state]
    (kdb/close-report conn)
    (doseq [f (reverse (file-seq dir))] (.delete ^java.io.File f))
    (reset! sandbox-state nil))
  nil)

(defn sandbox!
  "開一份新的沙盒報告（先關掉舊的），匯入虛構的路燈範例，回傳連線。
  (sandbox! \"path.edn\") 改用其他範例檔。"
  ([] (sandbox! nil))
  ([path]
   (sandbox-close!)
   (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                       "knot-sandbox" (make-array java.nio.file.attribute.FileAttribute 0)))
         conn (kdb/open-report (str dir))]
     (seed/seed! conn (edn/read-string (slurp (or path (io/resource "fixtures/reports/streetlight.edn")))))
     (reset! sandbox-state {:conn conn :dir dir})
     conn)))

(defn sb
  "沙盒的連線；還沒開時先開一份。"
  []
  (or (:conn @sandbox-state) (sandbox!)))

(defn sb-mcp!
  "在沙盒呼叫 MCP 工具，用法同 mcp!。"
  ([tool-name] (sb-mcp! tool-name {}))
  ([tool-name args] (call-tool (sb) tool-name args)))
