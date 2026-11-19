(ns knot.export
  "LaTeX 匯出的打包（SPEC 5.9）：內容由 knot.latex 產生，這裡只讀報告、打成 zip、寫檔。
  匯出不是修改，不產生修改組。"
  (:require [clojure.java.io :as io]
            [datalevin.core :as d]
            [knot.db :as db]
            [knot.latex :as latex]
            [knot.ui :as ui])
  (:import (java.io ByteArrayOutputStream)
           (java.net URLEncoder)
           (java.nio.charset StandardCharsets)
           (java.time LocalDateTime)
           (java.time.format DateTimeFormatter)
           (java.util.zip ZipEntry ZipOutputStream)))

(defn zip-bytes
  "{路徑 字串內容} → zip 的 bytes（檔名與內容皆為 UTF-8，依路徑排序）。"
  [files]
  (let [out (ByteArrayOutputStream.)]
    (with-open [zos (ZipOutputStream. out StandardCharsets/UTF_8)]
      (doseq [[path content] (sort-by key files)]
        (.putNextEntry zos (ZipEntry. ^String path))
        (.write zos (.getBytes ^String content StandardCharsets/UTF_8))
        (.closeEntry zos)))
    (.toByteArray out)))

(defn export-latex
  "匯出目前報告：{:title 報告名稱 :dir 清理後的名稱 :files {路徑 內容} :bytes zip}。"
  [conn]
  (let [state (ui/from-server (db/load-state (d/db conn)))
        title (get-in state [:report :report/title])
        files (latex/export-files state {:exported-at (.format (LocalDateTime/now)
                                                               (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))})]
    {:title title
     :dir (latex/directory-name title)
     :files files
     :bytes (zip-bytes files)}))

(defn content-disposition
  "下載標頭：ASCII 後備檔名 report.zip，加上 filename*=UTF-8'' 的中文檔名「<報告名稱>.zip」。"
  [dir]
  (str "attachment; filename=\"report.zip\"; filename*=UTF-8''"
       (-> (URLEncoder/encode (str dir ".zip") "UTF-8") (.replace "+" "%20"))))

(defn write-zip!
  "把匯出寫到 <報告目錄>/export/<清理後名稱>.zip，回傳 {:path 絕對路徑 :files [zip 內路徑...]}。"
  [conn report-dir]
  (let [{:keys [dir files bytes]} (export-latex conn)
        f (io/file report-dir "export" (str dir ".zip"))]
    (io/make-parents f)
    (with-open [o (io/output-stream f)] (.write o ^bytes bytes))
    {:path (.getCanonicalPath f) :files (vec (sort (keys files)))}))
