(ns knot.latex-golden-test
  "LaTeX 匯出的 golden file 測試（SPEC 5.9）：fixture 報告的每個輸出檔逐字比對 test-resources/golden/latex/。
  內容有意改變時，在 REPL 執行 (knot.latex-golden-test/regenerate!) 重寫 golden 檔，再用 git diff 確認。"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [knot.latex :as latex]
            [knot.export :as export]
            [knot.ui :as ui])
  (:import (java.io ByteArrayInputStream)
           (java.nio.charset StandardCharsets)
           (java.util.zip ZipInputStream)))

(def ^:private golden-dir "test-resources/golden/latex")
(def ^:private dir-name "OO 市 智慧&路燈_計畫 100%")

(defn- fixture-state []
  (ui/init-state (edn/read-string (slurp (io/resource "fixtures/reports/latex-report.edn")))))

(defn- exported []
  (latex/export-files (fixture-state) {:exported-at "2026-11-19 10:00"}))

(defn- relative [files]
  (into {} (for [[k v] files] [(subs k (inc (count dir-name))) v])))

(defn regenerate!
  "重寫 golden 檔（先清掉舊的）。"
  []
  (doseq [f (reverse (file-seq (io/file golden-dir))) :when (.isFile f)] (.delete f))
  (doseq [[rel content] (relative (exported))
          :let [f (io/file golden-dir rel)]]
    (io/make-parents f)
    (spit f content)))

(deftest golden-test
  (let [files (exported)
        rel (relative files)
        golden (into {} (for [f (file-seq (io/file golden-dir)) :when (.isFile f)]
                          [(str/replace (.getPath f) (str golden-dir "/") "") (slurp f)]))]
    (is (every? #(str/starts-with? % (str dir-name "/")) (keys files)) "所有檔案都在報告目錄之下")
    (is (= (set (keys golden)) (set (keys rel))) "檔案清單相同")
    (doseq [[path content] rel]
      (testing path
        (is (= (get golden path) content))))))

(defn unzip
  "zip bytes → {路徑 內容}"
  [bytes]
  (with-open [zis (ZipInputStream. (ByteArrayInputStream. bytes) StandardCharsets/UTF_8)]
    (loop [acc {}]
      (if-let [e (.getNextEntry zis)]
        (recur (assoc acc (.getName e) (String. (.readAllBytes zis) StandardCharsets/UTF_8)))
        acc))))

(deftest zip-test
  (let [files {"中文 目錄/main.tex" "主檔：你好" "中文 目錄/contents/ch01.tex" "第一章"}]
    (is (= files (unzip (export/zip-bytes files))))))

(deftest content-disposition-test
  (is (= "attachment; filename=\"report.zip\"; filename*=UTF-8''%E8%B7%AF%E7%87%88%20A%26B.zip"
         (export/content-disposition "路燈 A&B"))))
