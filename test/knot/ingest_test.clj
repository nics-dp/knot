(ns knot.ingest-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [knot.ingest :as ingest]))

(def blocks
  (ingest/markdown->blocks (slurp (io/resource "fixtures/sources/streetlight-final.md"))
                           {:source "streetlight-final.md" :id-prefix "s"}))

(deftest markdown->blocks-test
  (testing "段落與表格各成一個區塊，一律是候選"
    (is (= [:para :para :para :table :para] (mapv :block/kind blocks)))
    (is (every? #(= :candidate (:block/status %)) blocks))
    (is (= ["s-0" "s-1" "s-2" "s-3" "s-4"] (mapv :block/id blocks))))
  (testing "標題成為區塊標題，頁碼標記成為位置"
    (is (= "節電成效" (:block/title (nth blocks 3))))
    (is (= "p.2 · 節電成效" (:block/location (nth blocks 3))))
    (is (= "p.3 · 故障通報" (:block/location (nth blocks 4)))))
  (testing "表格欄位與列"
    (let [t (nth blocks 3)]
      (is (= ["場域" "路燈數" "汰換前用電 (kWh)" "汰換後用電 (kWh)" "節電率"] (:table/columns t)))
      (is (= ["A 區" "700" "182000" "96460" "47%"] (first (:table/rows t))))
      (is (= 2 (count (:table/rows t)))))))

(deftest noise-test
  (is (empty? (ingest/markdown->blocks "# 標題\n\n12\n\n- 頁 3 -\n" {:source "x" :id-prefix "x"}))
      "過短的段落（頁碼、頁首）不成為區塊"))
