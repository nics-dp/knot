(ns knot.sources-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.sources :as sources]))

(def para {:block/id "p" :block/kind :para :block/title "節電成效" :block/source "期末報告.pdf"
           :block/text "三處場域的平均節電率達 47%，全年用電 2,871 kWh。"})

(def table {:block/id "t" :block/kind :table :block/title "各場域量測結果" :block/source "量測紀錄.xlsx"
            :table/columns ["場域" "節電率" "斷線次數"]
            :table/rows [["場域 A" "47%" "2"] ["場域 B" "40%" "3"]]})

(deftest normalize-test
  (is (= "abc123" (sources/normalize " a b\tc\n１２３ ")) "全形英數轉半形、去掉空白")
  (is (= "節電率47%" (sources/normalize "節電率 ４７％")) "全形百分比")
  (is (= "ABC" (sources/normalize "ＡＢＣ")))
  (is (= "ab" (sources/normalize "a　b")) "全形空白也去掉")
  (is (= (sources/normalize "2,871") (sources/normalize "2871")) "忽略數字的千分位逗號")
  (is (= "a,b1,23" (sources/normalize "a, b 1,23")) "不是三位一組的逗號不動")
  (is (= "" (sources/normalize nil))))

(deftest fact-key-test
  (is (every? sources/fact-key-valid? ["a" "saving-rate.site-a" "ridership.2025-11" "x1" "a-b.c"]))
  (is (not-any? sources/fact-key-valid? ["" "A" "a_b" ".a" "a." "-a" "a--b" "a..b" "a b" "節電" nil 1 "a.-b"])))

(deftest fact-value-para-test
  (testing "節錄：值出現在原文"
    (is (nil? (sources/fact-value-error para {:fact/value "47"})))
    (is (nil? (sources/fact-value-error para {:fact/value "2,871"})))
    (is (nil? (sources/fact-value-error para {:fact/value "２，８７１"})) "全形比對")
    (is (nil? (sources/fact-value-error para {:fact/value "平均 節電率"})) "忽略空白"))
  (testing "值與單位分開時只檢查值"
    (is (nil? (sources/fact-value-error para {:fact/value "47" :fact/unit "%"})))
    (is (nil? (sources/fact-value-error para {:fact/value "47" :fact/unit "不在原文的單位"}))))
  (testing "不在原文：回傳可讀的原因"
    (let [e (sources/fact-value-error para {:fact/value "2872"})]
      (is (re-find #"2872" e))
      (is (re-find #"節電成效" e) "指出是哪筆素材")
      (is (re-find #"三處場域" e) "附原文開頭"))
    (is (re-find #"空白" (sources/fact-value-error para {:fact/value " "}))))
  (testing "節錄不能指定列與欄"
    (is (re-find #"節錄" (sources/fact-value-error para {:fact/value "47" :fact/row 0 :fact/column "節電率"})))))

(deftest fact-value-table-test
  (testing "整張表"
    (is (nil? (sources/fact-value-error table {:fact/value "40"})))
    (is (nil? (sources/fact-value-error table {:fact/value "節電率"})) "欄名也算")
    (is (re-find #"沒有出現" (sources/fact-value-error table {:fact/value "99"}))))
  (testing "指定儲存格"
    (is (nil? (sources/fact-value-error table {:fact/value "40" :fact/row 1 :fact/column "節電率"})))
    (let [e (sources/fact-value-error table {:fact/value "47" :fact/row 1 :fact/column "節電率"})]
      (is (re-find #"40%" e) "指出儲存格實際內容")
      (is (re-find #"節電率" e)))
    (is (re-find #"超出範圍" (sources/fact-value-error table {:fact/value "47" :fact/row 2 :fact/column "節電率"})))
    (is (re-find #"沒有欄「電」" (sources/fact-value-error table {:fact/value "47" :fact/row 0 :fact/column "電"})))
    (is (re-find #"同時給" (sources/fact-value-error table {:fact/value "47" :fact/row 0})))
    (is (re-find #"同時給" (sources/fact-value-error table {:fact/value "47" :fact/column "節電率"})))))

(deftest fact-value-thousands-test
  (let [plain {:block/kind :para :block/title "甲" :block/text "共 1210 人次，另有 7 筆。"}
        comma {:block/kind :para :block/title "乙" :block/text "共 1,210 人次，另有 7 筆。"}
        ok? #(nil? (sources/fact-value-error %1 %2))]
    (is (ok? comma {:fact/value "1210"}) "值沒有逗號、原文有")
    (is (ok? plain {:fact/value "1,210"}) "值有逗號、原文沒有")
    (is (ok? comma {:fact/value "1,210"}))
    (is (ok? plain {:fact/value "1210"}))
    (is (not (ok? plain {:fact/value "12"})) "12 仍不算出現在 1210 中")
    (is (not (ok? comma {:fact/value "210"})))))

(deftest fact-display-test
  (let [d #(sources/fact-display {:fact/value %1 :fact/unit %2})]
    (is (= "3,760 人次" (d "3760" "人次")))
    (is (= "-3,760" (d "-3760" nil)))
    (is (= "2,871 盞" (d "2,871" "盞")) "已有千分位不動")
    (is (= "95.8%" (d "95.8" "%")))
    (is (= "78.4%" (d "78.4" "%")))
    (is (= "20 站" (d "20" "站")))
    (is (= "114 年" (d "114 年" nil)))
    (is (= "1,234.5kWh" (d "1234.5" "kWh")) "小數的整數部分加千分位；非中日韓的單位緊接")
    (is (= "36.5℃" (d "36.5" "℃")))
    (is (= "999" (d "999" "")) "空單位視為沒有")
    (is (= "1,234,567" (d 1234567 nil)))
    (is (= "2026-10-29" (d "2026-10-29" nil)) "日期不動")
    (is (= "0123" (d "0123" nil)) "前導零（編號、代碼）照原樣")))

(deftest fact-value-number-boundary-test
  (let [t {:block/kind :table :block/title "營運" :table/columns ["月份" "騎乘人次"]
           :table/rows [["4 月" "1210"] ["5 月" "980"]]}
        p {:block/kind :para :block/title "摘要" :block/text "會員 1,210 人，達成率 3.12 倍，12 月最多，目標 95% 以上。"}
        ok? #(nil? (sources/fact-value-error %1 %2))]
    (testing "數字不能是更長數字的一部分"
      (is (not (ok? t {:fact/value "12" :fact/row 0 :fact/column "騎乘人次"})) "1210 的前兩位")
      (is (not (ok? t {:fact/value "121"})))
      (is (not (ok? p {:fact/value "210"})) "1,210 的後三位")
      (is (not (ok? p {:fact/value "1"})))
      (is (not (ok? p {:fact/value "3.1"}))))
    (testing "完整的數字可以"
      (is (ok? t {:fact/value "1210" :fact/row 0 :fact/column "騎乘人次"}))
      (is (ok? t {:fact/value "980"}) "整張表逐格比對，相鄰儲存格不會接在一起")
      (is (ok? p {:fact/value "1,210"}))
      (is (ok? p {:fact/value "3.12"}))
      (is (ok? p {:fact/value "12"}) "12 月")
      (is (ok? p {:fact/value "95%"}))
      (is (ok? p {:fact/value "95"})))))

(def blocks
  [{:block/id "1" :block/kind :para :block/title "計畫目標" :block/source "契約書.pdf"
    :block/text "本計畫旨在建置智慧路燈，降低公共照明用電。"}
   {:block/id "2" :block/kind :para :block/title "委員意見" :block/source "審查會議紀錄.docx"
    :block/text "委員建議補充節電率的量測方法。"}
   {:block/id "3" :block/kind :table :block/title "量測結果" :block/source "量測紀錄.xlsx"
    :table/columns ["場域" "節電率"] :table/rows [["Ａ區" "47%"]]}
   {:block/id "4" :block/kind :para :block/title "節電成效" :block/source "量測紀錄.xlsx"
    :block/text "節電成效良好。" :block/status :discarded}])

(defn- ids [bs] (mapv :block/id bs))

(deftest fact-value-date-spacing-test
  (let [p {:block/kind :para :block/title "期程" :block/text "期程：2026 年 6 月 1 日至 8 月 23 日，共 12 週。量測 12 34 次。"}
        ok? #(nil? (sources/fact-value-error p {:fact/value %}))]
    (testing "以數字開頭的值：數字與文字之間的空白不影響比對"
      (is (ok? "2026 年 6 月 1 日"))
      (is (ok? "2026年6月1日"))
      (is (ok? "8 月 23 日"))
      (is (ok? "12 週")))
    (testing "兩個數字之間的空白仍然保留，不會接成一個數字"
      (is (ok? "12 34"))
      (is (not (ok? "1234"))))
    (testing "邊界規則不變"
      (is (not (ok? "2")))
      (is (not (ok? "26"))))))

(deftest search-test
  (testing "空字串列出全部（預設排除已捨棄），維持原順序"
    (is (= ["1" "2" "3"] (ids (sources/search blocks "" {}))))
    (is (= ["1" "2" "3"] (ids (sources/search blocks "   " {})))))
  (testing "單一詞：原文、名稱、來源檔名、儲存格都找得到"
    (is (= ["2" "3"] (ids (sources/search blocks "節電" {}))) "節電率在原文與欄名")
    (is (= ["1"] (ids (sources/search blocks "契約書" {}))) "來源檔名")
    (is (= ["3"] (ids (sources/search blocks "47%" {}))) "儲存格")
    (is (= ["3"] (ids (sources/search blocks "A區" {}))) "全形與半形視為相同")
    (is (= ["3"] (ids (sources/search blocks "量測 結果" {}))) "詞之間的空白"))
  (testing "多個詞全部都要出現"
    (is (= ["2"] (ids (sources/search blocks "委員 節電率" {}))))
    (is (= [] (ids (sources/search blocks "委員 契約書" {}))))
    (is (= ["1"] (ids (sources/search blocks "計畫　用電" {}))) "全形空白也能分詞"))
  (testing "不分大小寫"
    (is (= ["3"] (ids (sources/search blocks "xlsx" {}))))
    (is (= ["3"] (ids (sources/search blocks "XLSX" {})))))
  (testing "篩選種類與來源"
    (is (= ["3"] (ids (sources/search blocks "" {:kind :table}))))
    (is (= ["1" "2"] (ids (sources/search blocks "" {:kind :para}))))
    (is (= ["3"] (ids (sources/search blocks "" {:source "量測紀錄.xlsx"}))) "已捨棄的不在內")
    (is (= ["3" "4"] (ids (sources/search blocks "" {:source "量測紀錄.xlsx" :include-discarded? true})))))
  (testing "已捨棄的預設搜尋不到"
    (is (= [] (ids (sources/search blocks "成效" {}))))
    (is (= ["4"] (ids (sources/search blocks "成效" {:include-discarded? true})))))
  (testing "相關度：詞出現在名稱的排前面，同分維持原順序"
    (let [bs [{:block/id "a" :block/kind :para :block/title "其他" :block/text "提到節電" :block/source "x"}
              {:block/id "b" :block/kind :para :block/title "節電成效" :block/text "內文" :block/source "x"}
              {:block/id "c" :block/kind :para :block/title "另一段" :block/text "也提到節電" :block/source "x"}]]
      (is (= ["b" "a" "c"] (ids (sources/search bs "節電" {})))))))

(deftest source-counts-test
  (is (= [{:source "契約書.pdf" :count 1} {:source "審查會議紀錄.docx" :count 1} {:source "量測紀錄.xlsx" :count 1}]
         (sources/source-counts blocks))
      "不含已捨棄"))
