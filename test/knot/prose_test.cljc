(ns knot.prose-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [knot.prose :as prose]))

(defn- para [& content] {:type "paragraph" :content (vec content)})
(defn- txt [s] {:type "text" :text s})
(defn- item [& content] {:type "listItem" :content [(apply para content)]})

(deftest markup->doc-paragraphs-test
  (testing "每一行一段，空行忽略，行首尾空白去掉"
    (is (= {:type "doc" :content [(para (txt "第一段")) (para (txt "第二段"))]}
           (:doc (prose/markup->doc "  第一段  \n\n\r\n第二段\n")))))
  (testing "沒有標記時 refs 全空"
    (is (= {:facts [] :labels [] :blocks []} (:refs (prose/markup->doc "單純的一段")))))
  (testing "全部空白是錯誤"
    (is (seq (:errors (prose/markup->doc "  \n \n"))))
    (is (seq (:errors (prose/markup->doc ""))))
    (is (seq (:errors (prose/markup->doc nil))))))

(deftest markup->doc-lists-test
  (testing "三種項目符號與數字清單；原本的數字丟掉"
    (is (= [{:type "bulletList" :content [(item (txt "甲")) (item (txt "乙")) (item (txt "丙"))]}
            {:type "orderedList" :content [(item (txt "一")) (item (txt "二"))]}]
           (:content (:doc (prose/markup->doc "- 甲\n* 乙\n• 丙\n3. 一\n7. 二"))))))
  (testing "兩種清單相接時各自成一個清單；清單與段落交錯"
    (is (= ["paragraph" "bulletList" "orderedList" "paragraph" "bulletList"]
           (mapv :type (:content (:doc (prose/markup->doc "主要成果如下：\n- 甲\n1. 乙\n結尾\n- 丙")))))))
  (testing "兩段不相接的清單是兩個清單"
    (is (= 2 (count (filter #(= "bulletList" (:type %))
                            (:content (:doc (prose/markup->doc "- 甲\n說明\n- 乙")))))))))

(deftest markup->doc-markers-test
  (let [{:keys [doc refs]} (prose/markup->doc "節電率為{{fact:saving-rate.site-a}}，見{{ref:tab:field-results}}。\n- 依據 [[cite:b-1]]\n再用一次{{fact:saving-rate.site-a}}")]
    (is (= (para (txt "節電率為") {:type "fact" :attrs {:key "saving-rate.site-a"}} (txt "，見")
                 {:type "ref" :attrs {:label "tab:field-results"}} (txt "。"))
           (first (:content doc))))
    (is (= {:type "bulletList" :content [(item (txt "依據 ") {:type "cite" :attrs {:block "b-1"}})]}
           (second (:content doc))))
    (testing "refs 依出現順序、去重"
      (is (= {:facts ["saving-rate.site-a"] :labels ["tab:field-results"] :blocks ["b-1"]} refs))))
  (testing "標記之間的空白保留，不產生空字串 text"
    (is (= (para {:type "fact" :attrs {:key "a"}} (txt " ") {:type "fact" :attrs {:key "b"}})
           (first (:content (:doc (prose/markup->doc "{{fact:a}} {{fact:b}}")))))))
  (testing "錯誤標記一次全部列出，並指出是哪一個"
    (let [errors (:errors (prose/markup->doc "{{foo:x}} 與 {{fact:}} 與 [[x:1]] 與 [[cite:]] 與 {{ref}}"))]
      (is (= 5 (count errors)))
      (is (some #(str/includes? % "{{foo:x}}") errors))
      (is (some #(str/includes? % "{{fact:}}") errors))
      (is (some #(str/includes? % "[[x:1]]") errors))))
  (testing "沒有結尾的標記"
    (is (seq (:errors (prose/markup->doc "壞掉的 {{fact:a"))))
    (is (seq (:errors (prose/markup->doc "壞掉的 [[cite:a"))))))

(deftest round-trip-test
  (doseq [s ["一段"
             "一段\n兩段"
             "主要成果如下：\n- 甲\n- 乙\n結尾"
             "- 甲\n1. 一\n2. 二\n- 乙"
             "節電率{{fact:k.a}}，見{{ref:tab:x}}[[cite:b1]]。\n1. {{fact:k.b}}"]]
    (is (= s (prose/doc->markup (:doc (prose/markup->doc s)))) s))
  (testing "不正規的輸入正規化後相等"
    (is (= "甲\n- 乙\n1. 一"
           (prose/doc->markup (:doc (prose/markup->doc " 甲 \n\n* 乙\n9. 一")))))))

(deftest doc->text-test
  (let [facts {"k.a" {:fact/value "47" :fact/unit "%"}}
        doc (:doc (prose/markup->doc "節電率{{fact:k.a}}，{{fact:missing}}，見{{ref:tab:x}}[[cite:b1]]。\n- 甲\n- 乙\n1. 一\n2. 二"))]
    (is (= "節電率47%，{missing}，見{tab:x}。\n- 甲\n- 乙\n1. 一\n2. 二" (prose/doc->text facts doc)))
    (testing "空段落不輸出"
      (is (= "" (prose/doc->text {} {:type "doc" :content [{:type "paragraph"}]}))))))

(def ^:private facts {"k" {:fact/key "k" :fact/value "1"}})

(defn- unbound [text]
  (prose/unbound-numbers facts (:doc (prose/markup->doc text))))

(deftest unbound-numbers-test
  (testing "半形、千分位、小數、百分比，依出現順序去重，去掉結尾標點"
    (is (= ["2,871" "95.7%"] (unbound "月平均騎乘 2,871 人次，達成率 95.7%，114 年")))
    (is (= ["3" "12.5"] (unbound "共 3 項，約 12.5。再 3 次。"))))
  (testing "全形數字與全形百分比"
    (is (= ["１２３" "４５％"] (unbound "共１２３人，占４５％。"))))
  (testing "標記內的數字不算（fact、ref、cite 的 attrs）"
    (is (= ["5"] (unbound "{{fact:site-2.rate}}、{{ref:tab:2024}}、[[cite:b3]] 共 5 項"))))
  (testing "沒有數字時為空"
    (is (= [] (unbound "沒有數字\n- 清單"))))
  (testing "清單中的數字也算"
    (is (= ["7"] (unbound "- 新增 7 處")))))

(deftest unbound-numbers-dates-test
  (testing "年月日不算，數字與字之間可有空白，民國與西元一樣"
    (is (= [] (unbound "114 年 3 月 15 日")))
    (is (= [] (unbound "114年3月15日")))
    (is (= [] (unbound "2025 年 12月 1 日")))
    (is (= ["65"] (unbound "114 年有 65 歲以上")))
    (is (= ["3"] (unbound "114 年 3 站，3 月"))))
  (testing "年月日之外的單位仍算數字"
    (is (= ["65"] (unbound "65 歲")))
    (is (= ["3"] (unbound "3 站")))))

(deftest unbound-numbers-section-numbers-test
  (testing "三段以上以句點分隔的是章節編號"
    (is (= [] (unbound "2.1.1")))
    (is (= [] (unbound "見 2.1.1。")))
    (is (= [] (unbound "見 3.1.2.1 與 1.2.3")))
    (is (= [] (unbound "全形的２．１．１"))))
  (testing "兩段的數字：前面緊接「第」或後面緊接節章項款才算編號"
    (is (= [] (unbound "第 2.1 節")))
    (is (= [] (unbound "第2.1.1節")))
    (is (= [] (unbound "見 3.2 章")))
    (is (= [] (unbound "第 3.2 項")))
    (is (= [] (unbound "參照第 3.2")))
    (is (= ["3.2"] (unbound "平均 3.2 人")))
    (is (= ["2.5"] (unbound "約 2.5 倍"))))
  (testing "一般的數字不受影響"
    (is (= ["95.7%"] (unbound "95.7%")))
    (is (= ["2,871"] (unbound "2,871 人次")))
    (is (= ["3"] (unbound "共 3 項設備")))))

(deftest unbound-numbers-no-facts-test
  (let [doc (:doc (prose/markup->doc "月平均騎乘 2,871 人次"))]
    (testing "報告沒有任何事實時不提醒"
      (is (= [] (prose/unbound-numbers {} doc)))
      (is (= [] (prose/unbound-numbers nil doc))))
    (testing "有事實時照常"
      (is (= ["2,871"] (prose/unbound-numbers facts doc))))))

(deftest fact-display-in-text-test
  (let [facts {"n" {:fact/value "3760" :fact/unit "人次"}}
        doc (:doc (prose/markup->doc "共{{fact:n}}。"))]
    (is (= "共3,760 人次。" (prose/doc->text facts doc)) "顯示加千分位與空格，儲存的值不變")
    (is (= 9 (prose/char-count facts doc)) "字數以顯示文字為準，空白不算：共3,760人次。")))

(deftest char-count-test
  (let [facts {"k.a" {:fact/value "47" :fact/unit "%"}}
        count* #(prose/char-count facts (:doc (prose/markup->doc %)))]
    (testing "空白（半形、全形、換行）不算"
      (is (= 4 (count* "你 好　世\n界")) "只算 4 個字（換行是分段）")
      (is (= 6 (count* "你好\n世界嗎？"))))
    (testing "清單的編號與符號不算"
      (is (= 4 (count* "- 甲乙\n- 丙丁")))
      (is (= 4 (count* "1. 甲乙\n2. 丙丁")))
      (is (= 5 (count* "前言\n- 甲乙\n1. 丙"))))
    (testing "一般段落開頭的「- 」不是清單時照算（不會被誤去掉）"
      (is (= 2 (prose/char-count {} {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "- 甲"}]}]})) "符號算、空白不算"))
    (testing "事實以代入的值與單位計算；查不到的事實以「{key}」計；ref 以「{label}」計；cite 不算"
      (is (= (count "節電率47%。") (count* "節電率{{fact:k.a}}。")))
      (is (= (count "{x.y}") (count* "{{fact:x.y}}")))
      (is (= (count "見{tab:t}") (count* "見{{ref:tab:t}}[[cite:b1]]"))))
    (testing "空 doc、nil、空段落為 0"
      (is (= 0 (prose/char-count facts nil)))
      (is (= 0 (prose/char-count facts {:type "doc" :content []})))
      (is (= 0 (prose/char-count facts {:type "doc" :content [{:type "paragraph"}]}))))))
