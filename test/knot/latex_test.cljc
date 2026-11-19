(ns knot.latex-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [knot.latex :as latex]))

(deftest escape-test
  (testing "每個特殊字元"
    (is (= "\\textbackslash{}" (latex/escape "\\")))
    (is (= "\\{" (latex/escape "{")))
    (is (= "\\}" (latex/escape "}")))
    (is (= "\\$" (latex/escape "$")))
    (is (= "\\&" (latex/escape "&")))
    (is (= "\\#" (latex/escape "#")))
    (is (= "\\textasciicircum{}" (latex/escape "^")))
    (is (= "\\_" (latex/escape "_")))
    (is (= "\\%" (latex/escape "%")))
    (is (= "\\textasciitilde{}" (latex/escape "~"))))
  (testing "反斜線不會二次跳脫"
    (is (= "\\textbackslash{}\\{x\\}" (latex/escape "\\{x}")))
    (is (= "a\\textbackslash{}\\textbackslash{}b" (latex/escape "a\\\\b"))))
  (testing "全形與一般文字不動"
    (is (= "％＆＿｛｝　中文 abc" (latex/escape "％＆＿｛｝　中文 abc")))
    (is (= "" (latex/escape nil)))))

(deftest directory-name-test
  (is (= "OO 計畫" (latex/directory-name "  OO 計畫 ")))
  (is (= "a_b_c_d_e_f_g_h_i" (latex/directory-name "a/b\\c:d*e?f\"g<h>i")))
  (is (= "x_y" (latex/directory-name "x|y")))
  (is (= "a_b" (latex/directory-name (str "a" (char 1) "b"))))
  (is (= "report" (latex/directory-name "   ")))
  (is (= "report" (latex/directory-name nil)))
  (is (= "report" (latex/directory-name "..")))
  (is (= "OO 市 100%_計畫" (latex/directory-name "OO 市 100%_計畫")) "% 與 _ 不在規則內，保留"))

(defn- p [& inl] {:type "paragraph" :content (vec inl)})
(defn- t [s] {:type "text" :text s})
(defn- li [& inl] {:type "listItem" :content [(apply p inl)]})

(deftest prose->latex-test
  (let [facts {"k" {:fact/key "k" :fact/value "47" :fact/unit "%"}
               "n" {:fact/key "n" :fact/value "20" :fact/unit "站"}}
        doc {:type "doc"
             :content [(p (t "節電 ") {:type "fact" :attrs {:key "k"}} (t "、") {:type "fact" :attrs {:key "n"}}
                          (t "，見") {:type "ref" :attrs {:label "tab:x"}} (t "與") {:type "ref" :attrs {:label "fig:y"}}
                          {:type "cite" :attrs {:block "b1"}})
                       (p (t "A&B"))
                       {:type "bulletList" :content [(li (t "甲")) (li (t "乙_"))]}
                       {:type "orderedList" :content [(li (t "一")) (li (t "二"))]}]}]
    (is (= (str "節電 47\\%、20 站，見表~\\ref{tab:x}與圖~\\ref{fig:y}\n\n"
                "A\\&B\n\n"
                "\\begin{itemize}\n  \\item 甲\n  \\item 乙\\_\n\\end{itemize}\n\n"
                "\\begin{enumerate}\n  \\item 一\n  \\item 二\n\\end{enumerate}")
           (latex/prose->latex facts doc)))
    (testing "找不到的事實顯示 {key}（跳脫）；空本文為 nil"
      (is (= "\\{zz\\}" (latex/prose->latex {} {:type "doc" :content [(p {:type "fact" :attrs {:key "zz"}})]})))
      (is (nil? (latex/prose->latex {} {:type "doc" :content []})))
      (is (nil? (latex/prose->latex {} nil))))))

(deftest chapter-todo-test
  (let [state {:report {:report/title "R"}
               :facts {}
               :nodes {"c" {:node/id "c" :node/kind :heading :node/name "章" :node/order-key "a"}
                       "i" {:node/id "i" :node/kind :intro :node/parent "c" :node/order-key "a" :node/draft "line1\nline2"}
                       "p" {:node/id "p" :node/kind :paragraph :node/parent "c" :node/order-key "b" :node/title "段"}}}
        out (get (latex/export-files state {:exported-at "T"}) "R/contents/ch01.tex")]
    (is (str/includes? out "\\chapter{章}\n\n% 尚未撰寫：line1\n% line2\n\n% 段\n% 尚未撰寫\n"))))
