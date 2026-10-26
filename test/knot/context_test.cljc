(ns knot.context-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.context :as context]
            [knot.ui :as ui]))

(def state
  (ui/init-state
   {:report/style-name "機關"
    :tree [{:node/id "A" :node/kind :heading :node/name "甲" :node/draft "章的意圖"
            :children [{:node/id "iA" :node/kind :intro :node/draft "引言意圖"}
                       {:node/id "A1" :node/kind :heading :node/name "甲一" :node/draft "節的意圖"
                        :children [{:node/id "a1" :node/kind :paragraph :node/title "一" :node/draft "前段意圖"
                                    :node/prose {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "很長的本文"}]}]}}
                                   {:node/id "a2" :node/kind :paragraph :node/title "二" :node/draft "本段意圖"
                                    :node/citations [{:block "b1"}]
                                    :children [{:node/id "t1" :node/kind :table :node/title "表一" :node/label "tab:x" :node/draft "表的意圖"}]}
                                   {:node/id "a3" :node/kind :paragraph :node/title "三" :node/draft "後段意圖"}]}]}]
    :relations [{:relation/from "a1" :relation/to "a2" :relation/type :causal}
                {:relation/from "a2" :relation/to "a3" :relation/type :progression :relation/pending true}]
    :blocks [{:block/id "b1" :block/kind :para :block/status :candidate :block/title "素材" :block/text "素材內容"}]
    :facts [{:fact/key "k" :fact/label "事實" :fact/value "1" :fact/unit "%"}]}))

(defn- section [ctx title]
  (some #(when (= title (:title %)) %) (:sections ctx)))

(deftest paragraph-context-test
  (let [ctx (context/generation-context state "a2")]
    (testing "章節路徑帶各層標題的意圖"
      (is (= [["第一章 甲" "章的意圖"] ["1.1 甲一" "節的意圖"]]
             (mapv (juxt :label :text) (:entries (section ctx "章節路徑"))))))
    (testing "前後段只帶意圖與關係，不帶全文"
      (is (= {:label "一" :text "前段意圖" :relation "因果"}
             (first (:entries (section ctx "前一段")))))
      (is (= "遞進，待確認" (:relation (first (:entries (section ctx "下一段"))))))
      (is (not (re-find #"很長的本文" (pr-str ctx)))))
    (testing "依據標示未確認素材"
      (is (:warn (first (:entries (section ctx "依據"))))))
    (testing "下層表格帶 label 與意圖"
      (is (= "表的意圖" (:text (first (:entries (section ctx "下層表格與圖")))))))
    (is (pos? (:chars ctx)))))

(deftest intro-context-test
  (let [ctx (context/generation-context state "iA")]
    (is (= ["引言意圖"] (mapv :text (:entries (section ctx "引言的意圖")))))
    (is (= ["節的意圖"] (mapv :text (:entries (section ctx "這一層的下層內容")))))))

(deftest other-kinds-test
  (is (nil? (context/generation-context state "A")))
  (is (nil? (context/generation-context state "t1"))))

(deftest remote-relation-context-test
  (let [s (ui/add-remote-relation state "a3" "a1" :basis)
        ctx (context/generation-context s "a3")]
    (is (= [["一" "前段意圖"]] (mapv (juxt :label :text) (:entries (section ctx "本段依據的段落")))))
    (is (nil? (section ctx "本段呼應的段落")) "沒有的類型不列出")))
