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
    :facts [{:fact/key "k" :fact/label "事實" :fact/value "1" :fact/unit "%" :fact/block "b1"}
            {:fact/key "other" :fact/label "無關的事實" :fact/value "2" :fact/unit "" :fact/block "b9"}]}))

(defn- section [ctx title]
  (some #(when (= title (:title %)) %) (:sections ctx)))

(deftest paragraph-context-test
  (let [ctx (context/generation-context state "a2")]
    (testing "章節路徑帶各層標題的意圖"
      (is (= [["第一章 甲" "章的意圖"] ["1.1 甲一" "節的意圖"]]
             (mapv (juxt :label :text) (:entries (section ctx "章節路徑"))))))
    (testing "所屬標題沒有引言時不列出"
      (is (nil? (section ctx "所屬標題的引言"))))
    (testing "鄰段有本文就帶本文，沒有就帶意圖"
      (is (= {:label "一" :text "很長的本文" :source :prose :relation "因果"}
             (select-keys (first (:entries (section ctx "前一段"))) [:label :text :source :relation])))
      (is (= {:text "後段意圖" :source :intent :relation "遞進，待確認"}
             (select-keys (first (:entries (section ctx "下一段"))) [:text :source :relation]))))
    (testing "只帶引用素材上的事實"
      (is (= ["k"] (mapv :key (:entries (section ctx "可用事實"))))))
    (testing "每一節都有分組"
      (is (every? :group (:sections ctx))))
    (testing "依據標示未確認素材"
      (is (:warn (first (:entries (section ctx "依據"))))))
    (testing "下層表格帶 label 與意圖"
      (is (= "表的意圖" (:text (first (:entries (section ctx "下層表格與圖")))))))
    (is (pos? (:chars ctx)))))

(deftest intro-context-test
  (let [ctx (context/generation-context state "iA")]
    (is (= ["引言意圖"] (mapv :text (:entries (section ctx "引言的意圖")))))
    (is (= ["節的意圖"] (mapv :text (:entries (section ctx "這一層的下層內容")))))))

(deftest mixed-structure-context-test
  (let [s (ui/add-child state "A" :paragraph)           ; 接在引言後面、下層標題 1.1 之前
        pid (get-in s [:ui :selected])
        s (update-in s [:nodes pid] assoc :node/title "章的段落" :node/draft "章段意圖")
        ctx (context/generation-context s pid)]
    (testing "段落的後一段只取緊接的段落：後面是下層標題時沒有"
      (is (nil? (section ctx "下一段")))
      (is (nil? (section ctx "前一段"))))
    (testing "下層標題的第一段沒有前一段，不會取到上層的段落"
      (is (nil? (section (context/generation-context s "a1") "前一段"))))
    (testing "引言的下層內容包含這一層直接的段落與下層標題"
      (let [ictx (context/generation-context s "iA")]
        (is (= [["章的段落" "章段意圖"] ["1.1 甲一" "節的意圖"]]
               (mapv (juxt :label :text) (:entries (section ictx "這一層的下層內容")))))))))

(deftest parent-intro-test
  (let [s (assoc-in state [:nodes "iA" :node/prose]
                    {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "引言本文"}]}]})
        s (update s :nodes assoc "a0" {:node/id "a0" :node/kind :paragraph :node/parent "A"
                                       :node/order-key "z" :node/title "零" :node/draft "x"})
        ctx (context/generation-context s "a0")]
    (is (= [{:label "引言" :text "引言本文" :source :prose}]
           (:entries (section ctx "所屬標題的引言")))
        "所屬標題的引言有本文時帶本文")))

(deftest other-kinds-test
  (is (nil? (context/generation-context state "A")))
  (is (nil? (context/generation-context state "t1"))))

(deftest remote-relation-context-test
  (let [s (-> state
              (ui/add-remote-relation "a3" "a1" :basis)
              (ui/add-remote-relation "a3" "a1" :echo))
        ctx (context/generation-context s "a3")]
    (is (= [["一" "很長的本文"]] (mapv (juxt :label :text) (:entries (section ctx "本段依據的段落"))))
        "依據帶目標段落的本文")
    (is (= [["一" "前段意圖"]] (mapv (juxt :label :text) (:entries (section ctx "本段呼應的段落"))))
        "呼應只帶意圖")
    (is (nil? (section ctx "本段對比的段落")) "沒有的類型不列出")))

(deftest prose-text-test
  (is (= "保留 31,405 筆，見{tab:x}。\n第二段"
         (context/prose-text {"k" {:fact/value "31,405" :fact/unit ""}}
                             {:type "doc"
                              :content [{:type "paragraph"
                                         :content [{:type "text" :text "保留 "}
                                                   {:type "fact" :attrs {:key "k"}}
                                                   {:type "text" :text " 筆，見"}
                                                   {:type "ref" :attrs {:label "tab:x"}}
                                                   {:type "text" :text "。"}]}
                                        {:type "paragraph" :content [{:type "text" :text "第二段"}]}]}))))
