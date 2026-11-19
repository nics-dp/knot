(ns knot.ops-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [knot.ops :as ops]
            [knot.tree :as tree]))

(defn- h [id name & children] (cond-> {:node/id id :node/kind :heading :node/name name}
                                (seq children) (assoc :children (vec children))))
(defn- p [id & {:as m}] (merge {:node/id id :node/kind :paragraph :node/title id :node/status :none :node/version 1} m))
(defn- intro [id] {:node/id id :node/kind :intro})

;; 第一章 甲：引言 iA；1.1 甲一（段落 a1 a2 a3）；1.2 甲二 → 1.2.1 → 1.2.1.1（段落 x1）
;; 第二章 乙：段落 b1、b2（人工改過）
(defn- state []
  {:nodes (tree/from-nested
           [(h "A" "甲"
               (intro "iA")
               (h "A1" "甲一" (p "a1") (p "a2") (p "a3"))
               (h "A2" "甲二" (h "A21" "甲二一" (h "A211" "甲二一一" (p "x1")))))
            (h "B" "乙" (p "b1") (p "b2" :node/status :human))])
   :relations [{:relation/from "a1" :relation/to "a2" :relation/type :progression :relation/pending false}
               {:relation/from "a2" :relation/to "a3" :relation/type :causal :relation/pending false}]})

(defn- titles [{:keys [nodes]} parent]
  (mapv #(or (:node/name %) (:node/title %) "引言") (tree/children nodes parent)))

(defn- error-of [r] (:error r))

(deftest add-card-test
  (let [s (state)]
    (testing "新增一章：接在最後"
      (let [r (ops/add-card s {:kind :heading :parent-id nil :title "丙" :intent "結論"})
            id (first (:created r))]
        (is (= ["甲" "乙" "丙"] (titles (:state r) nil)))
        (is (= {:node/kind :heading :node/name "丙" :node/draft "結論" :node/parent nil}
               (select-keys (get-in r [:state :nodes id]) [:node/kind :node/name :node/draft :node/parent])))
        (is (= :manual (:origin r)))
        (is (= "新增標題「丙」" (:label r)))
        (is (= [id] (:touched r)))))
    (testing "段落：預設放最後、狀態 none；after_id 指定位置"
      (let [r (ops/add-card s {:kind :paragraph :parent-id "A1" :title "新" :after-id "a1"})
            id (first (:created r))]
        (is (= ["a1" "新" "a2" "a3"] (titles (:state r) "A1")))
        (is (= :none (get-in r [:state :nodes id :node/status])))
        (is (= "" (get-in r [:state :nodes id :node/draft]))))
      (is (= ["a1" "a2" "a3" "新"]
             (titles (:state (ops/add-card s {:kind :paragraph :parent-id "A1" :title "新"})) "A1"))))
    (testing "插入段落後，後面段落與新段落的關係標為待確認（同畫面）"
      (let [r (ops/add-card s {:kind :paragraph :parent-id "A1" :title "新" :after-id "a1"})
            id (first (:created r))
            rels (:relations (:state r))]
        (is (= {:relation/type nil :relation/pending true}
               (select-keys (tree/relation-between rels id "a2") [:relation/type :relation/pending]))
            "新段落 → a2：待確認")
        (is (nil? (tree/relation-between rels "a1" "a2")) "a1 → a2 原關係移除")
        (is (= :causal (:relation/type (tree/relation-between rels "a2" "a3"))))))
    (testing "引言：放最前面，每個標題最多一張"
      (let [r (ops/add-card s {:kind :intro :parent-id "B" :intent "概述"})]
        (is (= ["引言" "b1" "b2"] (titles (:state r) "B")))
        (is (= "新增引言" (:label r))))
      (is (str/includes? (error-of (ops/add-card s {:kind :intro :parent-id "A"})) "已經有引言"))
      (is (str/includes? (error-of (ops/add-card s {:kind :intro :parent-id "B" :after-id "b1"})) "最前面"))
      (is (str/includes? (error-of (ops/add-card s {:kind :intro :parent-id "B" :title "x"})) "不需要 title")))
    (testing "標題底下的段落與下層標題可以並存：段落在前"
      (let [r (ops/add-card s {:kind :paragraph :parent-id "A" :title "x"})]
        (is (= ["引言" "x" "甲一" "甲二"] (titles (:state r) "A"))
            "新段落放在最後一個段落之後、下層標題之前（這裡沒有段落，就在引言之後）"))
      (let [r (ops/add-card s {:kind :heading :parent-id "A1" :title "x"})]
        (is (= ["a1" "a2" "a3" "x"] (titles (:state r) "A1")) "已有段落的標題直接新增下層標題，接在段落後面")
        (let [r2 (ops/add-card (:state r) {:kind :paragraph :parent-id "A1" :title "y"})]
          (is (= ["a1" "a2" "a3" "y" "x"] (titles (:state r2) "A1")) "再加段落：排在下層標題之前")
          (is (= ["a1" "a2" "a3" "y" "x"] (mapv #(or (:node/title %) (:node/name %))
                                                  (tree/children (:nodes (:state r2)) "A1")))
              "children 順序以 order-key 為準")))
      (is (str/includes? (error-of (ops/add-card s {:kind :paragraph :parent-id "A" :title "x" :after-id "A1"}))
                         "段落要排在下層標題之前")
          "after_id 在下層標題之後：拒絕")
      (is (str/includes? (error-of (ops/add-card s {:kind :paragraph :parent-id "A" :title "x" :after-id "A1"}))
                         "請用 position 為 into"))
      (is (str/includes? (error-of (ops/add-card s {:kind :heading :parent-id "A1" :title "x" :after-id "a1"}))
                         "下層標題要排在這一層所有段落之後")
          "標題接在中間的段落後面：拒絕")
      (is (some? (:state (ops/add-card s {:kind :heading :parent-id "A1" :title "x" :after-id "a3"}))))
      (is (some? (:state (ops/add-card s {:kind :heading :parent-id "A2" :title "x"})))
          "下層標題底下可再加同層下層標題"))
    (testing "最多四層"
      (is (str/includes? (error-of (ops/add-card (update s :nodes dissoc "x1") {:kind :heading :parent-id "A211" :title "x"}))
                         "已經是第四層，不能再新增下層標題"))
      (is (some? (:state (ops/add-card s {:kind :paragraph :parent-id "A211" :title "x"})))
          "第四層標題底下的 x1 已是段落，仍可再放段落"))
    (testing "最上層只能放標題；段落底下不能放卡片"
      (is (str/includes? (error-of (ops/add-card s {:kind :paragraph :parent-id nil :title "x"})) "最上層只能放標題"))
      (is (str/includes? (error-of (ops/add-card s {:kind :paragraph :parent-id "a1" :title "x"})) "是段落")))
    (testing "after_id 必須是 parent_id 的子卡片"
      (is (str/includes? (error-of (ops/add-card s {:kind :paragraph :parent-id "A1" :title "x" :after-id "b1"}))
                         "after_id 不是 parent_id 的子卡片"))
      (is (str/includes? (error-of (ops/add-card s {:kind :heading :parent-id nil :title "x" :after-id "A1"}))
                         "after_id 不是 parent_id 的子卡片"))
      (is (str/includes? (error-of (ops/add-card s {:kind :paragraph :parent-id "A1" :title "x" :after-id "nope"}))
                         "找不到 after_id")))
    (testing "其他錯誤"
      (is (str/includes? (error-of (ops/add-card s {:kind :bogus :parent-id "A1" :title "x"})) "kind 只接受"))
      (is (str/includes? (error-of (ops/add-card s {:kind :paragraph :parent-id "A1" :title "  "})) "需要 title"))
      (is (str/includes? (error-of (ops/add-card s {:kind :paragraph :parent-id "nope" :title "x"})) "找不到 parent_id")))))

(deftest build-outline-test
  (let [s (state)
        items [{:kind :heading :title "丙" :intent "結論章"
                :children [{:kind :intro :intent "引言"}
                           {:kind :heading :title "丙一" :children [{:kind :paragraph :title "p1"}
                                                                      {:kind :paragraph :title "p2"}]}
                           {:kind :heading :title "丙二"}]}
               {:kind :heading :title "丁"}]
        r (ops/build-outline s {:parent-id nil :items items})
        nodes (:nodes (:state r))
        numbers (tree/numbering nodes)]
    (testing "巢狀建立、依文件順序列出"
      (is (= ["甲" "乙" "丙" "丁"] (titles (:state r) nil)))
      (is (= ["引言" "丙一" "丙二"] (titles (:state r) (:node/id (nth (tree/children nodes nil) 2)))))
      (is (= ["丙" "引言" "丙一" "p1" "p2" "丙二" "丁"]
             (mapv #(or (:node/name (nodes %)) (:node/title (nodes %)) "引言") (:created r))))
      (is (= ["3" "3.1" "3.2" "4"] (mapv numbers (filter #(tree/heading? nodes %) (:created r)))))
      (is (= "建立大綱：7 張卡片" (:label r)))
      (is (= (:created r) (:touched r))))
    (testing "after_id：接在同層指定卡片後面"
      (let [r (ops/build-outline s {:parent-id "A1" :after-id "a1"
                                    :items [{:kind :paragraph :title "n1"} {:kind :paragraph :title "n2"}]})]
        (is (= ["a1" "n1" "n2" "a2" "a3"] (titles (:state r) "A1")))))
    (testing "引言寫在 items 的後面也會放到最前面"
      (let [r (ops/build-outline s {:parent-id "B" :items [{:kind :paragraph :title "b3"} {:kind :intro}]})]
        (is (= ["引言" "b1" "b2" "b3"] (titles (:state r) "B")))))
    (testing "整批驗證：錯誤指出位置，狀態不變"
      (let [bad [{:kind :heading :title "丙"}
                 {:kind :heading :title "丁"
                  :children [{:kind :heading :title "子"}
                             {:kind :paragraph :title "段落排在下層標題之後"}]}]
            r (ops/build-outline s {:parent-id nil :items bad})]
        (is (nil? (:state r)))
        (is (str/starts-with? (:error r) "items[1].children[1]："))
        (is (str/includes? (:error r) "段落要排在下層標題之前")))
      (let [r (ops/build-outline s {:parent-id nil
                                    :items [{:kind :heading :title "丙"
                                             :children [{:kind :paragraph :title "p1"}
                                                        {:kind :heading :title "子"}
                                                        {:kind :paragraph :title "p2"}]}]})]
        (is (str/starts-with? (:error r) "items[0].children[2]："))
        (is (nil? (:state r))))
      (let [r (ops/build-outline s {:parent-id nil
                                    :items [{:kind :heading :title "丙"
                                             :children [{:kind :intro}
                                                        {:kind :paragraph :title "p1"}
                                                        {:kind :paragraph :title "p2"}
                                                        {:kind :heading :title "子"}]}]})]
        (is (= ["引言" "p1" "p2" "子"]
               (titles (:state r) (:node/id (nth (tree/children (:nodes (:state r)) nil) 2))))
            "順序正確：引言、段落、下層標題"))
      (let [r (ops/build-outline s {:parent-id nil :items [{:kind :paragraph :title "x"}]})]
        (is (str/starts-with? (:error r) "items[0]：")))
      (let [r (ops/build-outline s {:parent-id "A211"
                                    :items [{:kind :paragraph :title "x"}]})]
        (is (some? (:state r))))
      (let [r (ops/build-outline s {:parent-id "A1"
                                    :items [{:kind :heading :title "x" :children [{:kind :heading :title "y"}]}]})]
        (is (some? (:state r)) "已有段落的標題可以新增下層標題，下層標題底下也可以再有下層標題"))
      (let [r (ops/build-outline s {:parent-id nil
                                    :items [{:kind :heading :title "a"
                                             :children [{:kind :heading :title "b"
                                                         :children [{:kind :heading :title "c"
                                                                     :children [{:kind :heading :title "d"
                                                                                 :children [{:kind :heading :title "e"}]}]}]}]}]})]
        (is (str/starts-with? (:error r) "items[0].children[0].children[0].children[0].children[0]："))
        (is (str/includes? (:error r) "第四層")))
      (let [r (ops/build-outline s {:parent-id "B" :items [{:kind :intro :children [{:kind :paragraph :title "y"}]}]})]
        (is (str/includes? (:error r) "不能有 children")))
      (is (= "items 不可為空" (:error (ops/build-outline s {:parent-id nil :items []}))))
      (is (= "items 必須是陣列" (:error (ops/build-outline s {:parent-id nil :items nil})))))))

(deftest update-card-test
  (let [s (state)]
    (testing "標題寫 name、段落寫 title、意圖寫 draft"
      (let [r (ops/update-card s {:id "A1" :title "新名稱" :intent "新意圖"})]
        (is (= {:node/name "新名稱" :node/draft "新意圖"}
               (select-keys (get-in r [:state :nodes "A1"]) [:node/name :node/draft])))
        (is (= ["修改「1.1 甲一」"] [(:label r)])))
      (let [r (ops/update-card s {:id "a1" :title "改過"})]
        (is (= "改過" (get-in r [:state :nodes "a1" :node/title])))
        (is (= :none (get-in r [:state :nodes "a1" :node/status])) "不影響狀態")))
    (testing "版本"
      (is (some? (:state (ops/update-card s {:id "a1" :title "x" :version 1}))))
      (is (= "卡片已被修改（目前版本 1），請先 get_card 重新讀取"
             (:error (ops/update-card s {:id "a1" :title "x" :version 0})))))
    (testing "其他錯誤"
      (is (str/includes? (:error (ops/update-card s {:id "iA" :title "x"})) "引言沒有名稱"))
      (is (some? (:state (ops/update-card s {:id "iA" :intent "引言意圖"}))))
      (is (str/includes? (:error (ops/update-card s {:id "a1"})) "沒有要修改"))
      (is (str/includes? (:error (ops/update-card s {:id "a1" :title " "})) "空白"))
      (is (str/includes? (:error (ops/update-card s {:id "nope" :title "x"})) "找不到卡片")))))

(deftest move-card-test
  (let [s (state)]
    (testing "移動段落：同一套 placement，關係標為待確認，origin 為 :move"
      (let [r (ops/move-card s {:id "a2" :target-id "a3" :position :after})
            rels (:relations (:state r))]
        (is (= ["a1" "a3" "a2"] (titles (:state r) "A1")))
        (is (= :move (:origin r)))
        (is (= "移動「a2」" (:label r)))
        (is (str/includes? (:message r) "待確認"))
        (is (= {:relation/type nil :relation/pending true}
               (select-keys (tree/relation-between rels "a3" "a2") [:relation/type :relation/pending])))
        (is (nil? (tree/relation-between rels "a2" "a3")))
        (is (every? :relation/pending (filter #(= "a2" (:relation/to %)) rels)))))
    (testing "移動標題：編號重新計算"
      (let [r (ops/move-card s {:id "A2" :target-id "A1" :position :before})]
        (is (= ["引言" "甲二" "甲一"] (titles (:state r) "A")))
        (is (str/includes? (:message r) "編號"))))
    (testing "移入有下層標題的標題：段落放在最後一個段落之後，標題放最後"
      (let [r (ops/move-card s {:id "b1" :target-id "A" :position :into})]
        (is (= ["引言" "b1" "甲一" "甲二"] (titles (:state r) "A"))))
      (let [r (ops/move-card s {:id "A2" :target-id "B" :position :into})]
        (is (= ["b1" "b2" "甲二"] (titles (:state r) "B")))))
    (testing "移入"
      (let [r (ops/move-card s {:id "b1" :target-id "A1" :position :into})]
        (is (= ["a1" "a2" "a3" "b1"] (titles (:state r) "A1")))))
    (testing "不合法時說明原因"
      (let [e (:error (ops/move-card s {:id "b1" :target-id "A1" :position :after}))]
        (is (str/includes? e "段落要排在下層標題之前") "段落不能排在下層標題之後")
        (is (str/includes? e "「1.1 甲一」") "建議用 into 放進該下層標題")
        (is (str/includes? e "into")))
      (let [e (:error (ops/move-card s {:id "A2" :target-id "iA" :position :after}))]
        (is (nil? e) "引言之後、其他標題之前沒有段落時，標題可以放"))
      (let [s (:state (ops/add-card s {:kind :paragraph :parent-id "A" :title "甲段"}))
            e (:error (ops/move-card s {:id "A2" :target-id "iA" :position :after}))]
        (is (str/includes? e "下層標題要排在這一層所有段落之後") "標題不能排在段落之前"))
      (is (str/includes? (:error (ops/move-card s {:id "A" :target-id "A21" :position :after})) "自己的下層"))
      (is (str/includes? (:error (ops/move-card s {:id "A" :target-id "A" :position :into})) "自己"))
      (is (str/includes? (:error (ops/move-card s {:id "b1" :target-id "iA" :position :before})) "引言")
          "不能排在引言前面")
      (is (str/includes? (:error (ops/move-card s {:id "b1" :target-id "a1" :position :into})) "是段落"))
      (is (str/includes? (:error (ops/move-card s {:id "A" :target-id "A211" :position :into})) "自己的下層"))
      (is (str/includes? (:error (ops/move-card (update s :nodes dissoc "x1") {:id "B" :target-id "A211" :position :into}))
                         "已經是第四層"))
      (is (str/includes? (:error (ops/move-card (update s :nodes dissoc "x1") {:id "A" :target-id "A211" :position :into}))
                         "自己的下層"))
      (is (str/includes? (:error (ops/move-card (update s :nodes dissoc "a1" "a2" "a3")
                                                {:id "A2" :target-id "A1" :position :into}))
                         "共 3 層")
          "連同下層超過四層")
      (is (str/includes? (:error (ops/move-card s {:id "nope" :target-id "A" :position :into})) "找不到卡片"))
      (is (str/includes? (:error (ops/move-card s {:id "b1" :target-id "A" :position :beside})) "position")))))

(deftest set-relation-test
  (let [s (state)]
    (testing "相鄰段落：設定並確認"
      (let [s' (assoc-in s [:relations 0 :relation/pending] true)
            r (ops/set-relation s' {:from-id "a1" :to-id "a2" :type :contrast})]
        (is (= {:relation/type :contrast :relation/pending false}
               (select-keys (tree/relation-between (:relations (:state r)) "a1" "a2")
                            [:relation/type :relation/pending])))
        (is (= ["a1" "a2"] (:touched r)))))
    (testing "沒有關係時新增"
      (let [r (ops/set-relation s {:from-id "b1" :to-id "b2" :type :example})]
        (is (= :example (:relation/type (tree/relation-between (:relations (:state r)) "b1" "b2"))))))
    (testing "已存在且已確認時說明沒有變動"
      (let [r (ops/set-relation s {:from-id "a1" :to-id "a2" :type :progression})]
        (is (= s (:state r)))
        (is (some? (:note r)))))
    (testing "不相鄰、順序顛倒、不是段落、類型不對都拒絕"
      (is (str/includes? (:error (ops/set-relation s {:from-id "a1" :to-id "a3" :type :causal})) "不相鄰"))
      (is (str/includes? (:error (ops/set-relation s {:from-id "a2" :to-id "a1" :type :causal})) "不相鄰"))
      (is (str/includes? (:error (ops/set-relation s {:from-id "a3" :to-id "b1" :type :causal})) "不相鄰"))
      (is (str/includes? (:error (ops/set-relation s {:from-id "A1" :to-id "A2" :type :causal})) "兩個段落"))
      (let [msg (:error (ops/set-relation s {:from-id "a1" :to-id "a3" :type :causal}))]
        (is (str/includes? msg "a2") "說出 to_id 實際的前一段")
        (is (str/includes? msg "可跨標題")))
      (is (str/includes? (:error (ops/set-relation s {:from-id "a1" :to-id "a1" :type :causal})) "第一段"))
      (is (str/includes? (:error (ops/set-relation s {:from-id "a1" :to-id "a2" :type :basis})) "type 只接受"))
      (is (str/includes? (:error (ops/set-relation s {:from-id "a1" :to-id "nope" :type :causal})) "找不到 to_id")))))

(deftest add-remote-relation-test
  (let [s (state)
        r (ops/add-remote-relation s {:from-id "a3" :to-id "b1" :type :basis})]
    (is (= {:relation/from "a3" :relation/to "b1" :relation/type :basis :relation/remote true}
           (last (:relations (:state r)))))
    (is (= :manual (:origin r)))
    (testing "已存在時說明"
      (let [r2 (ops/add-remote-relation (:state r) {:from-id "a3" :to-id "b1" :type :basis})]
        (is (= (:relations (:state r)) (:relations (:state r2))))
        (is (some? (:note r2))))
      (is (some? (:state (ops/add-remote-relation (:state r) {:from-id "a3" :to-id "b1" :type :echo})))))
    (testing "錯誤"
      (is (str/includes? (:error (ops/add-remote-relation s {:from-id "a1" :to-id "a1" :type :echo})) "同一張"))
      (is (str/includes? (:error (ops/add-remote-relation s {:from-id "a1" :to-id "A1" :type :echo})) "兩個段落"))
      (is (str/includes? (:error (ops/add-remote-relation s {:from-id "a1" :to-id "b1" :type :causal})) "type 只接受")))))

(deftest set-relation-across-headings-test
  (let [s (state)
        r (ops/set-relation s {:from-id "a3" :to-id "x1" :type :contrast})
        rel (tree/relation-between (:relations (:state r)) "a3" "x1")]
    (is (nil? (:error r)) "a3 與 x1 隔著 1.2、1.2.1、1.2.1.1 三層標題，仍是前後相接")
    (is (= {:relation/type :contrast :relation/pending false} (select-keys rel [:relation/type :relation/pending])))
    (is (nil? (:error (ops/set-relation s {:from-id "x1" :to-id "b1" :type :example}))) "跨章")
    (testing "新增標題不改變相鄰；移動標題讓邊界段落待確認"
      (let [m (ops/move-card (:state r) {:id "A2" :target-id "A1" :position :before})
            rels (:relations (:state m))]
        (is (nil? (:error m)))
        (is (nil? (tree/relation-between rels "a3" "x1")))
        (is (:relation/pending (tree/relation-between rels "x1" "a1")))
        (is (str/includes? (:message m) "待確認"))))))

(deftest delete-card-test
  (let [s (state)]
    (testing "刪除段落：關係一併移除，後一段改為承接前一段、待確認"
      (let [r (ops/delete-card s {:id "a2"})
            rels (:relations (:state r))]
        (is (not (contains? (:nodes (:state r)) "a2")))
        (is (not-any? #(#{"a2"} (:relation/to %)) rels))
        (is (:relation/pending (tree/relation-between rels "a1" "a3")))
        (is (= ["a3"] (:touched r)))
        (is (str/includes? (:message r) "待確認"))))
    (testing "刪除標題連同下層"
      (let [r (ops/delete-card s {:id "A2"})]
        (is (not-any? #(contains? (:nodes (:state r)) %) ["A2" "A21" "A211" "x1"]))
        (is (str/includes? (:message r) "3 張下層卡片"))
        (is (= ["b1"] (:touched r)) "x1 沒了，b1 的前一段改為 a3，待確認")))
    (testing "錯誤"
      (is (str/includes? (:error (ops/delete-card s {:id "nope"})) "找不到卡片"))
      (is (str/includes? (:error (ops/delete-card s {:id "a1" :version 2})) "已被修改"))
      (is (some? (:state (ops/delete-card s {:id "a1" :version 1}))))
      (is (str/includes? (:error (ops/delete-card {:nodes (tree/from-nested [(h "A" "甲" (p "a1"))])} {:id "A"}))
                         "至少要保留")))))

(deftest remove-relation-test
  (let [s (-> (state)
              (update :relations conj {:relation/from "a3" :relation/to "b1" :relation/type :basis :relation/remote true}))]
    (testing "相鄰關係"
      (let [r (ops/remove-relation s {:from-id "a1" :to-id "a2"})]
        (is (nil? (tree/relation-between (:relations (:state r)) "a1" "a2")))
        (is (= 2 (count (:relations (:state r)))))))
    (testing "不相鄰關係，指定類型"
      (let [r (ops/remove-relation s {:from-id "a3" :to-id "b1" :type :basis})]
        (is (not-any? #(= "b1" (:relation/to %)) (:relations (:state r))))))
    (testing "沒有符合的關係"
      (is (str/includes? (:error (ops/remove-relation s {:from-id "a1" :to-id "a2" :type :causal})) "沒有類型為 causal"))
      (is (str/includes? (:error (ops/remove-relation s {:from-id "a2" :to-id "a1"})) "沒有任何關係"))
      (is (str/includes? (:error (ops/remove-relation s {:from-id "a1" :to-id "nope"})) "找不到 to_id")))))

;; ---------------------------------------------------------------------------
;; 表格與圖、本文、表格內容（M6）

(defn- label-of [r id] (get-in r [:state :nodes id :node/label]))

(deftest add-table-and-figure-test
  (let [s (state)]
    (testing "表格掛在段落底下：標題、意圖、label 代稱"
      (let [r (ops/add-card s {:kind :table :parent-id "a1" :title "騎乘量" :intent "每月人次" :label "ridership"})
            id (first (:created r))]
        (is (nil? (:error r)))
        (is (= {:node/kind :table :node/title "騎乘量" :node/draft "每月人次" :node/label "tab:ridership" :node/parent "a1"}
               (select-keys (get-in r [:state :nodes id]) [:node/kind :node/title :node/draft :node/label :node/parent])))
        (is (= "新增表格「騎乘量」" (:label r)))))
    (testing "圖：fig: 前綴；沒給代稱時自動產生 4 碼"
      (let [r (ops/add-card s {:kind :figure :parent-id "a1" :title "趨勢圖"})
            l (label-of r (first (:created r)))]
        (is (re-matches #"fig:[a-z0-9]{4}" l))))
    (testing "表格與圖的 label 前綴不同，代稱可以相同"
      (let [r1 (ops/add-card s {:kind :table :parent-id "a1" :title "T" :label "x"})
            r2 (ops/add-card (:state r1) {:kind :figure :parent-id "a1" :title "F" :label "x"})]
        (is (nil? (:error r2)))))
    (testing "只能放在段落底下"
      (is (str/includes? (:error (ops/add-card s {:kind :table :parent-id "A1" :title "T"})) "底下不能放表格"))
      (is (str/includes? (:error (ops/add-card s {:kind :figure :parent-id nil :title "F"})) "只能放標題")))
    (testing "必須有 title"
      (is (str/includes? (:error (ops/add-card s {:kind :table :parent-id "a1"})) "需要 title")))
    (testing "label 代稱格式錯誤，說明規則並舉例"
      (doseq [bad ["Ridership" "a_b" "-a" "a-" "a--b" "" "中文" "a b"]]
        (let [e (:error (ops/add-card s {:kind :table :parent-id "a1" :title "T" :label bad}))]
          (is (and e (str/includes? e "ridership")) (pr-str bad)))))
    (testing "label 重複：拒絕並指出已使用的卡片"
      (let [s2 (:state (ops/add-card s {:kind :table :parent-id "a1" :title "第一張" :label "dup"}))
            e (:error (ops/add-card s2 {:kind :table :parent-id "a2" :title "第二張" :label "dup"}))]
        (is (str/includes? e "tab:dup"))
        (is (str/includes? e "第一張"))))
    (testing "標題、段落、引言不能給 label"
      (is (str/includes? (:error (ops/add-card s {:kind :paragraph :parent-id "A1" :title "P" :label "x"})) "只用於表格與圖")))
    (testing "自動產生的 label 避開既有的"
      (let [r (ops/add-card (assoc-in s [:nodes "a1" :node/label] "tab:aaaa")
                            {:kind :table :parent-id "a1" :title "T"})]
        (is (not= "tab:aaaa" (label-of r (first (:created r)))))))))

(deftest build-outline-with-tables-test
  (let [s (state)
        items [{:kind :paragraph :title "新段" :children [{:kind :table :title "表一" :label "t1"}
                                                           {:kind :figure :title "圖一" :label "t1"}]}]
        r (ops/build-outline s {:parent-id "A1" :after-id "a3" :items items})]
    (testing "段落的 children 可以是表格與圖"
      (is (nil? (:error r)))
      (is (= 3 (count (:created r))))
      (is (= #{"tab:t1" "fig:t1"} (set (keep #(get-in r [:state :nodes % :node/label]) (:created r))))))
    (testing "同一批裡重複的 label 被拒絕"
      (let [e (:error (ops/build-outline s {:parent-id "A1" :items [{:kind :paragraph :title "P"
                                                                    :children [{:kind :table :title "甲表" :label "same"}
                                                                               {:kind :table :title "乙表" :label "same"}]}]}))]
        (is (str/includes? e "items[0].children[1]"))
        (is (str/includes? e "甲表"))))
    (testing "段落底下只能放表格與圖；標題底下不能直接放表格"
      (is (str/includes? (:error (ops/build-outline s {:parent-id "A1" :items [{:kind :paragraph :title "P" :children [{:kind :paragraph :title "Q"}]}]}))
                         "只能放表格與圖"))
      (is (str/includes? (:error (ops/build-outline s {:parent-id nil :items [{:kind :heading :title "H" :children [{:kind :table :title "T"}]}]}))
                         "底下不能放表格")))
    (testing "表格與圖不能有 children"
      (is (str/includes? (:error (ops/build-outline s {:parent-id "A1" :items [{:kind :paragraph :title "P"
                                                                               :children [{:kind :table :title "T" :children [{:kind :figure :title "F"}]}]}]}))
                         "不能有 children")))))

(def ^:private doc-a {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "新的本文"}]}]})

(deftest write-prose-test
  (let [s (state)]
    (testing "段落：寫入本文、狀態轉為 ai（含原本 human 的段落）"
      (let [r (ops/write-prose s {:id "b2" :doc doc-a})]
        (is (nil? (:error r)))
        (is (= doc-a (get-in r [:state :nodes "b2" :node/prose])))
        (is (= :ai (get-in r [:state :nodes "b2" :node/status])))
        (is (= ["b2"] (:touched r)))
        (is (= "撰寫「b2」" (:label r)))))
    (testing "引言：不改狀態，名稱為標題的引言"
      (let [r (ops/write-prose s {:id "iA" :doc doc-a})]
        (is (= doc-a (get-in r [:state :nodes "iA" :node/prose])))
        (is (not (contains? (get-in r [:state :nodes "iA"]) :node/status)))
        (is (= "撰寫「甲」的引言" (:label r)))))
    (testing "cite：依序加入依據，已有的保留、不重複"
      (let [s1 (assoc-in s [:nodes "a1" :node/citations] [{:block "x"}])
            r (ops/write-prose s1 {:id "a1" :doc doc-a :cite-blocks ["y" "x" "z" "y"]})]
        (is (= [{:block "x"} {:block "y"} {:block "z"}] (get-in r [:state :nodes "a1" :node/citations])))))
    (testing "版本衝突"
      (is (str/includes? (:error (ops/write-prose s {:id "a1" :doc doc-a :version 9})) "已被修改"))
      (is (nil? (:error (ops/write-prose s {:id "a1" :doc doc-a :version 1})))))
    (testing "拒絕表格、標題與圖"
      (let [s2 (:state (ops/add-card s {:kind :table :parent-id "a1" :title "T"}))
            s3 (:state (ops/add-card s {:kind :figure :parent-id "a1" :title "F"}))
            tid (first (filter #(= :table (:node/kind (get (:nodes s2) %))) (keys (:nodes s2))))
            fid (first (filter #(= :figure (:node/kind (get (:nodes s3) %))) (keys (:nodes s3))))]
        (is (str/includes? (:error (ops/write-prose s2 {:id tid :doc doc-a})) "write_table"))
        (is (str/includes? (:error (ops/write-prose s3 {:id fid :doc doc-a})) "沒有本文"))
        (is (str/includes? (:error (ops/write-prose s {:id "A1" :doc doc-a})) "沒有本文"))))
    (testing "找不到卡片"
      (is (str/includes? (:error (ops/write-prose s {:id "nope" :doc doc-a})) "找不到卡片")))))

(deftest write-table-test
  (let [s (:state (ops/add-card (state) {:kind :table :parent-id "a1" :title "指標總表"}))
        tid (first (filter #(= :table (:node/kind (get (:nodes s) %))) (keys (:nodes s))))
        write #(ops/write-table s (merge {:id tid} %))]
    (testing "寫入：數字轉字串、狀態 ai"
      (let [r (write {:columns ["指標" "數值"] :rows [["人次" 2871] ["達成率" "95.7%"] ["比率" 1.5]]})]
        (is (nil? (:error r)))
        (is (= {:columns ["指標" "數值"] :rows [["人次" "2871"] ["達成率" "95.7%"] ["比率" "1.5"]]}
               (get-in r [:state :nodes tid :node/table-content])))
        (is (= :ai (get-in r [:state :nodes tid :node/status])))
        (is (= "填寫表格「指標總表」" (:label r)))))
    (testing "rows 可為空"
      (is (= [] (get-in (write {:columns ["甲"] :rows []}) [:state :nodes tid :node/table-content :rows]))))
    (testing "各種錯誤"
      (is (str/includes? (:error (write {:columns [] :rows []})) "columns"))
      (is (str/includes? (:error (write {:columns nil :rows []})) "columns"))
      (is (str/includes? (:error (write {:columns ["甲" " "] :rows []})) "欄名"))
      (is (str/includes? (:error (write {:columns ["甲" 1] :rows []})) "欄名"))
      (is (str/includes? (:error (write {:columns ["甲"] :rows nil})) "rows"))
      (is (str/includes? (:error (write {:columns ["甲" "乙"] :rows [["1" "2"] ["3"]]})) "第 2 列有 1 個儲存格"))
      (is (str/includes? (:error (write {:columns ["甲"] :rows [["1"] "x"]})) "第 2 列"))
      (is (str/includes? (:error (write {:columns ["甲" "乙"] :rows [["1" nil]]})) "第 1 列第 2 格"))
      (is (str/includes? (:error (write {:columns ["甲"] :rows [[true]]})) "字串或數字")))
    (testing "版本衝突、非表格卡片、找不到"
      (is (str/includes? (:error (write {:columns ["甲"] :rows [] :version 99})) "已被修改"))
      (is (str/includes? (:error (ops/write-table s {:id "a1" :columns ["甲"] :rows []})) "不是表格"))
      (is (str/includes? (:error (ops/write-table s {:id "nope" :columns ["甲"] :rows []})) "找不到卡片")))))

(deftest target-chars-test
  (let [s (state)
        target #(get-in % [:state :nodes %2 :node/target-chars])]
    (testing "update-card 設定與清除"
      (let [r (ops/update-card s {:id "a1" :target-chars 400})]
        (is (= 400 (target r "a1")))
        (is (= :none (get-in r [:state :nodes "a1" :node/status])) "不影響狀態")
        (is (nil? (target (ops/update-card (:state r) {:id "a1" :target-chars nil}) "a1"))
            "傳 nil 清除")
        (is (not (contains? (get-in (ops/update-card (:state r) {:id "a1" :target-chars nil}) [:state :nodes "a1"])
                            :node/target-chars))))
      (is (= 5000 (target (ops/update-card s {:id "A" :target-chars 5000}) "A")) "標題")
      (is (= 300 (target (ops/update-card s {:id "iA" :target-chars 300}) "iA")) "引言")
      (testing "同時改其他欄位；沒給 target-chars 不動它"
        (let [r (ops/update-card (:state (ops/update-card s {:id "a1" :target-chars 400})) {:id "a1" :intent "新"})]
          (is (= 400 (target r "a1"))))))
    (testing "只給 target-chars 不算「沒有要修改的欄位」"
      (is (nil? (:error (ops/update-card s {:id "a1" :target-chars 10})))))
    (testing "非正整數是錯誤"
      (doseq [bad [0 -5 1.5 "400" true]]
        (is (str/includes? (:error (ops/update-card s {:id "a1" :target-chars bad})) "正整數") (pr-str bad))))
    (testing "表格與圖沒有字數"
      (let [s2 (:state (ops/add-card s {:kind :table :parent-id "a1" :title "表"}))
            id (first (tree/child-ids (:nodes s2) "a1"))]
        (is (str/includes? (:error (ops/update-card s2 {:id id :target-chars 100})) "沒有字數"))
        (is (str/includes? (:error (ops/add-card s {:kind :table :parent-id "a1" :title "表" :target-chars 100})) "沒有字數"))))
    (testing "add-card 可帶目標字數"
      (let [r (ops/add-card s {:kind :paragraph :parent-id "A1" :title "新" :target-chars 250})
            id (first (:created r))]
        (is (= 250 (get-in r [:state :nodes id :node/target-chars]))))
      (is (str/includes? (:error (ops/add-card s {:kind :paragraph :parent-id "A1" :title "新" :target-chars 0})) "正整數")))
    (testing "build-outline 的 item 可帶目標字數；錯誤指出位置"
      (let [r (ops/build-outline s {:parent-id nil
                                    :items [{:kind :heading :title "丙" :target-chars 2000
                                             :children [{:kind :paragraph :title "p" :target-chars 300}]}]})
            [h p] (:created r)]
        (is (= [2000 300] [(get-in r [:state :nodes h :node/target-chars])
                           (get-in r [:state :nodes p :node/target-chars])])))
      (is (str/starts-with? (:error (ops/build-outline s {:parent-id nil
                                                          :items [{:kind :heading :title "丙" :children [{:kind :paragraph :title "p" :target-chars -1}]}]}))
                            "items[0].children[0]")))))
