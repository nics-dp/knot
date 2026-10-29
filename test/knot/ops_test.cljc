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
      (is (str/includes? (error-of (ops/add-card s {:kind :table :parent-id "A1" :title "x"})) "kind 只接受"))
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
      (let [r (ops/build-outline s {:parent-id nil :items [{:kind :paragraph :title "x" :children [{:kind :paragraph :title "y"}]}]})]
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
        (is (= [] (:touched r)))))
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
