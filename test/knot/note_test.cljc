(ns knot.note-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.note :as note]
            [knot.tree :as tree]))

(def nodes
  (tree/from-nested
   [{:node/id "A" :node/kind :heading :node/name "甲"
     :children [{:node/id "a1" :node/kind :paragraph :node/title "一"}
                {:node/id "a2" :node/kind :paragraph :node/title "二"}]}]))

(defn- n [id at & {:as m}]
  (merge {:note/id id :note/author :user :note/text id :note/at at} m))

(def notes
  [(n "t1" 1 :note/node "a1" :note/todo true :note/state :open)
   (n "t2" 2 :note/node "a1" :note/todo true :note/state :done)
   (n "r1" 3 :note/node "a1" :note/reply-to "t1" :note/author :agent)
   (n "r2" 4 :note/node "a1" :note/reply-to "r1")
   (n "q1" 5 :note/node "a2" :note/author :agent)
   (n "q2" 6 :note/node "a2" :note/author :agent)
   (n "r3" 7 :note/node "a2" :note/reply-to "q2")
   (n "g1" 8 :note/todo true :note/state :open)
   (n "x1" 9 :note/node "gone" :note/todo true :note/state :open)
   (n "x2" 10 :note/node "gone" :note/author :agent)])

(deftest threads-test
  (is (= [["t1" ["r1" "r2"]] ["t2" []]]
         (map (fn [{:keys [note replies]}] [(:note/id note) (mapv :note/id replies)])
              (note/threads notes "a1")))
      "最上層依時間排序，回覆（含回覆的回覆）跟在後面")
  (is (= ["g1"] (map (comp :note/id :note) (note/threads notes nil))) "整份報告的留言"))

(deftest todo-counts-test
  (let [counts (note/todo-counts nodes notes)]
    (is (= {"a1" 1} counts) "只算未完成、卡片仍存在的；整份報告的不列")
    (is (= 1 (note/subtree-todo-count nodes counts "A")))
    (is (= 0 (note/subtree-todo-count nodes counts "a2")))))

(deftest question-counts-test
  (let [counts (note/question-counts nodes notes)]
    (is (= {"a2" 1} counts) "只算還沒有人回覆、卡片仍存在的 agent 提問")
    (is (= 1 (note/subtree-todo-count nodes counts "A")) "標題為含下層的合計")))

(deftest open-todos-and-questions-test
  (is (= ["t1" "g1"] (map :note/id (note/open-todos nodes notes))))
  (is (= ["q1"] (map :note/id (note/agent-questions nodes notes)))
      "agent 的最上層留言、沒有回覆的才算提問；卡片已刪除的不列"))

;; Knot 的待辦 -----------------------------------------------------------------

(def doc {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "本文"}]}]})

(def k-nodes
  (tree/from-nested
   [{:node/id "A" :node/kind :heading :node/name "甲"
     :children [{:node/id "iA" :node/kind :intro :node/prose doc}
                {:node/id "a1" :node/kind :paragraph :node/title "背景" :node/draft "d1" :node/prose "p1"}
                {:node/id "a2" :node/kind :paragraph :node/title "現況" :node/draft "d2" :node/prose doc}]}
    {:node/id "B" :node/kind :heading :node/name "乙"
     :children [{:node/id "b1" :node/kind :paragraph :node/title "彙整" :node/prose doc}]}]))

(def k-rels
  [{:relation/from "a1" :relation/to "a2" :relation/type :causal}
   {:relation/from "b1" :relation/to "a1" :relation/type :summarizes :relation/remote true}])

(defn- k-state [nodes] {:nodes nodes :relations k-rels})
(defn- edit [nodes id & kvs] (update nodes id merge (apply hash-map kvs)))

(deftest knot-todos-test
  (let [before (k-state k-nodes)
        todos (fn [after notes] (note/knot-todos before (k-state after) notes))
        open-note {:note/id "n1" :note/author :knot :note/todo true :note/state :open :note/node "a2"
                   :note/triggers [{:id "b1" :dependency :summary :reason "摘要它" :fields #{:prose}}]}]
    (testing "改本文：摘要、上層引言、因果承接各一則（依文件順序），記下每個觸發與原因"
      (let [ts (todos (edit k-nodes "a1" :node/prose "p1x") [])]
        (is (= [["iA" :add] ["a2" :add] ["b1" :add]] (map (juxt :note/node :op) ts)))
        (is (= [[{:id "a1" :dependency :summary :reason "是它的上層引言" :fields #{:prose}}]
                [{:id "a1" :dependency :argument :reason "因果承接" :fields #{:prose}}]
                [{:id "a1" :dependency :summary :reason "摘要它" :fields #{:prose}}]]
               (map :note/triggers ts)))))
    (testing "文字：意圖、本文、兩者"
      (is (= "「第一章 背景」的意圖改了，本段摘要它，可能需要跟著更新。"
             (:note/text (last (todos (edit k-nodes "a1" :node/draft "d1x") [])))))
      (is (= "「第一章 背景」的本文改了，本段因果承接它，可能需要跟著更新。"
             (:note/text (second (todos (edit k-nodes "a1" :node/prose "p1x") [])))))
      (is (= "「第一章 背景」的意圖與本文改了，本段是它的上層引言，可能需要跟著更新。"
             (:note/text (first (todos (edit k-nodes "a1" :node/prose "p1x" :node/draft "d1x") []))))))
    (testing "同一個目標一次有多個觸發：合併成一則"
      (let [ts (todos (edit (edit k-nodes "a1" :node/prose "p1x") "a2" :node/prose (assoc doc :v 2)) [])
            intro (first ts)]
        (is (= ["iA" "a2" "b1"] (map :note/node ts)))
        (is (= ["a1" "a2"] (map :id (:note/triggers intro))))
        (is (= "「第一章 背景」「第一章 現況」改了，本段是它們的上層引言，可能需要跟著更新。" (:note/text intro)))))
    (testing "目標已有未完成的 Knot 待辦：附加新的觸發（:update），已有的觸發不重複"
      (let [ts (todos (edit k-nodes "a1" :node/prose "p1x") [open-note])
            upd (second ts)]
        (is (= [:add :update :add] (map :op ts)))
        (is (= "n1" (:note/id upd)))
        (is (= ["b1" "a1"] (map :id (:note/triggers upd))))
        (is (= "「第二章 彙整」「第一章 背景」改了，本段摘要它們、因果承接它們，可能需要跟著更新。"
               (:note/text upd)))
        (is (= "因果承接" (:reason upd))))
      (is (empty? (note/knot-todos before (k-state (edit k-nodes "b1" :node/prose "p")) [open-note]))
          "b1 已是 a2 待辦的觸發，且 b1 的依賴不含 a2")
      (let [same (assoc open-note :note/triggers [{:id "a1" :dependency :argument :reason "因果承接" :fields #{:prose}}])]
        (is (not-any? #(= "a2" (:note/node %)) (todos (edit k-nodes "a1" :node/prose "p1x") [same]))
            "同一觸發卡片已記錄時不動")))
    (testing "完成的待辦不算，會重新留一則"
      (is (= [:add :add :add] (map :op (todos (edit k-nodes "a1" :node/prose "p1x") [(assoc open-note :note/state :done)])))))
    (testing "目標還沒有本文時不留待辦（引言也一樣）"
      (let [blank (-> k-nodes (assoc-in ["a2" :node/prose] nil) (assoc-in ["iA" :node/prose] {:type "doc" :content []}))
            ts (note/knot-todos (k-state blank) (k-state (edit blank "a1" :node/prose "p1x")) [])]
        (is (= ["b1"] (map :note/node ts)))
        (is (= ["摘要它"] (map :reason ts)))))
    (testing "沒有變動、標題或新增的卡片不觸發"
      (is (empty? (todos k-nodes [])))
      (is (empty? (todos (edit k-nodes "A" :node/name "甲甲") [])))
      (is (empty? (todos (edit k-nodes "b1" :node/title "新名") [])))
      (is (empty? (todos (assoc k-nodes "a3" {:node/id "a3" :node/kind :paragraph :node/parent "A" :node/draft "x"}) []))))))
