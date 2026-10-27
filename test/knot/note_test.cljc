(ns knot.note-test
  (:require [clojure.test :refer [deftest is]]
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

(deftest open-todos-and-questions-test
  (is (= ["t1" "g1"] (map :note/id (note/open-todos nodes notes))))
  (is (= ["q1"] (map :note/id (note/agent-questions nodes notes)))
      "agent 的最上層留言、沒有回覆的才算提問；卡片已刪除的不列"))
