(ns knot.note
  "留言與待辦（SPEC 5.11）的純函式：留言串、待辦數、agent 的提問。
  notes 是後端 load-state 的 :notes（依時間排序的扁平清單）；
  所屬卡片已刪除的留言不顯示、不計入待辦。"
  (:require [knot.tree :as tree]))

(defn visible
  "畫面上看得到的留言：整份報告的留言，或所屬卡片仍存在的留言。"
  [nodes notes]
  (filterv #(or (nil? (:note/node %)) (contains? nodes (:note/node %))) notes))

(defn open-todo? [n]
  (and (:note/todo n) (= :open (:note/state n))))

(defn- replies-by-parent [notes]
  (group-by :note/reply-to (filter :note/reply-to notes)))

(defn- all-replies
  "某則留言的所有回覆（含回覆的回覆），依時間排序。"
  [by-parent id]
  (->> (tree-seq (constantly true) #(get by-parent (:note/id %)) {:note/id id})
       rest
       (sort-by :note/at)
       vec))

(defn threads
  "某張卡片（node-id 為 nil 時是整份報告）的留言串：最上層的留言依時間排序，
  每則帶著它的回覆 [{:note :replies [...]}]。"
  [notes node-id]
  (let [by-parent (replies-by-parent notes)]
    (->> notes
         (filter #(and (nil? (:note/reply-to %)) (= node-id (:note/node %))))
         (sort-by :note/at)
         (mapv (fn [n] {:note n :replies (all-replies by-parent (:note/id n))})))))

(defn todo-counts
  "每張卡片未完成的待辦數 {卡片 id n}；整份報告的待辦不列。"
  [nodes notes]
  (->> (visible nodes notes)
       (filter #(and (open-todo? %) (:note/node %)))
       (map :note/node)
       frequencies))

(defn subtree-todo-count
  "卡片與其所有下層卡片的待辦數，供全文大綱的標題使用。"
  [nodes counts id]
  (reduce + (map #(get counts % 0) (cons id (tree/descendant-ids nodes id)))))

(defn open-todos
  "整份報告未完成的待辦，依時間排序。"
  [nodes notes]
  (filterv open-todo? (sort-by :note/at (visible nodes notes))))

(defn agent-questions
  "agent 的提問：agent 寫的最上層留言、還沒有任何回覆（SPEC 5.11）。"
  [nodes notes]
  (let [replied (set (keep :note/reply-to notes))]
    (filterv #(and (= :agent (:note/author %)) (nil? (:note/reply-to %))
                   (not (replied (:note/id %))))
             (sort-by :note/at (visible nodes notes)))))
