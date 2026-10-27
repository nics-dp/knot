(ns knot.notes
  "留言與待辦（SPEC 5.11）。留言不是內容修改：不經 db/apply-changes!、不進修改組、不能復原。
  留言以卡片的 :node/id 關聯（不是 ref）：卡片刪除後留言留在資料庫但看不到，復原卡片（同一個 :node/id）後又出現。
  所有寫入函式在 conn 的鎖內檢查並寫入；成功回傳留言（刪除為 {:deleted ids}），失敗回傳 {:error 中文原因}。"
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [knot.db :as db]))

(defn list-notes
  "所有留言（含所屬卡片已刪除而隱藏的），依時間由舊到新。"
  [db]
  (db/list-notes db))

(defn- card-exists? [db id]
  (some? (d/entid db [:node/id id])))

(defn visible?
  "留言可見：屬於整份報告（沒有 :note/node），或所屬卡片仍存在。"
  [db note]
  (or (nil? (:note/node note)) (card-exists? db (:note/node note))))

(defn- visible-notes [db]
  (filterv #(visible? db %) (list-notes db)))

(defn- find-note [db id]
  (some #(when (= id (:note/id %)) %) (list-notes db)))

(defn- replies-of
  "note-id 的所有後代回覆（遞迴），依時間由舊到新。"
  [notes note-id]
  (let [children (group-by :note/reply-to notes)
        descend (fn descend [id] (mapcat #(cons % (descend (:note/id %))) (get children id)))]
    (vec (sort-by :note/at (descend note-id)))))

(defn thread
  "note 的討論串（後代回覆），依時間由舊到新。"
  [db note-id]
  (replies-of (list-notes db) note-id))

(defn- now [] (java.util.Date.))

(defn- error [msg] {:error msg})

;; ---------------------------------------------------------------------------
;; 寫入

(defn add-note!
  "新增留言。選項 {:node 卡片 id 或 nil :author :user|:agent :text :todo :reply-to 留言 id}。
  回覆的卡片取自被回覆的留言；只有使用者能建立待辦。回傳留言或 {:error ..}。"
  [conn {:keys [node author text todo reply-to]}]
  (locking conn
    (let [db (d/db conn)
          parent (when reply-to (find-note db reply-to))]
      (cond
        (not (#{:user :agent} author)) (error "留言者必須是使用者或 agent")
        (or (not (string? text)) (str/blank? text)) (error "留言內容不能是空白")
        (and reply-to (nil? parent)) (error "找不到要回覆的留言")
        (and parent node (not= node (:note/node parent))) (error "回覆的卡片必須與被回覆的留言相同")
        (and (nil? parent) node (not (card-exists? db node))) (error "找不到卡片")
        (and todo (not= author :user)) (error "只有使用者能建立待辦")
        :else
        (let [node (if parent (:note/node parent) node)
              id (random-uuid)
              note (cond-> {:note/id id
                            :note/author author
                            :note/text text
                            :note/at (now)}
                     node (assoc :note/node node)
                     reply-to (assoc :note/reply-to reply-to)
                     todo (assoc :note/todo true :note/state :open))]
          (d/transact! conn [note])
          note)))))

(defn set-todo!
  "使用者把自己的留言標為待辦（todo? 為 true，狀態 :open）或取消待辦。"
  [conn id todo?]
  (locking conn
    (let [db (d/db conn)
          note (find-note db id)]
      (cond
        (nil? note) (error "找不到留言")
        (not= :user (:note/author note)) (error "只有使用者自己的留言能標為待辦")
        todo? (do (d/transact! conn [{:note/id id :note/todo true :note/state :open}])
                  (find-note (d/db conn) id))
        :else (do (d/transact! conn (for [a [:note/todo :note/state :note/done-at :note/changeset]
                                          :let [v (get note a)] :when (some? v)]
                                      [:db/retract [:note/id id] a v]))
                  (find-note (d/db conn) id))))))

(defn set-state!
  "待辦改為 :open 或 :done。:done 記下完成時間與選填的修改組 id（:changeset）；:open 清除這兩者。"
  [conn id state {:keys [changeset]}]
  (locking conn
    (let [db (d/db conn)
          note (find-note db id)]
      (cond
        (nil? note) (error "找不到留言")
        (not (:note/todo note)) (error "這則留言不是待辦")
        (not (#{:open :done} state)) (error "待辦狀態只能是 open 或 done")
        :else
        (do (d/transact! conn
                         (if (= :done state)
                           [(cond-> {:note/id id :note/state :done :note/done-at (now)}
                              changeset (assoc :note/changeset changeset))]
                           (cons {:note/id id :note/state :open}
                                 (for [a [:note/done-at :note/changeset]
                                       :let [v (get note a)] :when (some? v)]
                                   [:db/retract [:note/id id] a v]))))
            (find-note (d/db conn) id))))))

(defn delete-note!
  "刪除自己的留言與它所有的回覆（遞迴）。回傳 {:deleted [留言 id ...]} 或 {:error ..}。"
  [conn id author]
  (locking conn
    (let [notes (list-notes (d/db conn))
          note (some #(when (= id (:note/id %)) %) notes)]
      (cond
        (nil? note) (error "找不到留言")
        (not= author (:note/author note)) (error "只能刪除自己的留言")
        :else
        (let [ids (into [id] (map :note/id) (replies-of notes id))]
          (d/transact! conn (for [i ids] [:db/retractEntity [:note/id i]]))
          {:deleted ids})))))

;; ---------------------------------------------------------------------------
;; 查詢（只含可見的留言）

(defn open-todos
  "未完成的待辦，依時間由舊到新。"
  [db]
  (filterv #(and (:note/todo %) (= :open (:note/state %))) (visible-notes db)))

(defn agent-questions
  "agent 提出、還沒有任何回覆的頂層留言。"
  [db]
  (let [notes (visible-notes db)
        replied (into #{} (keep :note/reply-to) (list-notes db))]
    (filterv #(and (= :agent (:note/author %))
                   (nil? (:note/reply-to %))
                   (not (replied (:note/id %))))
             notes)))

(defn agent-threads
  "agent 提出的頂層留言中已有回覆者（與 agent-questions 互補）。"
  [db]
  (let [replied (into #{} (keep :note/reply-to) (list-notes db))]
    (filterv #(and (= :agent (:note/author %))
                   (nil? (:note/reply-to %))
                   (replied (:note/id %)))
             (visible-notes db))))

(defn todo-counts
  "{卡片 id 未完成待辦數}；整份報告的留言不列。"
  [db]
  (frequencies (keep :note/node (open-todos db))))

(defn latest-agent-changeset
  "since 之後 agent 最近一個修改過卡片 node-id 的修改組 id；沒有時為 nil。"
  [db node-id since]
  (when node-id
    (->> (d/q '[:find ?id ?at
                :in $ ?target ?since
                :where [?r :revision/target ?target] [?r :revision/changeset ?c]
                [?c :changeset/by :agent] [?c :changeset/at ?at]
                [(> ?at ?since)] [?c :changeset/id ?id]]
              db (str "nodes/" node-id) since)
         (sort-by second)
         last
         first)))
