(ns knot.ui
  "主編輯畫面的狀態轉換。每個函式都是 (state & args) → state 的純函式。"
  (:require [knot.tree :as tree]))

(defn init-state [report]
  {:report (select-keys report [:report/title :report/subtitle :report/style-name
                                :report/pending-proposals])
   :nodes (tree/from-nested (:tree report))
   :relations (vec (:relations report))
   :facts (into {} (map (juxt :fact/key identity)) (:facts report))
   :blocks (vec (:blocks report))
   :history []
   :ui {:selected (or (:ui/selected report) "p312")
        :page :editor
        :view :tree
        :source-filter :all
        :source-query ""
        :viewport-h 640
        :bias {}
        :next-id 1}})

(defn- titles [{:keys [nodes]}]
  (let [numbers (tree/numbering nodes)]
    #(tree/display-title nodes numbers %)))

(defn- remember
  "記下目前的卡片與關係，供復原使用；label 說明這次修改。"
  [state label]
  (update state :history conj {:nodes (:nodes state)
                               :relations (:relations state)
                               :selected (get-in state [:ui :selected])
                               :label label}))

;; ---------------------------------------------------------------------------
;; 選取與導覽

(defn select [state id]
  (if (contains? (:nodes state) id)
    (update state :ui assoc :selected id :bias {} :toast nil)
    state))

(defn navigate
  "方向鍵：左右換層（parent／第一個子卡片），上下換兄弟。"
  [state direction]
  (let [{:keys [nodes]} state
        id (get-in state [:ui :selected])
        target (case direction
                 :left (tree/parent nodes id)
                 :right (first (tree/child-ids nodes id))
                 :up (tree/prev-sibling nodes id)
                 :down (tree/next-sibling nodes id))]
    (if target (select state target) state)))

(defn set-view [state view]
  (assoc-in state [:ui :view] view))

(defn set-viewport-height [state h]
  (assoc-in state [:ui :viewport-h] h))

(defn dismiss-toast [state]
  (assoc-in state [:ui :toast] nil))

;; ---------------------------------------------------------------------------
;; 移動

(defn- move-message [state moving-id place]
  (let [title (titles state)
        dest (if-let [p (:parent place)] (title p) "報告最上層")]
    (str "已將「" (title moving-id) "」移到「" dest "」"
         (if (tree/paragraph? (:nodes state) moving-id)
           "；相鄰段落的關係標為「待確認」"
           "；編號已重新計算"))))

(defn apply-move
  "依 placement 移動卡片：一次移動是一個可復原的修改組（SPEC 5.3）。"
  [state moving-id place]
  (if-not place
    state
    (let [msg (move-message state moving-id place)
          {:keys [nodes relations]} (tree/move-with-relations state moving-id place)]
      (-> state
          (remember (str "移動「" ((titles state) moving-id) "」"))
          (assoc :nodes nodes :relations relations)
          (update :ui assoc :selected moving-id :bias {} :toast msg)))))

(defn move-selected
  "詳情面板的上移（-1）、下移（1）。"
  [state delta]
  (let [id (get-in state [:ui :selected])]
    (apply-move state id (tree/sibling-placement (:nodes state) id delta))))

;; ---------------------------------------------------------------------------
;; 拖曳

(defn drag-start [state id]
  (update state :ui assoc :drag id :over nil))

(defn drag-over
  "指標在 target 卡片的 half（:top／:bottom）上方；更新插入位置提示。"
  [state target half]
  (let [{:keys [drag over]} (:ui state)
        pos (when drag (tree/drop-position (:nodes state) drag target half))
        over' (when pos {:id target :pos pos})]
    (if (= over over')
      state
      (assoc-in state [:ui :over] over'))))

(defn drag-end [state]
  (update state :ui assoc :drag nil :over nil :bias {}))

(defn drop-dragged [state]
  (let [{:keys [drag over]} (:ui state)
        place (when (and drag over)
                (tree/placement (:nodes state) drag (:id over) (:pos over)))]
    (-> (apply-move state drag place)
        (drag-end))))

(defn edge-scroll
  "拖曳時指標靠近欄位上下緣，該欄自動捲動。"
  [state column delta]
  (if (and (get-in state [:ui :drag]) delta (not (zero? delta)))
    (update-in state [:ui :bias column] (fnil + 0) delta)
    state))

;; ---------------------------------------------------------------------------
;; 新增與轉換

(defn- new-id [state]
  [(str "new-" (get-in state [:ui :next-id]))
   (update-in state [:ui :next-id] inc)])

(defn- insert-node [state parent-id node]
  (let [[id state] (new-id state)
        k (tree/insert-key (:nodes state) parent-id (:node/kind node))]
    [id (assoc-in state [:nodes id]
                  (assoc node :node/id id :node/parent parent-id :node/order-key k))]))

(defn add-child
  "在標題底下新增引言、段落或下層標題；不合法時不變。"
  [state parent-id kind]
  (let [{:keys [nodes]} state
        probe-id ::probe
        probe (assoc nodes probe-id {:node/id probe-id :node/kind kind})]
    (if-not (tree/accepts? probe parent-id probe-id)
      state
      (let [node (case kind
                   :intro {:node/kind :intro :node/draft ""}
                   :paragraph {:node/kind :paragraph :node/title "新段落" :node/status :none :node/draft ""}
                   :heading {:node/kind :heading :node/name "新標題" :node/draft ""})
            label (str "新增" ({:intro "引言" :paragraph "段落" :heading "下層標題"} kind))
            [id state] (-> state (remember label) (insert-node parent-id node))]
        (update state :ui assoc :selected id :bias {}
                :toast (if (= kind :intro)
                         "新增引言：寫下意圖（可留空），本文由 AI 依下層內容生成"
                         label))))))

(defn wrap-paragraphs
  "已有段落的標題要新增下層標題時：把現有段落移到新的下層標題。"
  [state heading-id]
  (let [{:keys [nodes]} state
        paras (filterv #(tree/paragraph? nodes %) (tree/child-ids nodes heading-id))]
    (if (or (empty? paras)
            (>= (tree/depth nodes heading-id) tree/max-heading-level))
      state
      (let [label "把現有段落移到新的下層標題"
            [sub-id state] (-> state
                               (remember label)
                               (insert-node heading-id {:node/kind :heading
                                                        :node/name "新標題"
                                                        :node/draft ""}))
            state (reduce (fn [s pid]
                            (update s :nodes tree/move pid
                                    {:parent sub-id
                                     :index (count (tree/child-ids (:nodes s) sub-id))}))
                          state
                          paras)]
        (update state :ui assoc :selected sub-id :bias {} :toast label)))))

(defn paragraph->intro
  "把段落改為所屬標題的引言（該標題還沒有引言時）。"
  [state para-id]
  (let [{:keys [nodes]} state
        p (tree/parent nodes para-id)]
    (if (or (not (tree/paragraph? nodes para-id))
            (seq (tree/child-ids nodes para-id))
            (tree/intro-id nodes p))
      state
      (let [label "把段落改為引言"
            state (remember state label)
            nodes (-> (:nodes state)
                      (update para-id assoc :node/kind :intro)
                      (update para-id dissoc :node/title))
            nodes (assoc-in nodes [para-id :node/order-key] (tree/insert-key (dissoc nodes para-id) p :intro))]
        (-> state
            (assoc :nodes nodes)
            (update :relations (fn [rels] (vec (remove #(or (= para-id (:relation/from %))
                                                            (= para-id (:relation/to %)))
                                                       rels))))
            (update :ui assoc :selected para-id :toast label))))))

(defn toggle-lock
  "鎖定只由使用者設定或解除；解除後回到人工改過或 AI 生成。"
  [state id]
  (let [node (get-in state [:nodes id])]
    (if-not (= :paragraph (:node/kind node))
      state
      (let [locked? (= :locked (:node/status node))
            label (if locked? "解除鎖定" "鎖定段落")]
        (-> state
            (remember label)
            (update-in [:nodes id]
                       (fn [n]
                         (if locked?
                           (-> n
                               (assoc :node/status (or (:node/unlocked-status n)
                                                       (if (:node/prose n) :human :none)))
                               (dissoc :node/unlocked-status))
                           (assoc n :node/status :locked
                                  :node/unlocked-status (:node/status n)))))
            (assoc-in [:ui :toast] label))))))

;; ---------------------------------------------------------------------------
;; 復原

(defn undo
  "復原最近一個修改組（SPEC 5.10）。M1 之後改由資料層新增反向修改組。"
  [state]
  (if-let [{:keys [nodes relations selected label]} (peek (:history state))]
    (-> state
        (update :history pop)
        (assoc :nodes nodes :relations relations)
        (update :ui assoc :selected selected :bias {} :toast (str "已復原：" label)))
    state))

;; ---------------------------------------------------------------------------
;; 編輯：使用者輸入的內容都是卡片的意圖（SPEC 5.4）

(def editable-fields #{:node/draft :node/title :node/name})

(defn set-node-field
  "編輯卡片的名稱或意圖。逐字輸入不進復原紀錄（輸入框有自己的復原）。"
  [state id field value]
  (if (and (editable-fields field) (contains? (:nodes state) id))
    (assoc-in state [:nodes id field] (or value ""))
    state))

(defn add-citation
  "以素材選取器把區塊加入段落的依據。"
  [state id block-id]
  (let [cites (get-in state [:nodes id :node/citations])]
    (if (or (not (tree/paragraph? (:nodes state) id))
            (some #(= block-id (:block %)) cites))
      state
      (-> state
          (remember "加入依據")
          (update-in [:nodes id :node/citations] (fnil conj []) {:block block-id})
          (assoc-in [:ui :toast] "已加入依據")))))

(defn remove-citation [state id block-id]
  (-> state
      (remember "移除依據")
      (update-in [:nodes id :node/citations] #(vec (remove (fn [c] (= block-id (:block c))) %)))))

(defn set-relation
  "設定兩個相鄰段落的關係類型並確認；type 為 nil 時移除關係。"
  [state from to type]
  (let [others (vec (remove #(and (= from (:relation/from %)) (= to (:relation/to %))
                                  (not= :summarizes (:relation/type %)))
                            (:relations state)))]
    (-> state
        (remember "設定段落關係")
        (assoc :relations (cond-> others
                            type (conj {:relation/from from :relation/to to
                                        :relation/type type :relation/pending false}))))))

;; ---------------------------------------------------------------------------
;; 素材選取器（段落編輯區）

(defn open-picker [state]
  (update state :ui assoc :picker {:query "" :filter :all}))

(defn close-picker [state]
  (update state :ui dissoc :picker))

(defn set-picker-query [state q]
  (if (get-in state [:ui :picker])
    (assoc-in state [:ui :picker :query] (or q ""))
    state))

(defn set-picker-filter [state f]
  (if (get-in state [:ui :picker])
    (assoc-in state [:ui :picker :filter] f)
    state))

;; ---------------------------------------------------------------------------
;; 素材庫頁面

(defn set-page [state page]
  (update state :ui assoc :page page :picker nil))

(defn set-source-filter [state f]
  (assoc-in state [:ui :source-filter] f))

(defn set-source-query [state q]
  (assoc-in state [:ui :source-query] (or q "")))

(defn select-block [state block-id]
  (assoc-in state [:ui :library-selected] block-id))

(defn set-block-status
  "候選區塊的確認或捨棄（M3 之後寫入資料層）。"
  [state block-id status]
  (update state :blocks
          (fn [blocks]
            (mapv #(if (= block-id (:block/id %)) (assoc % :block/status status) %) blocks))))

(defn open-paragraph
  "從素材庫跳回撰寫頁的某張卡片。"
  [state id]
  (-> state (set-page :editor) (assoc-in [:ui :view] :tree) (select id)))
