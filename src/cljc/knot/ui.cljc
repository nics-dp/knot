(ns knot.ui
  "主編輯畫面的狀態轉換。每個函式都是 (state & args) → state 的純函式。"
  (:require [knot.layout :as layout]
            [knot.tree :as tree]))

(def initial-ui
  {:page :tree
   :source-filter :all
   :source-query ""
   :viewport-h 640
   :bias {}
   ;; 編輯區預設收合，點卡片不會自動打開
   :editor-collapsed true})

(defn- default-selection
  "預設選取第一個段落；沒有段落時選第一張卡片。"
  [nodes]
  (let [order (tree/document-order nodes)]
    (or (some #(when (tree/paragraph? nodes %) %) order) (first order))))

(defn init-state
  "由範例報告（巢狀卡片樹）建立狀態；測試與離線使用。"
  [report]
  (let [nodes (tree/from-nested (:tree report))]
    {:report (select-keys report [:report/title :report/subtitle :report/style-name])
     :nodes nodes
     :relations (vec (:relations report))
     :facts (into {} (map (juxt :fact/key identity)) (:facts report))
     :blocks (vec (:blocks report))
     :history []
     :ui (assoc initial-ui :selected (or (:ui/selected report) (default-selection nodes)))}))

(defn- server-data
  "後端 load-state 的結果轉成狀態中的資料部分。"
  [loaded]
  {:report (:report loaded)
   :nodes (into {} (map (juxt :node/id identity)) (:nodes loaded))
   :relations (vec (:relations loaded))
   :facts (into {} (map (juxt :fact/key identity)) (:facts loaded))
   :blocks (vec (:blocks loaded))})

(defn from-server
  "由後端讀回的報告建立狀態。"
  [loaded]
  (let [data (server-data loaded)]
    (assoc data
           :history []
           :ui (assoc initial-ui :selected (default-selection (:nodes data))))))

(defn replace-data
  "以後端的資料取代目前的資料（例如復原之後），保留畫面狀態；選取的卡片已不存在時改選預設。"
  [state loaded]
  (let [state (merge state (server-data loaded))]
    (if (contains? (:nodes state) (get-in state [:ui :selected]))
      state
      (assoc-in state [:ui :selected] (default-selection (:nodes state))))))

(defn- titles [{:keys [nodes]}]
  (let [numbers (tree/numbering nodes)]
    #(tree/display-title nodes numbers %)))

(defn- remember
  "記下一次修改：label 說明這次修改，origin 為修改組的來源（SPEC 4 :changeset/origin）。
  前端據此決定修改組的名稱；離線時也供 undo 使用。"
  ([state label] (remember state label :manual))
  ([state label origin]
   (update state :history conj {:nodes (:nodes state)
                                :relations (:relations state)
                                :selected (get-in state [:ui :selected])
                                :label label
                                :origin origin})))

;; ---------------------------------------------------------------------------
;; 選取與導覽

(defn select [state id]
  (if (contains? (:nodes state) id)
    (update state :ui assoc :selected id :bias {} :toast nil :picker nil :rel-picker nil
            :editing-relations false)
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

(defn set-viewport-height [state h]
  (assoc-in state [:ui :viewport-h] h))

(defn toggle-editor
  "收合或展開編輯區（覆蓋在卡片看板上的抽屜）。"
  [state]
  (update-in state [:ui :editor-collapsed] not))

(defn open-editor [state]
  (assoc-in state [:ui :editor-collapsed] false))

(defn close-editor [state]
  (assoc-in state [:ui :editor-collapsed] true))

(defn toggle-previous
  "本文區切換「與前一版比較」。"
  [state]
  (update-in state [:ui :show-previous] not))

(defn set-previous
  "後端讀回的前一版內容（knot.db/node-previous）。"
  [state id data]
  (assoc-in state [:ui :previous] {:id id :data data}))

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
          (remember (str "移動「" ((titles state) moving-id) "」") :move)
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
  "指標在第 column 欄、欄內內容座標 y；更新放置位置提示（見 layout/drop-target）。"
  [state column y]
  (let [{:keys [drag over]} (:ui state)
        nodes (:nodes state)
        over' (when (and drag y)
                (layout/drop-target nodes (layout/columns nodes) drag column y))]
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

(defn scroll-column
  "捲動卡片樹的一欄：delta 為位移變化量，[lo hi] 為這一欄允許的 bias 範圍。"
  [state column delta lo hi]
  (if (and delta (not (zero? delta)))
    (let [b (+ (get-in state [:ui :bias column] 0) delta)]
      (assoc-in state [:ui :bias column] (-> b (max lo) (min hi))))
    state))

(defn edge-scroll
  "拖曳時指標靠近欄位上下緣，該欄自動捲動。"
  [state column delta lo hi]
  (if (get-in state [:ui :drag])
    (scroll-column state column delta lo hi)
    state))

;; ---------------------------------------------------------------------------
;; 新增與轉換

(defn- new-id [state]
  [(random-uuid) state])

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
        (update state :ui assoc :selected id :bias {} :focus-title (when-not (= kind :intro) id)
                :editor-collapsed false
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
(defn sibling-kind
  "在 id 的層級上新增時的卡片類型：標題、段落照原類型；引言之後依同層其他卡片，
  沒有其他卡片時為段落。表格與圖待 label 命名方式定案（回傳 nil）。"
  [nodes id]
  (case (tree/kind nodes id)
    (:heading :paragraph) (tree/kind nodes id)
    :intro (or (some #{:heading :paragraph} (map #(tree/kind nodes %) (tree/siblings nodes id)))
               :paragraph)
    nil))

(defn add-sibling
  "在選取卡片的層級上新增：接在它後面，類型見 sibling-kind。"
  [state id]
  (let [{:keys [nodes]} state
        k (sibling-kind nodes id)
        p (tree/parent nodes id)]
    (if-not (#{:heading :paragraph} k)
      state
      (let [[new-id state] (new-id (remember state (if (= k :heading) "新增同層標題" "新增段落")))
            node (if (= k :heading)
                   {:node/kind :heading :node/name "新標題" :node/draft ""}
                   {:node/kind :paragraph :node/title "新段落" :node/status :none :node/draft ""})
            nodes' (assoc (:nodes state) new-id
                          (assoc node :node/id new-id :node/parent p
                                 :node/order-key (tree/key-after (:nodes state) id)))
            next-para (tree/next-sibling nodes' new-id)]
        (-> state
            (assoc :nodes nodes')
            ;; 原本接在後面的段落改為承接新段落，關係待確認
            (update :relations #(if (= k :paragraph)
                                  (tree/mark-pending % nodes' [next-para] {next-para id})
                                  %))
            (update :ui assoc :selected new-id :bias {} :focus-title new-id :editor-collapsed false
                    :toast (if (= k :heading) "已新增同層標題" "已新增段落")))))))

(defn delete-node
  "刪除卡片與其所有下層卡片；可以復原。"
  [state id]
  (let [{:keys [nodes]} state]
    (if-not (contains? nodes id)
      state
      (let [title ((titles state) id)
            gone (set (cons id (tree/descendant-ids nodes id)))
            fallback (or (tree/prev-sibling nodes id) (tree/next-sibling nodes id) (tree/parent nodes id))
            next-para (let [n (tree/next-sibling nodes id)] (when (tree/paragraph? nodes n) n))
            nodes' (apply dissoc nodes gone)]
        (if (empty? nodes')
          state
          (-> state
              (remember (str "刪除「" title "」"))
              (assoc :nodes nodes')
              (update :relations (fn [rels]
                                   (let [rels (vec (remove #(or (gone (:relation/from %)) (gone (:relation/to %))) rels))]
                                     (if next-para
                                       (tree/mark-pending rels nodes' [next-para] {next-para id})
                                       rels))))
              (update :ui assoc :selected (or fallback (first (tree/child-ids nodes' nil))) :bias {}
                      :toast (str "已刪除「" title "」"
                                  (when (> (count gone) 1) (str "與 " (dec (count gone)) " 張下層卡片"))
                                  "；Ctrl／⌘+Z 可復原"))))))))

(defn clear-focus-title [state]
  (update state :ui dissoc :focus-title))

;; ---------------------------------------------------------------------------
;; 編輯：使用者輸入的內容都是卡片的意圖（SPEC 5.4）

(def editable-fields #{:node/draft :node/title :node/name})

(defn set-node-field
  "編輯卡片的名稱或意圖。逐字輸入不進復原紀錄（輸入框有自己的復原）。"
  [state id field value]
  (if (and (editable-fields field) (contains? (:nodes state) id))
    (assoc-in state [:nodes id field] (or value ""))
    state))

(defn set-prose
  "在本文編輯器修改本文（逐字輸入，不進復原紀錄；編輯器有自己的復原）。
  段落改了本文就轉為 :human（SPEC 4 狀態轉換）；鎖定的段落不能修改。"
  [state id doc]
  (let [{:node/keys [kind status] :as node} (get-in state [:nodes id])]
    (if (or (nil? node) (= :locked status) (= doc (:node/prose node)))
      state
      (cond-> (assoc-in state [:nodes id :node/prose] doc)
        (and (= :paragraph kind) (not= :human status))
        (assoc-in [:nodes id :node/status] :human)))))

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
                                  (not (tree/remote-relation? %)))
                            (:relations state)))]
    (-> state
        (remember "設定段落關係")
        (assoc :relations (cond-> others
                            type (conj {:relation/from from :relation/to to
                                        :relation/type type :relation/pending false}))))))

(defn add-remote-relation
  "新增不相鄰的關係：from 為本段，to 為目標段落。"
  [state from to type]
  (let [{:keys [nodes relations]} state
        dup? (some #(and (= from (:relation/from %)) (= to (:relation/to %))
                         (= type (:relation/type %)) (tree/remote-relation? %))
                   relations)]
    (if (or dup? (= from to)
            (not (tree/paragraph? nodes from)) (not (tree/paragraph? nodes to)))
      state
      (-> state
          (remember "新增關係")
          (update :relations conj {:relation/from from :relation/to to
                                   :relation/type type :relation/remote true})
          (update :ui dissoc :rel-picker)
          (assoc-in [:ui :toast] "已新增關係")))))

(defn remove-remote-relation [state from to type]
  (-> state
      (remember "移除關係")
      (update :relations (fn [rels]
                           (vec (remove #(and (= from (:relation/from %)) (= to (:relation/to %))
                                              (= type (:relation/type %)) (tree/remote-relation? %))
                                        rels))))))

(defn set-remote-relation-type
  "改變不相鄰關係的類型。類型是關係識別鍵的一部分，因此是換成新的一條；
  同一對段落已有該類型時直接合併。"
  [state from to old-type new-type]
  (if (or (nil? new-type) (= old-type new-type))
    state
    (let [same? (fn [t r] (and (= from (:relation/from r)) (= to (:relation/to r))
                               (= t (:relation/type r)) (tree/remote-relation? r)))
          exists? (some #(same? new-type %) (:relations state))]
      (-> state
          (remember "變更關係類型")
          (update :relations
                  (fn [rels]
                    (cond-> (vec (remove #(same? old-type %) rels))
                      (not exists?) (conj {:relation/from from :relation/to to
                                           :relation/type new-type :relation/remote true}))))))))

(defn toggle-relations-edit
  "關係預設唯讀；按「編輯」進入編輯狀態，按「完成」離開並關閉關係選取器。"
  [state]
  (if (get-in state [:ui :editing-relations])
    (update state :ui assoc :editing-relations false :rel-picker nil)
    (assoc-in state [:ui :editing-relations] true)))

(defn open-rel-picker [state]
  (update state :ui assoc :rel-picker {:type :basis :query ""} :picker nil))

(defn close-rel-picker [state]
  (update state :ui dissoc :rel-picker))

(defn set-rel-picker-type [state type]
  (if (get-in state [:ui :rel-picker])
    (assoc-in state [:ui :rel-picker :type] type)
    state))

(defn set-rel-picker-query [state q]
  (if (get-in state [:ui :rel-picker])
    (assoc-in state [:ui :rel-picker :query] (or q ""))
    state))

;; ---------------------------------------------------------------------------
;; 素材選取器（段落編輯區）

(defn open-picker [state]
  (update state :ui assoc :picker {:query "" :filter :all} :rel-picker nil))

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

(defn toggle-source-file
  "素材庫依來源檔篩選；再點一次取消。"
  [state source]
  (update-in state [:ui :source-file] #(when-not (= % source) source)))

(defn select-block [state block-id]
  (assoc-in state [:ui :library-selected] block-id))

(defn set-block-status
  "候選區塊的確認、捨棄或恢復。"
  [state block-id status]
  (-> state
      (remember ({:confirmed "確認素材" :discarded "捨棄素材" :candidate "恢復素材"} status "變更素材狀態"))
      (update :blocks
              (fn [blocks]
                (mapv #(if (= block-id (:block/id %)) (assoc % :block/status status) %) blocks)))))

(defn open-paragraph
  "從素材庫跳回撰寫頁的某張卡片。"
  [state id]
  (-> state (set-page :tree) (select id)))
