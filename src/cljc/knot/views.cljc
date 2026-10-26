(ns knot.views
  "主編輯畫面（SPEC 6 的 01-main-editor）。畫面是狀態的純函式，事件以 Nexus action 描述。"
  (:require [clojure.string :as str]
            [knot.layout :as layout]
            [knot.tree :as tree]))

(def column-gap 12)

(def status-pill
  {:ai {:label "AI 生成" :class "pill-ai"}
   :human {:label "人工改過" :class "pill-human"}
   :locked {:label "鎖定" :class "pill-locked"}
   :none {:label "尚未生成" :class "pill-none"}})

(def status-note
  {:ai "本文由 AI 生成，可直接修改"
   :human "本文已手動修改；重新生成前會先顯示差異"
   :locked "已鎖定，重新生成與修改建議都不會改寫本段"
   :none "尚未生成本文"})

(def relation-label
  {:progression "遞進" :causal "因果" :contrast "對比" :example "舉例" :summarizes "摘要"})

(defn- gist [s]
  (str/replace (or s "") "；" "，"))

(defn- grip []
  [:span.grip {:aria-hidden "true"}
   [:svg {:width 8 :height 14 :viewBox "0 0 8 14" :fill "currentColor"}
    (for [[x y] [[2 2] [6 2] [2 7] [6 7] [2 12] [6 12]]]
      [:circle {:replicant/key [x y] :cx x :cy y :r 1.2}])]])

(defn- kind-label [nodes id]
  (case (tree/kind nodes id)
    :heading (case (tree/depth nodes id) 1 "章" 2 "節" (str "第 " (tree/depth nodes id) " 層標題"))
    :intro "引言"
    :paragraph "段落"
    :table "表格"
    :figure "圖"))

(defn- crumb-titles [nodes title id]
  (mapv title (reverse (tree/ancestor-ids nodes id))))

;; ---------------------------------------------------------------------------
;; 本文（唯讀）

(defn- xref-text [label]
  (str (if (str/starts-with? label "fig:") "圖 " "表 ") label))

(defn- render-inline [facts {:keys [type text attrs]}]
  (case type
    "text" text
    "fact" (let [{:fact/keys [label value unit]} (get facts (:key attrs))]
             (if value
               [:span.chip.chip-fact {:title (str label "（" (:key attrs) "）")} value unit]
               [:span.chip.chip-warn {:title "找不到這個事實"} (:key attrs)]))
    "ref" [:span.chip.chip-ref (xref-text (:label attrs))]
    nil))

(defn prose-view [facts doc]
  (for [[i block] (map-indexed vector (:content doc))]
    [:p.prose {:replicant/key i}
     (map #(render-inline facts %) (:content block))]))

;; ---------------------------------------------------------------------------
;; 頂部列

(defn- topbar [{:keys [report nodes ui]} title]
  (let [sel (:selected ui)
        crumbs (conj (crumb-titles nodes title sel) (title sel))]
    [:header.topbar
     [:div.topbar-title
      [:span.muted.small (:report/title report) " · " (:report/subtitle report)]
      [:span.crumb (str/join " / " crumbs)]]
     [:div.segmented {:role "group" :aria-label "檢視模式"}
      (for [[view label] [[:tree "卡片樹"] [:fulltext "全文"]]]
        [:button {:replicant/key view
                  :type "button"
                  :aria-pressed (str (= view (:view ui)))
                  :on {:click [[:ui/set-view view]]}}
         label])]
     [:button.btn.btn-amber {:type "button" :disabled true :title "M5 實作修改建議審查"}
      [:span.badge (:report/pending-proposals report)] "修改建議待審"]
     [:button.btn {:type "button" :disabled true :title "M7 實作交接檢查"} "交接檢查"]
     [:button.btn.btn-primary {:type "button" :disabled true :title "M6 實作 LaTeX 匯出"} "匯出 LaTeX"]]))

;; ---------------------------------------------------------------------------
;; 素材庫

(defn- block-matches? [query {:block/keys [title text source] :as block}]
  (or (str/blank? query)
      (some #(str/includes? (or % "") query)
            (concat [title text source] (:table/columns block)))))

(defn- block-card [cited {:block/keys [id kind status title text source location ai-split] :as block}]
  (let [candidate? (= :candidate status)
        entity? (= :entity (:table/kind block))
        cols (:table/columns block)
        rows (:table/rows block)]
    [:article.block {:replicant/key id
                     :class (cond candidate? "block-candidate"
                                  (cited id) "block-cited")}
     [:div.block-meta
      [:span.tag {:class (when (= :table kind) "tag-navy")}
       (cond entity? "實體表" (= :table kind) "表格" :else "段落")]
      [:span (str source (when location (str " · " location)))]
      (when-not entity?
        [:span.block-status {:class (if candidate? "text-amber" "text-teal")}
         (if candidate? "未確認" "已確認")])]
     [:span.block-title title]
     (if (and (= :table kind) (not entity?))
       (list
        [:div.table-preview {:replicant/key "preview"
                             :style {:grid-template-columns (str "repeat(" (min 3 (count cols)) ", minmax(0, 1fr))")}}
         (for [c (take 3 cols)] [:span.th {:replicant/key c} c])
         (for [[ri row] (map-indexed vector (take 2 rows))
               [ci cell] (map-indexed vector (take 3 row))]
           [:span {:replicant/key [ri ci]} cell])]
        [:span.muted.small {:replicant/key "dims"}
         (str (count cols) " 欄 × " (count rows) " 列")])
       (when text [:span.block-text text]))
     (when ai-split
       [:span.small.text-amber "AI 自動切出，尚未人工確認"])]))

(defn- library [{:keys [blocks nodes ui]}]
  (let [{:keys [source-filter source-query selected]} ui
        cited (set (map :block (get-in nodes [selected :node/citations])))
        confirmed (count (filter #(= :confirmed (:block/status %)) blocks))
        unconfirmed (- (count blocks) confirmed)
        shown (->> blocks
                   (filter (case source-filter
                             :para #(= :para (:block/kind %))
                             :table #(= :table (:block/kind %))
                             :candidate #(= :candidate (:block/status %))
                             any?))
                   (filter #(block-matches? (str/trim source-query) %)))]
    [:aside.library {:aria-label "素材庫"}
     [:div.panel-head
      [:h2 "素材庫"]
      [:span.muted.small (str "已確認 " confirmed " / 共 " (count blocks) " 區塊")]]
     [:div.field
      [:label.muted.small {:for "src-search"} "搜尋素材"]
      [:input#src-search {:type "search"
                          :placeholder "關鍵字、檔名或欄位"
                          :value source-query
                          :on {:input [[:ui/set-source-query [:event/target-value]]]}}]]
     [:div.chips {:role "group" :aria-label "篩選"}
      (for [[f label] [[:all "全部"] [:para "段落"] [:table "表格"] [:candidate (str "未確認 " unconfirmed)]]]
        [:button.chip-btn {:replicant/key f
                           :type "button"
                           :class (when (= f :candidate) "chip-btn-amber")
                           :aria-pressed (str (= f source-filter))
                           :on {:click [[:ui/set-source-filter f]]}}
         label])]
     [:div.blocks
      (if (seq shown)
        (map #(block-card cited %) shown)
        [:span.muted.small "沒有符合的素材區塊"])]
     [:button.btn.btn-dashed {:type "button" :disabled true :title "M3 實作素材匯入"} "＋ 匯入來源檔"]]))

;; ---------------------------------------------------------------------------
;; 卡片樹

(defn- card-role [selected anc desc nodes id]
  (cond
    (= id selected) :active
    (or (anc id) (desc id)) :path
    (= (tree/parent nodes id) (tree/parent nodes selected)) :sibling
    :else :other))

(def role-opacity {:active 1 :path 1 :sibling 0.7 :other 0.35})

(defn- tree-card [{:keys [nodes title role drag over]} {:keys [id top height]}]
  (let [node (get nodes id)
        k (:node/kind node)
        dragging? (= id drag)
        into? (and (= :into (:pos over)) (= id (:id over)))]
    [:div.card {:replicant/key id
                :draggable "true"
                :class (cond-> [(str "card-" (name role)) (str "card-kind-" (name k))]
                         dragging? (conj "card-dragging")
                         into? (conj "card-into"))
                :style {:top (str top "px")
                        :height (str height "px")
                        :opacity (cond into? 1 dragging? 0.3 :else (role-opacity role))}
                :on {:dragstart [[:drag/start id]]
                     :dragover [[:drag/over id [:event/half]]]
                     :drop [[:drag/drop]]
                     :dragend [[:drag/end]]}}
     (grip)
     [:button.card-body {:type "button"
                         :aria-current (when (= role :active) "true")
                         :on {:click [[:ui/select id]]}}
      (case k
        (:table :figure)
        (list
         [:span.card-line {:replicant/key "t"}
          [:span.mono.text-navy (if (= k :table) "表" "圖")]
          [:span.card-title (title id)]]
         [:span.mono.muted.small {:replicant/key "l"} (:node/label node)])

        (list
         [:span.card-line {:replicant/key "t"}
          [:span.card-title (title id)]
          (when-let [{:keys [label class]} (status-pill (:node/status node))]
            [:span.pill {:class class} label])]
         [:span.card-gist {:replicant/key "g"}
          (gist (:node/draft node))]))]]))

(defn- relation-chips [{:keys [nodes relations]} col roles]
  (for [{:keys [id top]} col
        :let [prev (tree/prev-paragraph nodes id)]
        :when (and prev (tree/paragraph? nodes id))
        :let [rel (tree/relation-between relations prev id)]
        :when rel]
    [:span.relation {:replicant/key (str "rel-" id)
                     :style {:top (str (- top 25) "px")
                             :opacity (max (role-opacity (roles id)) (role-opacity (roles prev)))}}
     [:span.relation-line]
     [:span.relation-chip {:class (when (:relation/pending rel) "relation-pending")}
      (if (:relation/pending rel)
        (str (some-> (:relation/type rel) relation-label (str " · ")) "待確認")
        (relation-label (:relation/type rel)))]]))

(defn- insert-marker [col {:keys [id pos]}]
  (when (#{:before :after} pos)
    (let [cards (vec col)
          i (first (keep-indexed #(when (= id (:id %2)) %1) cards))]
      (when i
        (let [{:keys [top height gap-before]} (nth cards i)
              nxt (get cards (inc i))
              y (if (= pos :before)
                  (- top (/ (max gap-before 8) 2))
                  (+ top height (/ (max (or (:gap-before nxt) 0) 8) 2)))]
          [:span.insert-marker {:style {:top (str (- y 2) "px")}}])))))

(defn- tree-panel [{:keys [nodes ui] :as state} title]
  (let [{:keys [selected drag over bias viewport-h toast]} ui
        cols (layout/columns nodes)
        offs (layout/offsets nodes cols selected viewport-h)
        anc (set (tree/ancestor-ids nodes selected))
        desc (set (tree/descendant-ids nodes selected))
        roles (memoize #(card-role selected anc desc nodes %))
        sel-depth (tree/depth nodes selected)]
    [:section.tree-panel {:aria-label "卡片樹"}
     [:div.panel-head
      [:h2 "卡片樹"]
      [:span.muted.small "拖曳卡片調整順序；拖到上一層的卡片上可移入；方向鍵切換卡片，Ctrl／⌘+Z 復原"]]
     [:div.legend
      [:span [:i.swatch.swatch-active] "目前選取"]
      [:span [:i.swatch.swatch-path] "上層與下層"]
      [:span {:style {:opacity 0.7}} [:i.swatch] "同一層的兄弟"]
      [:span {:style {:opacity 0.35}} [:i.swatch] "其他內容"]]
     (when toast
       [:div.toast {:role "status"}
        [:span toast]
        [:button.toast-close {:type "button" :aria-label "關閉訊息" :on {:click [[:ui/dismiss-toast]]}} "×"]])
     [:div.tree-scroll {:class (when drag "is-dragging")
                        :replicant/on-render [[:dom/reveal-columns sel-depth selected (count cols)]]}
      [:div.tree-columns {:style {:column-gap (str column-gap "px")}}
       (for [[i col] (map-indexed vector cols)]
         [:div.tree-col {:replicant/key i}
          [:span.col-label.muted.small (case i 0 "章" 1 "節" (str "第 " (inc i) " 層"))]
          [:div.col-viewport {:style {:height (str viewport-h "px")}
                              :on {:dragover [[:ui/edge-scroll i [:event/edge-delta]]]}}
           [:div.tree-inner {:style {:transform (str "translateY(" (+ (nth offs i) (get bias i 0)) "px)")}}
            (relation-chips state col roles)
            (for [c col]
              (tree-card {:nodes nodes :title title :role (roles (:id c)) :drag drag :over over} c))
            (when (and over (some #(= (:id over) (:id %)) col))
              (insert-marker col over))]]])]]]))

;; ---------------------------------------------------------------------------
;; 全文視圖

(defn- fulltext [{:keys [nodes facts ui]} title]
  (let [selected (:selected ui)]
    [:section.fulltext {:aria-label "全文"}
     [:div.fulltext-page
      (for [id (tree/document-order nodes)
            :let [node (get nodes id)]]
        (case (:node/kind node)
          :heading
          (let [d (tree/depth nodes id)]
            [(keyword (str "h" (inc d))) {:replicant/key id :class "ft-heading"} (title id)])

          :intro
          [:div.ft-block.ft-intro {:replicant/key id}
           (if-let [doc (:node/prose node)]
             (prose-view facts doc)
             [:p.prose.muted (str "（引言尚未生成：" (gist (:node/draft node)) "）")])]

          :paragraph
          [:div.ft-block {:replicant/key id
                          :role "button"
                          :tabindex 0
                          :class (when (= id selected) "ft-selected")
                          :on {:click [[:ui/select id]]}}
           (if-let [doc (:node/prose node)]
             (prose-view facts doc)
             [:p.prose.muted (str "（尚未生成：" (:node/title node) "）")])]

          (:table :figure)
          [:div.ft-float {:replicant/key id}
           [:span.chip.chip-ref (xref-text (:node/label node))]
           [:span (:node/title node)]]))]]))

;; ---------------------------------------------------------------------------
;; 詳情面板

(defn- section [heading & body]
  [:div.detail-section
   [:h3 heading]
   body])

(defn- move-buttons [nodes id]
  (let [up? (tree/sibling-placement nodes id -1)
        down? (tree/sibling-placement nodes id 1)]
    [:div.move-row
     [:button.btn.btn-small {:type "button" :disabled (not up?) :on {:click [[:ui/move-selected -1]]}} "上移"]
     [:button.btn.btn-small {:type "button" :disabled (not down?) :on {:click [[:ui/move-selected 1]]}} "下移"]
     [:span.muted.small "也可以直接拖曳卡片"]]))

(defn- relation-rows [{:keys [nodes relations]} title id]
  (let [prev (tree/prev-paragraph nodes id)
        nxt (let [n (tree/next-sibling nodes id)] (when (tree/paragraph? nodes n) n))
        chip (fn [rel]
               [:span.relation-chip {:class (when (:relation/pending rel) "relation-pending")}
                (if (:relation/pending rel) "待確認" (relation-label (:relation/type rel)))])]
    (concat
     (when-let [rel (and prev (tree/relation-between relations prev id))]
       [[:div.rel-row {:replicant/key "prev"} [:span.rel-kind "承接"] [:span.rel-name (title prev)] (chip rel)]])
     (when-let [rel (and nxt (tree/relation-between relations id nxt))]
       [[:div.rel-row {:replicant/key "next"} [:span.rel-kind "導向"] [:span.rel-name (title nxt)] (chip rel)]])
     (for [r relations
           :when (and (= id (:relation/to r)) (= :summarizes (:relation/type r)))]
       [:div.rel-row {:replicant/key (str "sum-" (:relation/from r))}
        [:span.rel-kind "被引用"]
        [:span.rel-name (title (:relation/from r))]
        [:span.relation-chip "摘要依賴"]])
     (for [k (tree/child-ids nodes id)]
       [:div.rel-row {:replicant/key (str "kid-" k)}
        [:span.rel-kind "下層"]
        [:span.rel-name (title k)]
        [:span.mono.small (get-in nodes [k :node/label])]]))))

(defn- citations [{:keys [blocks]} node]
  (let [by-id (into {} (map (juxt :block/id identity)) blocks)
        cites (:node/citations node)]
    (if (seq cites)
      [:div.cites
       (for [{:keys [block where]} cites
             :let [b (by-id block)
                   warn? (= :candidate (:block/status b))]]
         [:div.cite {:replicant/key block :class (when warn? "cite-warn")}
          [:span.mono.text-navy.small (if (= :table (:block/kind b)) "表" "段")]
          [:span (:block/title b)]
          [:span.cite-where {:class (when warn? "text-amber")}
           (if warn? "未確認素材" (or where (:block/source b)))]])]
      [:span.notice-amber "尚未加入依據。從左側素材庫拖曳區塊到這張卡片（M3）。"])))

(defn- paragraph-detail [{:keys [nodes facts report] :as state} title id]
  (let [node (get nodes id)
        status (or (:node/status node) :none)
        rels (relation-rows state title id)
        parent-id (tree/parent nodes id)]
    (list
     (section "要點"
              [:div.boxed.pre-line (str/replace (or (:node/draft node) "") "；" "\n")])
     (section "依據" (citations state node))
     (section "關係"
              (if (seq rels)
                [:div.rel-rows rels]
                [:span.muted.small "尚未標註與其他段落的關係"]))
     [:div.detail-section {:replicant/key "prose"}
      [:div.section-head
       [:h3 "本文"]
       [:span.muted.small (str "文風：" (:report/style-name report))]]
      (if-let [doc (:node/prose node)]
        [:div.prose-box (prose-view facts doc)]
        [:div.prose-box [:p.prose.muted "尚未生成本文。寫好要點並加入依據後，按「生成本段」產生。"]])
      [:span.muted.small "藍色標記為事實引用，數值來自素材表格的儲存格；灰色為 LaTeX 交互參照。本文編輯於 M4 實作。"]]
     [:div.detail-footer {:replicant/key "footer"}
      [:span.small {:class (if (= :human status) "text-teal" "muted")} (status-note status)]
      (when (and (not (tree/intro-id nodes parent-id))
                 (empty? (tree/child-ids nodes id)))
        [:button.btn {:type "button" :on {:click [[:ui/paragraph->intro id]]}} "改為引言"])
      [:button.btn {:type "button" :on {:click [[:ui/toggle-lock id]]}}
       (if (= :locked status) "解除鎖定" "鎖定")]
      [:button.btn.btn-dark {:type "button" :disabled true :title "M4 實作段落生成"}
       (if (= :none status) "生成本段" "重新生成")]])))

(defn- heading-detail [{:keys [nodes]} title id]
  (let [node (get nodes id)
        kids (tree/child-ids nodes id)
        intro (tree/intro-id nodes id)
        has-paras (some #(tree/paragraph? nodes %) kids)
        has-heads (some #(tree/heading? nodes %) kids)
        can-sub (< (tree/depth nodes id) tree/max-heading-level)]
    (list
     (section "內容摘要" [:div.boxed (gist (:node/draft node))])
     (when-let [items (seq (:node/contract-items node))]
       (section "契約項目"
                [:div.chips (for [c items] [:span.tag {:replicant/key c} c])]))
     (section "引言"
              (if intro
                [:button.kid {:type "button" :on {:click [[:ui/select intro]]}}
                 (gist (get-in nodes [intro :node/draft]))]
                [:button.btn.btn-dashed {:type "button" :on {:click [[:ui/add-child id :intro]]}} "＋ 新增引言"]))
     (section "下層內容"
              [:div.kids
               (for [k kids :when (not= k intro)]
                 [:button.kid {:replicant/key k :type "button" :on {:click [[:ui/select k]]}}
                  (title k)])]
              [:div.add-row
               (when-not has-heads
                 [:button.btn.btn-small {:type "button" :on {:click [[:ui/add-child id :paragraph]]}} "＋ 段落"])
               (when can-sub
                 (if has-paras
                   [:button.btn.btn-small {:type "button" :on {:click [[:ui/wrap-paragraphs id]]}}
                    "＋ 下層標題（把現有段落移入）"]
                   [:button.btn.btn-small {:type "button" :on {:click [[:ui/add-child id :heading]]}} "＋ 下層標題"]))]
              [:span.muted.small "引言之後只能全是段落或全是下層標題；標題最多四層。"]))))

(defn- intro-detail [{:keys [nodes facts]} title id]
  (let [node (get nodes id)]
    (list
     (section "所屬標題" [:button.kid {:type "button" :on {:click [[:ui/select (tree/parent nodes id)]]}}
                      (title (tree/parent nodes id))])
     (section "要點" [:div.boxed (gist (:node/draft node))])
     (section "本文"
              (if-let [doc (:node/prose node)]
                [:div.prose-box (prose-view facts doc)]
                [:div.prose-box [:p.prose.muted "尚未生成。引言適合在下層內容大致完成後才生成。"]])))))

(defn- float-detail [{:keys [nodes]} id]
  (let [node (get nodes id)]
    (list
     (section "內容摘要" [:div.boxed (gist (:node/draft node))])
     (section "LaTeX label"
              [:span.mono.boxed-muted (:node/label node)]
              [:span.muted.small "label 一經建立就不變。匯出時產生獨立檔案，本文以 \\ref 參照。"])
     (when-let [f (:node/figure-file node)]
       (section "圖檔" [:span.mono.boxed-muted f])))))

(defn- detail [{:keys [nodes ui] :as state} title]
  (let [id (:selected ui)
        k (tree/kind nodes id)
        crumbs (crumb-titles nodes title id)]
    [:aside.detail {:aria-label "選取內容詳情"}
     [:div.detail-head
      [:span.muted.small (str (kind-label nodes id) " · " (if (seq crumbs) (str/join " / " crumbs) "報告"))]
      [:h2 (title id)]
      (move-buttons nodes id)]
     (case k
       :paragraph (paragraph-detail state title id)
       :heading (heading-detail state title id)
       :intro (intro-detail state title id)
       (float-detail state id))]))

;; ---------------------------------------------------------------------------

(defn app [state]
  (if-not (:nodes state)
    [:main.app [:div.loading "載入中…"]]
    (let [nodes (:nodes state)
          numbers (tree/numbering nodes)
          title #(tree/display-title nodes numbers %)]
      [:main.app
       (topbar state title)
       [:div.workspace
        (library state)
        (if (= :fulltext (get-in state [:ui :view]))
          (fulltext state title)
          (tree-panel state title))
        (detail state title)]])))
