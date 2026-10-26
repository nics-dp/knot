(ns knot.views
  "畫面（SPEC 6）：撰寫頁（卡片樹與編輯區）與素材庫頁。
  畫面是狀態的純函式，事件以 Nexus action 描述。"
  (:require [clojure.string :as str]
            [knot.context :as context]
            [knot.layout :as layout]
            [knot.tree :as tree]
            [knot.ui :as ui]))

(def column-gap 12)
(def list-limit
  "素材清單一次顯示的上限，避免上千個區塊拖慢畫面。"
  60)

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

(def relation-label context/relation-label)

(def relation-options [:progression :causal :contrast :example])

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

(defn- block-index [blocks]
  (into {} (map (juxt :block/id identity)) blocks))

(defn- block-matches? [query {:block/keys [title text source] :as block}]
  (or (str/blank? query)
      (some #(str/includes? (or % "") query)
            (concat [title text source] (:table/columns block)))))

(defn- filter-blocks [blocks f query]
  (->> blocks
       (filter (case f
                 :para #(= :para (:block/kind %))
                 :table #(= :table (:block/kind %))
                 :candidate #(= :candidate (:block/status %))
                 any?))
       (filter #(block-matches? (str/trim (or query "")) %))))

(defn- block-tag [{:block/keys [kind] :as block}]
  [:span.tag {:class (when (= :table kind) "tag-navy")}
   (cond (= :entity (:table/kind block)) "實體表" (= :table kind) "表格" :else "段落")])

(defn- block-status [{:block/keys [status] :as block}]
  (when-not (= :entity (:table/kind block))
    (case status
      :candidate [:span.block-status.text-amber "未確認"]
      :discarded [:span.block-status.muted "已捨棄"]
      [:span.block-status.text-teal "已確認"])))

(defn- filter-chips [blocks current action]
  (let [unconfirmed (count (filter #(= :candidate (:block/status %)) blocks))]
    [:div.chips {:role "group" :aria-label "篩選"}
     (for [[f label] [[:all "全部"] [:para "段落"] [:table "表格"] [:candidate (str "未確認 " unconfirmed)]]]
       [:button.chip-btn {:replicant/key f
                          :type "button"
                          :class (when (= f :candidate) "chip-btn-amber")
                          :aria-pressed (str (= f current))
                          :on {:click [[action f]]}}
        label])]))

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

(def tabs [[:library "素材"] [:tree "卡片"] [:fulltext "全文"]])

(defn- topbar [{:keys [report nodes ui]} title]
  (let [{:keys [page selected]} ui
        crumb (if (= page :library)
                "素材庫"
                (str/join " / " (conj (crumb-titles nodes title selected) (title selected))))]
    [:header.topbar
     [:div.topbar-title
      [:span.muted.small (:report/title report) " · " (:report/subtitle report)]
      [:span.crumb crumb]]
     [:nav.segmented {:aria-label "頁籤"}
      (for [[p label] tabs]
        [:button {:replicant/key p
                  :type "button"
                  :aria-pressed (str (= p page))
                  :on {:click [[:ui/set-page p]]}}
         label])]
     [:button.btn.btn-amber {:type "button" :disabled true :title "M5 實作修改建議審查"}
      [:span.badge (:report/pending-proposals report)] "修改建議待審"]
     [:button.btn {:type "button" :disabled true :title "M7 實作交接檢查"} "交接檢查"]
     [:button.btn.btn-primary {:type "button" :disabled true :title "M6 實作 LaTeX 匯出"} "匯出 LaTeX"]]))

(defn- toast-view [toast]
  (when toast
    [:div.toast {:role "status"}
     [:span toast]
     [:button.toast-close {:type "button" :aria-label "關閉訊息" :on {:click [[:ui/dismiss-toast]]}} "×"]]))

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
                     :dragend [[:drag/end]]}}
     (grip)
     ;; 用 div 而非 button：Firefox 不能從 button 開始拖曳
     [:div.card-body {:role "button"
                      :tabindex 0
                      :data-id id
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
          (if (and (= k :intro) (str/blank? (:node/draft node)))
            "（由 AI 依下層內容生成）"
            (gist (:node/draft node)))]))]]))

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

(defn- tree-toolbar
  "在選取卡片的層級上新增或刪除。"
  [{:keys [nodes ui]}]
  (let [id (:selected ui)
        k (ui/sibling-kind nodes id)]
    [:div.toolbar {:role "toolbar" :aria-label "新增與刪除卡片"}
     [:button.btn.btn-primary {:type "button"
                             :disabled (nil? k)
                             :title (if k
                                      (str "在「" (tree/display-title nodes (tree/numbering nodes) id) "」後面新增")
                                      "表格與圖的 label 命名方式待定，暫不提供新增")
                             :on {:click [[:ui/add-sibling id]]}}
      "新增"]
     [:button.btn.btn-danger {:type "button"
                                        :title "刪除選取的卡片與其下層卡片；可以復原"
                                        :on {:click [[:ui/delete-node id]]}}
      "刪除"]]))

(defn- scroll-range
  "這一欄 bias 的允許範圍：內容頂端不低於上緣、底端不高於下緣（已在範圍外時不再更遠）。"
  [offset col-h viewport-h]
  (let [t-max (max offset layout/top-margin)
        t-min (min offset (- viewport-h col-h layout/top-margin))]
    [(- t-min offset) (- t-max offset)]))

(defn- tree-panel [{:keys [nodes ui] :as state} title]
  (let [{:keys [selected drag over bias viewport-h toast]} ui
        cols (layout/columns nodes)
        offs (layout/offsets nodes cols selected viewport-h)
        anc (set (tree/ancestor-ids nodes selected))
        desc (set (tree/descendant-ids nodes selected))
        roles (memoize #(card-role selected anc desc nodes %))
        sel-depth (tree/depth nodes selected)
        drag-depth (when drag (tree/depth nodes drag))]
    [:section.tree-panel {:aria-label "卡片"}
     [:header.board-head
      [:h2.area-title "卡片"]
      (tree-toolbar state)
      [:div.legend {:aria-label "圖例"}
       [:span [:i.swatch.swatch-active] "目前選取"]
       [:span [:i.swatch.swatch-path] "上層與下層"]
       [:span [:i.swatch.swatch-sibling] "同一層"]
       [:span [:i.swatch.swatch-other] "其他"]]]
     [:div.board
      (when-not drag (toast-view toast))
      [:div.tree-scroll {:class (when drag "is-dragging")
                        :replicant/on-mount [[:dom/observe-viewport]]
                        :replicant/on-render [[:dom/reveal-columns sel-depth selected (count cols)]]}
      [:div.tree-columns {:style {:column-gap (str column-gap "px")}}
       (for [[i col] (map-indexed vector cols)
             :let [[lo hi] (scroll-range (nth offs i) (layout/column-height col) viewport-h)
                   translate (+ (nth offs i) (get bias i 0))]]
         [:div.tree-col {:replicant/key i
                         :class (when drag
                                  (condp = (inc i)
                                    drag-depth "col-drop-sibling"
                                    (dec drag-depth) "col-drop-into"
                                    "col-drop-none"))}
          [:div.col-head
           [:span.col-level (inc i)]
           [:span.col-name (case i 0 "章" 1 "節" (str "第 " (inc i) " 層"))]
           ;; 拖曳時以可否放置的標籤取代卡片數
           (condp = (when drag (inc i))
             drag-depth [:span.col-drop-tag "調整順序"]
             (some-> drag-depth dec) [:span.col-drop-tag "移入"]
             [:span.col-count (str (count col) " 張")])]
          [:div.col-viewport {:on {:wheel [[:ui/wheel-column i [:event/wheel-delta] lo hi]]
                                   :dragover [[:drag/over i [:event/viewport-y] translate]
                                              [:ui/edge-scroll i [:event/edge-delta] lo hi]]
                                   :drop [[:drag/drop]]}}
           [:div.tree-inner {:style {:transform (str "translateY(" translate "px)")}}
            (relation-chips state col roles)
            (for [c col]
              (tree-card {:nodes nodes :title title :role (roles (:id c)) :drag drag :over over} c))
            (when (and over (some #(= (:id over) (:id %)) col))
              (insert-marker col over))]]])]]]]))

;; ---------------------------------------------------------------------------
;; 全文視圖

(defn- fulltext [{:keys [nodes facts ui]} title]
  (let [selected (:selected ui)]
    [:section.fulltext {:aria-label "全文"}
     (toast-view (:toast ui))
     [:div.fulltext-page
      (for [id (tree/document-order nodes)
            :let [node (get nodes id)]]
        (case (:node/kind node)
          :heading
          (let [d (tree/depth nodes id)]
            [(keyword (str "h" (inc d))) {:replicant/key id :class "ft-heading"} (title id)])

          :intro
          [:div.ft-block.ft-intro {:replicant/key id
                                   :class (when (= id selected) "ft-selected")
                                   :on {:click [[:ui/select id]]}}
           (if-let [doc (:node/prose node)]
             (prose-view facts doc)
             [:p.prose.muted "（引言尚未生成，將由 AI 依下層內容補上）"])]

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
;; 編輯區

(defn- section [heading & body]
  [:div.detail-section
   (if (vector? heading) heading [:h3 heading])
   body])

(defn- move-buttons [nodes id]
  (let [up? (tree/sibling-placement nodes id -1)
        down? (tree/sibling-placement nodes id 1)]
    [:div.move-row
     [:button.btn.btn-small {:type "button" :disabled (not up?) :on {:click [[:ui/move-selected -1]]}} "上移"]
     [:button.btn.btn-small {:type "button" :disabled (not down?) :on {:click [[:ui/move-selected 1]]}} "下移"]]))

(defn- name-input [id field value placeholder focus?]
  [:input.title-input {:replicant/key (str id field)
                       :replicant/on-mount (when focus? [[:dom/select-all] [:ui/clear-focus-title]])
                       :type "text"
                       :aria-label "名稱"
                       :placeholder placeholder
                       :value (or value "")
                       :on {:input [[:ui/set-node-field id field [:event/target-value]]]
     :compositionend [[:ui/set-node-field id field [:event/target-value]]]}}])

(defn- intent-input [id value placeholder]
  (section [:div.section-head [:h3 "意圖"]
            [:span.muted.small "AI 生成時的提示詞，不會出現在報告中"]]
           [:textarea.intent {:replicant/key (str id "-intent")
                              :rows 4
                              :placeholder placeholder
                              :value (or value "")
                              :on {:input [[:ui/set-node-field id :node/draft [:event/target-value]]]
     :compositionend [[:ui/set-node-field id :node/draft [:event/target-value]]]}}]))

(defn- relation-select [from to rel]
  [:div.rel-control
   [:select.rel-select {:aria-label "關係類型"
                        :on {:change [[:ui/set-relation from to [:event/target-keyword]]]}}
    [:option {:value "" :selected (nil? (:relation/type rel))} "未標註"]
    (for [t relation-options]
      [:option {:replicant/key t :value (name t) :selected (= t (:relation/type rel))}
       (relation-label t)])]
   (when (:relation/pending rel)
     (if (:relation/type rel)
       [:button.btn.btn-small.btn-amber {:type "button"
                                         :on {:click [[:ui/set-relation from to (:relation/type rel)]]}}
        "確認"]
       [:span.relation-chip.relation-pending "待確認"]))])

(defn- relation-rows [{:keys [nodes relations]} title id]
  (let [prev (tree/prev-paragraph nodes id)
        nxt (let [n (tree/next-sibling nodes id)] (when (tree/paragraph? nodes n) n))]
    (concat
     (when prev
       [[:div.rel-row {:replicant/key "prev"} [:span.rel-kind "承接"] [:span.rel-name (title prev)]
         (relation-select prev id (tree/relation-between relations prev id))]])
     (when nxt
       [[:div.rel-row {:replicant/key "next"} [:span.rel-kind "導向"] [:span.rel-name (title nxt)]
         (relation-select id nxt (tree/relation-between relations id nxt))]])
     (for [r relations
           :when (and (= id (:relation/to r)) (= :summarizes (:relation/type r)))]
       [:div.rel-row {:replicant/key (str "sum-" (:relation/from r))}
        [:span.rel-kind "被引用"]
        [:button.link {:type "button" :on {:click [[:ui/select (:relation/from r)]]}}
         (title (:relation/from r))]
        [:span.relation-chip "摘要依賴"]])
     (for [k (tree/child-ids nodes id)]
       [:div.rel-row {:replicant/key (str "kid-" k)}
        [:span.rel-kind "下層"]
        [:button.link {:type "button" :on {:click [[:ui/select k]]}} (title k)]
        [:span.mono.small (get-in nodes [k :node/label])]]))))

(defn- picker [{:keys [blocks ui]} node]
  (let [{:keys [query filter]} (:picker ui)
        linked (set (map :block (:node/citations node)))
        shown (remove #(= :discarded (:block/status %)) (filter-blocks blocks filter query))
        more (- (count shown) list-limit)]
    [:div.picker {:role "dialog" :aria-label "選取素材"}
     [:div.picker-head
      [:input.picker-search {:type "search"
                             :placeholder "搜尋素材：關鍵字、檔名或欄位"
                             :value query
                             :replicant/on-mount [[:dom/focus]]
                             :on {:input [[:ui/set-picker-query [:event/target-value]]]
     :compositionend [[:ui/set-picker-query [:event/target-value]]]}}]
      [:button.btn.btn-small {:type "button" :on {:click [[:ui/close-picker]]}} "完成"]]
     (filter-chips blocks filter :ui/set-picker-filter)
     [:div.picker-list
      (if (seq shown)
        (for [{:block/keys [id title source location] :as b} (take list-limit shown)]
          [:div.picker-row {:replicant/key id}
           (block-tag b)
           [:span.picker-title title]
           [:span.muted.small.picker-source (str source (when location (str " · " location)))]
           (block-status b)
           (if (linked id)
             [:span.small.text-teal "已加入"]
             [:button.btn.btn-small {:type "button" :on {:click [[:ui/add-citation (:node/id node) id]]}} "加入"])])
        [:span.muted.small "沒有符合的素材區塊"])]
     (when (pos? more)
       [:span.muted.small (str "還有 " more " 個符合的區塊")])]))

(defn- citations [{:keys [blocks ui] :as state} node]
  (let [by-id (block-index blocks)
        cites (:node/citations node)
        id (:node/id node)]
    (list
     (if (seq cites)
       [:div.cites {:replicant/key "cites"}
        (for [{:keys [block where]} cites
              :let [b (by-id block)
                    warn? (= :candidate (:block/status b))]]
          [:div.cite {:replicant/key block :class (when warn? "cite-warn")}
           [:span.mono.text-navy.small (if (= :table (:block/kind b)) "表" "段")]
           [:span.cite-title (:block/title b)]
           [:span.cite-where {:class (when warn? "text-amber")}
            (if warn? "未確認素材" (or where (:block/source b)))]
           [:button.cite-remove {:type "button" :aria-label "移除依據"
                                 :on {:click [[:ui/remove-citation id block]]}} "×"]])]
       [:span.notice-amber {:replicant/key "empty"} "尚未加入依據。"])
     (if (:picker ui)
       (picker state node)
       [:button.btn.btn-dashed {:replicant/key "add" :type "button" :on {:click [[:ui/open-picker]]}}
        "加入依據"]))))

(defn- context-preview [state id]
  (when-let [{:keys [sections chars]} (context/generation-context state id)]
    [:details.context
     [:summary
      [:span "生成時 AI 會參考的內容"]
      [:span.muted.small (str "約 " chars " 字")]]
     (for [{:keys [title entries]} sections]
       [:div.context-section {:replicant/key title}
        [:h4 title]
        (for [[i {:keys [label text relation warn]}] (map-indexed vector entries)]
          [:div.context-entry {:replicant/key i :class (when warn "cite-warn")}
           [:span.context-label label (when relation [:span.relation-chip relation])]
           [:span.context-text (if (str/blank? text) "（未填意圖）" (gist text))]])])]))

(defn- paragraph-editor [{:keys [nodes facts report] :as state} title id]
  (let [node (get nodes id)
        status (or (:node/status node) :none)
        rels (relation-rows state title id)
        parent-id (tree/parent nodes id)]
    (list
     (intent-input id (:node/draft node) "這段要說什麼、要強調什麼、用什麼語氣。可以分行列點。")
     (section "依據" (citations state node))
     (section "關係"
              (if (seq rels)
                [:div.rel-rows rels]
                [:span.muted.small "沒有相鄰的段落"]))
     [:div.detail-section {:replicant/key "prose"}
      [:div.section-head
       [:h3 "本文"]
       [:span.muted.small (str "文風：" (:report/style-name report))]]
      (if-let [doc (:node/prose node)]
        [:div.prose-box (prose-view facts doc)]
        [:div.prose-box [:p.prose.muted "尚未生成本文。寫好意圖並加入依據後，按「生成本段」產生。"]])]
     (context-preview state id)
     [:div.detail-footer {:replicant/key "footer"}
      [:span.small {:class (if (= :human status) "text-teal" "muted")} (status-note status)]
      (when (and (not (tree/intro-id nodes parent-id))
                 (empty? (tree/child-ids nodes id)))
        [:button.btn {:type "button" :on {:click [[:ui/paragraph->intro id]]}} "改為引言"])
      [:button.btn {:type "button" :on {:click [[:ui/toggle-lock id]]}}
       (if (= :locked status) "解除鎖定" "鎖定")]
      [:button.btn.btn-dark {:type "button" :disabled true :title "M4 實作段落生成"}
       (if (= :none status) "生成本段" "重新生成")]])))

(defn- heading-editor [{:keys [nodes]} title id]
  (let [node (get nodes id)
        kids (tree/child-ids nodes id)
        intro (tree/intro-id nodes id)
        has-paras (some #(tree/paragraph? nodes %) kids)
        has-heads (some #(tree/heading? nodes %) kids)
        can-sub (< (tree/depth nodes id) tree/max-heading-level)]
    (list
     (intent-input id (:node/draft node) "這一章／節要涵蓋什麼、回應哪些契約項目、與前後章節的關係。")
     (when-let [items (seq (:node/contract-items node))]
       (section "契約項目"
                [:div.chips (for [c items] [:span.tag {:replicant/key c} c])]))
     (section "引言"
              (if intro
                [:button.kid {:type "button" :on {:click [[:ui/select intro]]}}
                 (let [d (get-in nodes [intro :node/draft])]
                   (if (str/blank? d) "（由 AI 依下層內容生成）" (gist d)))]
                [:button.btn.btn-dashed {:type "button" :on {:click [[:ui/add-child id :intro]]}}
                 "新增引言"]))
     (section "下層內容"
              [:div.kids
               (for [k kids :when (not= k intro)]
                 [:button.kid {:replicant/key k :type "button" :on {:click [[:ui/select k]]}}
                  (title k)])]
              [:div.add-row
               (when-not has-heads
                 [:button.btn.btn-small {:type "button" :on {:click [[:ui/add-child id :paragraph]]}} "新增段落"])
               (when can-sub
                 (if has-paras
                   [:button.btn.btn-small {:type "button" :on {:click [[:ui/wrap-paragraphs id]]}}
                    "新增下層標題並移入段落"]
                   [:button.btn.btn-small {:type "button" :on {:click [[:ui/add-child id :heading]]}} "新增下層標題"]))]))))

(defn- intro-editor [{:keys [nodes facts] :as state} title id]
  (let [node (get nodes id)
        heading (tree/parent nodes id)]
    (list
     (section "所屬標題"
              [:button.kid {:type "button" :on {:click [[:ui/select heading]]}} (title heading)])
     (intent-input id (:node/draft node) "可留空。想強調的重點或語氣寫在這裡，AI 會依下層內容生成引言。")
     (section "本文"
              (if-let [doc (:node/prose node)]
                [:div.prose-box (prose-view facts doc)]
                [:div.prose-box [:p.prose.muted "尚未生成。引言適合在下層內容大致完成後才生成。"]]))
     (context-preview state id)
     [:div.detail-footer {:replicant/key "footer"}
      [:span.small.muted "引言由 AI 依這一層的下層內容生成"]
      [:button.btn.btn-dark {:type "button" :disabled true :title "M4 實作引言生成"} "AI 生成引言"]])))

(defn- float-editor [{:keys [nodes]} id]
  (let [node (get nodes id)]
    (list
     (intent-input id (:node/draft node) "這張表／圖要呈現什麼、圖說的重點。")
     (section "LaTeX label"
              [:span.mono.boxed-muted (:node/label node)])
     (when-let [f (:node/figure-file node)]
       (section "圖檔" [:span.mono.boxed-muted f])))))

(defn- editor-pane [{:keys [nodes ui] :as state} title]
  (let [id (:selected ui)
        node (get nodes id)
        k (:node/kind node)
        crumbs (crumb-titles nodes title id)
        focus? (= id (:focus-title ui))]
    [:aside.editor {:aria-label "編輯區"}
     [:div.detail-head
      [:span.muted.small (str (kind-label nodes id) " · " (if (seq crumbs) (str/join " / " crumbs) "報告"))]
      (case k
        :heading [:div.title-row
                  [:span.title-number (some-> (title id) (str/split #" " 2) first)]
                  (name-input id :node/name (:node/name node) "標題名稱" focus?)]
        :intro [:h2 "引言"]
        (name-input id :node/title (:node/title node) "名稱" focus?))
      (move-buttons nodes id)]
     (case k
       :paragraph (paragraph-editor state title id)
       :heading (heading-editor state title id)
       :intro (intro-editor state title id)
       (float-editor state id))]))

;; ---------------------------------------------------------------------------
;; 素材庫頁面

(defn- table-view
  "表格區塊的內容；max-cols、max-rows 為 nil 時顯示全部。"
  [block & [max-cols max-rows]]
  (let [cols (cond->> (:table/columns block) max-cols (take max-cols))
        rows (cond->> (:table/rows block) max-rows (take max-rows))]
    [:div.table-preview {:style {:grid-template-columns (str "repeat(" (count cols) ", minmax(0, 1fr))")}}
     (for [c cols] [:span.th {:replicant/key c} c])
     (for [[ri row] (map-indexed vector rows)
           [ci cell] (map-indexed vector (take (count cols) row))]
       [:span {:replicant/key [ri ci]} cell])]))

(defn- citing-nodes [nodes block-id]
  (->> (vals nodes)
       (filter #(some (fn [c] (= block-id (:block c))) (:node/citations %)))
       (map :node/id)))

(defn- library-card [nodes selected {:block/keys [id status title text source location ai-split] :as block}]
  (let [n (count (citing-nodes nodes id))]
    [:button.block {:replicant/key id
                    :type "button"
                    :class (cond-> []
                             (= :candidate status) (conj "block-candidate")
                             (= :discarded status) (conj "block-discarded")
                             (= id selected) (conj "block-selected"))
                    :on {:click [[:ui/select-block id]]}}
     [:span.block-meta
      (block-tag block)
      [:span (str source (when location (str " · " location)))]
      (block-status block)]
     [:span.block-title title]
     (cond
       (and (= :table (:block/kind block)) (not= :entity (:table/kind block)))
       (list (table-view block 3 2)
             [:span.muted.small {:replicant/key "dims"}
              (str (count (:table/columns block)) " 欄 × " (count (:table/rows block)) " 列")])
       text [:span.block-text text])
     [:span.small {:class (if (pos? n) "text-navy" "muted")}
      (if (pos? n) (str "被 " n " 個段落引用") "尚未被引用")
      (when ai-split [:span.text-amber " · AI 自動切出"])]]))


(defn- library-detail [{:keys [nodes] :as _state} title block]
  (if-not block
    [:aside.lib-detail [:p.muted "選取左側的素材區塊查看內容。"]]
    (let [{:block/keys [id status source location text]} block
          citing (citing-nodes nodes id)]
      [:aside.lib-detail
       [:div.detail-head
        [:span.block-meta (block-tag block) [:span (str source (when location (str " · " location)))] (block-status block)]
        [:h2 (:block/title block)]]
       (when (#{:candidate :discarded} status)
         [:div.move-row
          (when (= :candidate status)
            [:button.btn.btn-small.btn-primary {:type "button" :on {:click [[:ui/set-block-status id :confirmed]]}} "確認"])
          (if (= :discarded status)
            [:button.btn.btn-small {:type "button" :on {:click [[:ui/set-block-status id :candidate]]}} "恢復為候選"]
            [:button.btn.btn-small {:type "button" :on {:click [[:ui/set-block-status id :discarded]]}} "捨棄"])])
       (section "內容"
                (if (= :table (:block/kind block))
                  (table-view block)
                  [:div.boxed text]))
       (section "引用此區塊的段落"
                (if (seq citing)
                  [:div.kids
                   (for [n citing]
                     [:button.kid {:replicant/key n :type "button" :on {:click [[:ui/open-paragraph n]]}}
                      (title n)])]
                  [:span.muted.small "尚未被引用"]))])))

(defn- library-page [{:keys [blocks nodes ui] :as state} title]
  (let [{:keys [source-filter source-query library-selected source-file]} ui
        shown (cond->> (filter-blocks blocks source-filter source-query)
                source-file (filter #(= source-file (:block/source %))))
        confirmed (count (filter #(= :confirmed (:block/status %)) blocks))
        selected (some #(when (= library-selected (:block/id %)) %) blocks)]
    [:div.library-page
     [:aside.lib-filters
      [:div.panel-head
       [:h2 "素材庫"]
       [:span.muted.small (str "已確認 " confirmed " / 共 " (count blocks) " 區塊")]]
      [:div.field
       [:label.muted.small {:for "lib-search"} "搜尋素材"]
       [:input#lib-search {:type "search"
                           :placeholder "關鍵字、檔名或欄位"
                           :value source-query
                           :on {:input [[:ui/set-source-query [:event/target-value]]]
     :compositionend [[:ui/set-source-query [:event/target-value]]]}}]]
      (filter-chips blocks source-filter :ui/set-source-filter)
      [:div.detail-section
       [:h3 "來源檔"]
       (for [[src bs] (sort-by key (group-by :block/source blocks))]
         [:button.source-row {:replicant/key src
                              :type "button"
                              :title src
                              :aria-pressed (str (= src source-file))
                              :on {:click [[:ui/toggle-source-file src]]}}
          [:span.source-name src]
          [:span.muted.small.nowrap (str (count bs) " 區塊")]])]
      [:button.btn.btn-dashed {:type "button" :disabled true :title "M3 實作素材匯入"} "匯入來源檔"]]
     [:section.lib-list {:aria-label "素材區塊"}
      [:div.lib-count.muted.small
       (str "符合 " (count shown) " 個區塊"
            (when (> (count shown) list-limit) (str "，顯示前 " list-limit " 個；請用搜尋或篩選縮小範圍")))]
      (if (seq shown)
        (map #(library-card nodes library-selected %) (take list-limit shown))
        [:span.muted.small "沒有符合的素材區塊"])]
     (library-detail state title selected)]))

;; ---------------------------------------------------------------------------

(defn app [state]
  (if-not (:nodes state)
    [:main.app [:div.loading "載入中…"]]
    (let [nodes (:nodes state)
          numbers (tree/numbering nodes)
          title #(tree/display-title nodes numbers %)
          page (get-in state [:ui :page])]
      [:main.app
       (topbar state title)
       (case page
         :library (library-page state title)
         :fulltext [:div.workspace (fulltext state title) (editor-pane state title)]
         [:div.workspace (tree-panel state title) (editor-pane state title)])])))
