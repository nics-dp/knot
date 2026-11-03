(ns knot.views
  "畫面（SPEC 6）：撰寫頁（卡片樹與編輯區）與素材庫頁。
  畫面是狀態的純函式，事件以 Nexus action 描述。"
  (:require [clojure.string :as str]
            [knot.context :as context]
            [knot.diff :as diff]
            [knot.history :as history]
            [knot.layout :as layout]
            [knot.note :as note]
            [knot.prose :as prose]
            [knot.sources :as sources]
            [knot.tree :as tree]
            [knot.ui :as ui]))

(def column-gap 12)
(def min-column-width 195)
(def list-limit
  "素材清單一次顯示的上限，避免上千個區塊拖慢畫面。"
  60)

(def status-pill
  "卡片與編輯區名稱旁的狀態標籤：只標出尚未生成的段落。
  agent 撰寫與人工改過仍記錄在 :node/status（agent 改寫人工改過的段落時要先顯示差異），但不標示。"
  {:none {:label "尚未生成" :class "pill-none"}})

(def relation-label context/relation-label)

(def relation-options [:progression :causal :contrast :example])

(defn- gist [s]
  (str/replace (or s "") "；" "，"))

;; 字數 -----------------------------------------------------------------------

(defn format-chars
  "字數加千分位：12345 → \"12,345\"。"
  [n]
  (str/replace (str n) #"\B(?=(\d{3})+(?!\d))" ","))

(def count-low-ratio "低於目標的這個比例提示不足。" 0.8)
(def count-high-ratio "高於目標的這個比例提示超出。" 1.2)

(defn count-info
  "字數的顯示：chars 為目前字數（nil 表示這種卡片沒有字數）、target 為目標字數（nil 表示沒設定）。
  回傳 {:text 顯示文字 :class 顏色類別或 nil :title 說明或 nil}；沒有東西可顯示時為 nil。
  有本文：「312 字」或「312／400 字」；沒有本文但有目標：「目標 400 字」。
  低於目標的 80% 為 :count-low（不足）、超過 120% 為 :count-high（超出），title 說明差多少。"
  [chars target]
  (cond
    (nil? chars) nil
    (and (zero? chars) (nil? target)) nil
    (zero? chars) {:text (str "目標 " (format-chars target) " 字")
                   :title "尚無本文"}
    (nil? target) {:text (str (format-chars chars) " 字")}
    :else
    (let [ratio (/ chars target)
          pct (fn [r] (str (Math/round (double (* 100 r))) "%"))]
      (cond-> {:text (str (format-chars chars) "／" (format-chars target) " 字")}
        (< ratio count-low-ratio)
        (assoc :class "count-low" :title (str "比目標少 " (pct (/ (- target chars) target)) "（目標 " (format-chars target) " 字）"))
        (> ratio count-high-ratio)
        (assoc :class "count-high" :title (str "比目標多 " (pct (/ (- chars target) target)) "（目標 " (format-chars target) " 字）"))))))

(defn- grip []
  [:span.grip {:aria-hidden "true"}
   [:svg {:width 8 :height 14 :viewBox "0 0 8 14" :fill "currentColor"}
    (for [[x y] [[2 2] [6 2] [2 7] [6 7] [2 12] [6 12]]]
      [:circle {:replicant/key [x y] :cx x :cy y :r 1.2}])]])

(def heading-level-names
  "標題各層的稱呼，沿用政府文件的章、節、項、款。"
  ["章" "節" "項" "款"])

(defn- kind-label [nodes id]
  (case (tree/kind nodes id)
    :heading (get heading-level-names (dec (tree/depth nodes id)))
    :intro "引言"
    :paragraph "段落"
    :table "表格"
    :figure "圖"))

(defn- crumb-titles [nodes title id]
  (mapv title (reverse (tree/ancestor-ids nodes id))))

(defn- block-index [blocks]
  (into {} (map (juxt :block/id identity)) blocks))

(defn filter-blocks
  "素材清單：依類型（:all、:para、:table）、來源檔、搜尋字串篩選；預設排除已捨棄。
  搜尋用 knot.sources/search（多詞、NFKC、相關度排序）。"
  [blocks kind query & [{:keys [source include-discarded?]}]]
  (sources/search blocks query
                  {:kind (when (#{:para :table} kind) kind)
                   :source source
                   :include-discarded? include-discarded?}))

(defn- block-tag
  "類型標籤。介面用語「節錄」對應 :para，以免和段落卡片混淆。"
  [{:block/keys [kind] :as block}]
  [:span.tag {:class (when (= :table kind) "tag-navy")}
   (cond (= :entity (:table/kind block)) "實體表" (= :table kind) "表格" :else "節錄")])

(defn- block-status [block]
  (when (sources/discarded? block)
    [:span.block-status.muted "已捨棄"]))

(defn- filter-chips [current action]
  [:div.chips {:role "group" :aria-label "篩選"}
   (for [[f label] [[:all "全部"] [:para "節錄"] [:table "表格"]]]
     [:button.chip-btn {:replicant/key f
                        :type "button"
                        :aria-pressed (str (= f current))
                        :on {:click [[action f]]}}
      label])])

;; ---------------------------------------------------------------------------
;; 本文（唯讀）

(defn xref-text [label]
  (str (if (str/starts-with? label "fig:") "圖 " "表 ") label))

(defn cite-text
  "引用區塊的顯示文字：區塊標題，找不到時為「來源」。blocks 為素材區塊的序列。"
  [blocks block-id]
  (or (some #(when (= (str block-id) (str (:block/id %))) (not-empty (:block/title %))) blocks) "來源"))

(defn- render-inline [facts blocks {:keys [type text attrs]}]
  (case type
    "text" text
    "fact" (let [{:fact/keys [label value] :as f} (get facts (:key attrs))]
             (if value
               [:span.chip.chip-fact {:title (str label "（" (:key attrs) "）")} (sources/fact-display f)]
               [:span.chip.chip-warn {:title "找不到這個事實"} (:key attrs)]))
    "ref" [:span.chip.chip-ref (xref-text (:label attrs))]
    "cite" [:span.chip.chip-cite {:title (str "來源區塊：" (:block attrs))} (cite-text blocks (:block attrs))]
    nil))

(defn prose-view
  "唯讀的本文：段落、項目清單、編號清單（清單只有一層）。blocks 為素材區塊，用來顯示引用的區塊標題。"
  [facts doc & [blocks]]
  (for [[i block] (map-indexed vector (:content doc))
        :let [inline (fn [para] (map #(render-inline facts blocks %) (:content para)))]]
    (case (:type block)
      ("bulletList" "orderedList")
      [(if (= "bulletList" (:type block)) :ul.prose-list :ol.prose-list) {:replicant/key i}
       (for [[j item] (map-indexed vector (:content block))]
         [:li {:replicant/key j} (mapcat inline (:content item))])]
      [:p.prose {:replicant/key i} (inline block)])))

(defn table-card-view
  "表格卡片自己的內容（{:columns :rows}）：欄名為表頭。"
  [{:keys [columns rows]}]
  [:div.data-table-wrap
   [:table.data-table
    [:thead [:tr (for [[i c] (map-indexed vector columns)] [:th {:replicant/key i} c])]]
    [:tbody (for [[ri row] (map-indexed vector rows)]
              [:tr {:replicant/key ri}
               (for [[ci cell] (map-indexed vector row)] [:td {:replicant/key ci} cell])])]]])

(defn- table-filled? [node] (boolean (seq (:columns (:node/table-content node)))))

;; ---------------------------------------------------------------------------
;; 頂部列

(def tabs [[:library "素材"] [:tree "卡片"] [:fulltext "全文"] [:style "風格"]])

(defn- time-text [at]
  (when at
    #?(:cljs (.toLocaleString at "zh-TW" #js {:month "numeric" :day "numeric" :hour "2-digit" :minute "2-digit"})
       :clj (str at))))

(declare author-tag note-threads note-compose)

(defn- card-where
  "清單裡的卡片名稱；段落名稱常會重複，前面加上所屬標題。"
  [nodes title id]
  (list (when-let [p (when-not (tree/heading? nodes id) (tree/parent nodes id))]
          [:span.muted {:replicant/key "p"} (str (title p) " ›")])
        [:span {:replicant/key "t"} (title id)]))

(defn- menu-item
  "頂部列清單的一項：作者、卡片名稱，下面是說明；有卡片時可以點選跳過去。"
  [key on-click author where detail]
  (let [body (list [:span.menu-item-title {:replicant/key "w"} (author-tag author) where]
                   [:span.menu-item-detail {:replicant/key "d"} detail])]
    (if on-click
      [:button.menu-item {:replicant/key key :type "button" :on {:click on-click}} body]
      [:div.menu-item {:replicant/key key} body])))

(defn- menu-button [open? on-click label n title]
  [:button.btn {:type "button" :aria-expanded (str (boolean open?)) :title title :on {:click on-click}}
   label (when (pos? n) [:span.count-badge n]) " ▾"])

(defn- recent-menu
  "最近修改的卡片（不分 agent 或使用者），點選跳到卡片（SPEC 5.13）。"
  [{:keys [nodes ui]} title]
  (let [cards (filter #(contains? nodes (:node/id %)) (:recent ui))
        open? (:recent-open ui)]
    [:div.menu-wrap
     (menu-button open? [[:recent/toggle]] "最近修改" 0 "列出最近修改的卡片")
     (when open?
       (list
        [:div.menu-backdrop {:replicant/key "backdrop" :on {:click [[:ui/close-recent]]}}]
        [:div.menu-panel.recent-menu {:replicant/key "menu"}
         [:div.menu-head "最近修改的卡片"]
         (if (seq cards)
           (for [{id :node/id :keys [label at by]} cards]
             (menu-item id [[:ui/open-recent-card id]] by (card-where nodes title id)
                        (str/join " · " (remove str/blank? [label (time-text at)]))))
           [:p.small.muted "還沒有修改過的卡片"])]))]))

(defn- todos-menu
  "頂部列的「待辦」：整份報告未完成的待辦、agent 的提問與整份報告的留言（SPEC 5.11）。"
  [{:keys [nodes notes ui] :as state} title]
  (let [todos (note/open-todos nodes notes)
        questions (note/agent-questions nodes notes)
        open? (:todos-open ui)
        item (fn [{:note/keys [id node text author at] :as n} question?]
               (menu-item id (when node [(if question?
                                           [:ui/open-note-thread node id]
                                           [:ui/open-note-card node])])
                          author
                          (if node (card-where nodes title node) "整份報告")
                          (list (when (:note/modified-after n) [:span.note-modified {:replicant/key "mod"} "之後已修改"])
                                [:span.menu-item-text {:replicant/key "x"} (gist text)]
                                (when at [:span {:replicant/key "at"} (str " · " (time-text at))]))))]
    [:div.menu-wrap
     (menu-button open? [[:ui/toggle-todos]] "待辦事項" (+ (count todos) (count questions))
                  "交代 agent 的待辦事項、agent 的提問與整份報告的留言")
     (when open?
       (list
        [:div.menu-backdrop {:replicant/key "backdrop" :on {:click [[:ui/close-todos]]}}]
        [:div.menu-panel.todos-panel {:replicant/key "panel"}
         [:div.menu-head (str "等你回覆（" (count questions) "）")]
         (if (seq questions) (map #(item % true) questions) [:p.small.muted "沒有待回覆的提問"])
         [:div.menu-head (str "交給 agent（" (count todos) "）")]
         (if (seq todos) (map #(item % false) todos) [:p.small.muted "沒有未完成的待辦事項"])
         [:div.menu-head "整份報告的留言"]
         [:div.threads (note-threads state nil)]
         (note-compose state :report nil "給整份報告的交代或備忘…")]))]))

(defn- report-edit-panel
  "報告名稱與副標的編輯面板；Enter 儲存、Esc 取消。"
  [{:keys [ui] :as state}]
  (let [{:keys [title subtitle]} (:report-edit ui)
        input (fn [field label value placeholder focus?]
                [:label.field {:replicant/key (name field)}
                 [:span.small label]
                 [:input (cond-> {:type "text" :value value :placeholder placeholder
                                  :on {:input [[:ui/set-report-draft field [:event/target-value]]]
                                       :compositionend [[:ui/set-report-draft field [:event/target-value]]]
                                       :keydown [[:report/keydown [:event/key]]]}}
                           focus? (assoc :replicant/on-mount [[:dom/focus]]))]])]
    (list
     [:div.menu-backdrop {:replicant/key "backdrop" :on {:click [[:ui/close-report-edit]]}}]
     [:div.menu-panel.report-edit-panel {:replicant/key "panel"}
      (input :title "報告名稱" title "" true)
      (input :subtitle "副標（可留空）" subtitle "例如：成果報告" false)
      [:div.note-compose-actions
       [:button.btn {:type "button" :on {:click [[:ui/close-report-edit]]}} "取消"]
       [:button.btn.btn-primary {:type "button" :disabled (not (ui/report-draft-valid? state))
                                 :on {:click [[:report/save]]}}
        "儲存"]]])))

(defn- topbar [{:keys [report ui] :as state} title]
  (let [{:keys [page]} ui
        full (str/join " · " (remove str/blank? [(:report/title report) (:report/subtitle report)]))]
    [:header.topbar
     [:div.menu-wrap.topbar-title-wrap
      [:button.topbar-title {:type "button" :title "編輯報告名稱與副標" :aria-label (str "編輯報告名稱與副標：" full)
                             :aria-expanded (str (boolean (:report-edit ui)))
                             :on {:click [[:ui/open-report-edit]]}}
       [:span.report-name (:report/title report)]
       (when-not (str/blank? (:report/subtitle report))
         [:span.muted " · " (:report/subtitle report)])]
      (when (:report-edit ui) (report-edit-panel state))]
     [:nav.segmented.tabs {:aria-label "頁籤"}
      (for [[p label] tabs]
        [:button {:replicant/key p
                  :type "button"
                  :aria-pressed (str (= p page))
                  :on {:click [[:ui/set-page p]]}}
         label])]
     [:div.topbar-actions
      (recent-menu state title)
      (todos-menu state title)
      [:button.btn.btn-primary {:type "button" :disabled true :title "M6 實作 LaTeX 匯出"} "匯出 LaTeX"]]]))

(defn- conflict-view
  "版本衝突後的說明：被其他來源改過的卡片，以及沒有存入、可以複製的文字（SPEC 5.13）。"
  [conflict]
  (when-let [{:keys [names lost]} conflict]
    [:div.conflict {:role "alert"}
     [:div.conflict-head
      [:span (str names "剛被其他來源（agent 或其他分頁）修改，已重新載入；剛才的修改沒有存入"
                  (when (seq lost) "。下面是沒有存入的文字，可以複製後重新輸入："))]
      [:button.toast-close {:type "button" :aria-label "關閉" :on {:click [[:ui/dismiss-conflict]]}} "×"]]
     (for [[i {:keys [title field text]}] (map-indexed vector lost)]
       [:div.conflict-item {:replicant/key i}
        [:div.conflict-item-head
         [:span.small (str title " · " field)]
         [:button.text-btn {:type "button" :on {:click [[:clipboard/copy text]]}} "複製"]]
        [:textarea.conflict-text {:readonly true :value text :rows (-> text str/split-lines count (max 2) (min 6))}]])]))

(defn- toast-view [toast]
  (when toast
    [:div.toast {:role "status"}
     [:span toast]
     [:button.toast-close {:type "button" :aria-label "關閉訊息" :on {:click [[:ui/dismiss-toast]]}} "×"]]))

;; ---------------------------------------------------------------------------
;; 卡片樹

(defn- card-role [selected anc desc related nodes id]
  (cond
    (= id selected) :active
    (or (anc id) (desc id)) :path
    (related id) :related
    (= (tree/parent nodes id) (tree/parent nodes selected)) :sibling
    :else :other))

(def role-opacity {:active 1 :path 1 :related 1 :sibling 0.7 :other 0.35})

(defn- note-badges
  "卡片與大綱標題旁的數字：等你回覆的 agent 提問（醒目）與交給 agent 的待辦事項（淡色），
  與頂部列「待辦事項」的兩組對應。subtree? 為真時是含下層的合計。
  open-id 有值時數字是按鈕：點了選取那張卡片並捲到編輯區的「留言與歷程」；
  提問的數字帶 question-id 時直接打開那則提問的回覆框。"
  ([todos questions subtree?] (note-badges todos questions subtree? nil nil))
  ([todos questions subtree? open-id question-id]
   (let [todos (or todos 0) questions (or questions 0)
         scope (when subtree? "（含下層）")
         attrs (fn [title note-id]
                 (if open-id
                   {:type "button" :title (str title (if note-id "；點選回覆" "；點選開啟留言"))
                    :on {:click [(cond-> [:ui/open-note-thread open-id] note-id (conj note-id))]}}
                   {:title title}))
         tag (fn [k] (if open-id (keyword (str "button." k)) (keyword (str "span." k))))]
     (when (pos? (+ todos questions))
       [:span.note-badges
        (when (pos? questions)
          [(tag "question-count") (attrs (str questions " 則 agent 的提問等你回覆" scope) question-id) questions])
        (when (pos? todos)
          [(tag "todo-count") (attrs (str todos " 則待辦事項交給 agent" scope) nil) todos])]))))

(defn- count-span
  "字數小字：class 為基本類別，超出或不足時加上顏色類別並以 title 說明。"
  [base {:keys [text class title]}]
  [:span {:class (cond-> [base] class (conj class)) :title title} text])

(defn- tree-card [{:keys [nodes title role drag over agent todos questions counts]} {:keys [id top height]}]
  (let [node (get nodes id)
        k (:node/kind node)
        dragging? (= id drag)
        into? (and (= :into (:pos over)) (= id (:id over)))]
    [:div.card {:replicant/key id
                :draggable "true"
                :class (cond-> [(str "card-" (name role)) (str "card-kind-" (name k))]
                         dragging? (conj "card-dragging")
                         into? (conj "card-into")
                         (contains? (:ids agent) id) (conj (:class agent)))
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
                      ;; 點卡片只選取；編輯區由使用者自行展開
                      :on {:click [[:ui/select id]]}}
      (case k
        (:table :figure)
        (list
         [:span.card-line {:replicant/key "t"}
          [:span.mono.text-navy (if (= k :table) "表" "圖")]
          [:span.card-title (title id)]
          (when (and (= k :table) (not (table-filled? node)))
            [:span.pill.pill-none "尚未填寫"])
          (note-badges (get todos id) (get questions id) false)]
         [:span.mono.muted.small {:replicant/key "l"} (:node/label node)])

        (list
         [:span.card-line {:replicant/key "t"}
          [:span.card-title (title id)]
          (when-let [info (count-info (get counts id) (:node/target-chars node))]
            (count-span "card-count" info))
          (when-let [{:keys [label class]} (status-pill (:node/status node))]
            [:span.pill {:class class} label])
          (note-badges (get todos id) (get questions id) false)]
         [:span.card-gist {:replicant/key "g"}
          (if (and (= k :intro) (str/blank? (:node/draft node)))
            "（由 agent 依下層內容撰寫）"
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

(defn- selected-path
  "選取卡片的位置：上層卡片接著卡片名稱；太長時省略上層、保留卡片名稱。"
  [nodes title id]
  (let [ancestors (crumb-titles nodes title id)]
    [:div.selected-path {:title (str/join " › " (conj ancestors (title id)))}
     (when (seq ancestors)
       [:span.path-ancestors (str (str/join " › " ancestors) " ›")])
     [:span.path-current (title id)]]))

(defn- tree-toolbar
  "在選取卡片的層級上新增或刪除。"
  [{:keys [nodes ui]}]
  (let [id (:selected ui)
        k (ui/sibling-kind nodes id)]
    [:div.toolbar {:role "toolbar" :aria-label "卡片操作"}
     [:button.btn.btn-primary {:type "button"
                             :disabled (nil? k)
                             :title (if k
                                      (str "在「" (tree/display-title nodes (tree/numbering nodes) id) "」後面新增")
                                      "這張卡片不能新增同層卡片")
                             :on {:click [[:ui/add-sibling id]]}}
      "新增"]
     [:button.btn.btn-danger {:type "button"
                                        :title "刪除選取的卡片與其下層卡片；可以復原"
                                        :on {:click [[:ui/delete-node id]]}}
      "刪除"]]))

(defn- column-name
  "欄位名稱依內容決定：標題用層級稱呼（章、節、項、款），另有段落、表與圖時一併列出。"
  [nodes depth col]
  (let [kinds (set (map #(tree/kind nodes (:id %)) col))]
    (str/join "／" (cond-> []
                     (kinds :heading) (conj (get heading-level-names (dec depth)))
                     (kinds :paragraph) (conj "段落")
                     (some kinds [:table :figure]) (conj "表與圖")
                     (= kinds #{:intro}) (conj "引言")))))

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
        related (let [{:keys [outgoing incoming]} (tree/remote-relations (:relations state) selected)]
                  (set (concat (map :relation/to outgoing) (map :relation/from incoming))))
        roles (memoize #(card-role selected anc desc related nodes %))
        sel-depth (tree/depth nodes selected)
        todos (note/todo-counts nodes (:notes state))
        questions (note/question-counts nodes (:notes state))
        ;; agent 剛修改的卡片：兩個相同的動畫交替使用，同一張卡片再被修改時才會重新播放
        agent (when-let [{:keys [ids n]} (:agent-change ui)]
                {:ids (set ids) :class (if (even? n) "card-agent-a" "card-agent-b")})
        drag-depth (when drag (tree/depth nodes drag))
        counts (tree/char-counts state)]
    [:section.tree-panel {:aria-label "卡片"}
     [:header.board-head
      [:h2.area-title "卡片"]
      (selected-path nodes title selected)
      (tree-toolbar state)]
     [:div.board
      (conflict-view (:conflict ui))
      (when-not drag (toast-view toast))
      [:div.tree-scroll {:class (cond-> []
                                  drag (conj "is-dragging")
                                  (not (:editor-collapsed ui)) (conj "drawer-open"))
                        :replicant/on-mount [[:dom/observe-viewport]]
                        :replicant/on-render [[:dom/reveal-columns sel-depth selected (count cols)
                                               (boolean (:editor-collapsed ui))]]}
      ;; 欄寬依看板全寬均分（不因編輯區而縮小）、最窄 min-column-width；放不下時水平捲動
      (let [n (count cols)
            w (str "max(" min-column-width "px, calc((100cqw - " (* (dec n) column-gap) "px) / " n "))")]
        [:div.tree-columns {:style {:column-gap (str column-gap "px")
                                    :grid-template-columns (str "repeat(" n ", " w ")")}}
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
           [:span.col-name (column-name nodes (inc i) col)]
           ;; 拖曳時，左邊一欄以「移入」取代卡片數
           (if (and drag (= (inc i) (dec drag-depth)))
             [:span.col-drop-tag "移入"]
             [:span.col-count (str (count col) " 張")])]
          [:div.col-viewport {:on {:wheel [[:ui/wheel-column i [:event/wheel-delta] lo hi]]
                                   :dragover [[:drag/over i [:event/viewport-y] translate]
                                              [:ui/edge-scroll i [:event/edge-delta] lo hi]]
                                   :drop [[:drag/drop]]}}
           [:div.tree-inner {:style {:transform (str "translateY(" translate "px)")}}
            (relation-chips state col roles)
            (for [c col]
              (tree-card {:nodes nodes :title title :role (roles (:id c)) :drag drag :over over :agent agent :todos todos :questions questions :counts counts} c))
            (when (and over (some #(= (:id over) (:id %)) col))
              (insert-marker col over))]]])])]]]))

;; ---------------------------------------------------------------------------
;; 全文視圖

(defn- outline
  "全文頁左側的大綱：所有標題依層級縮排；選取的卡片所在的章節標為目前位置。
  標題旁是這一章節（含下層）等你回覆的提問數與未完成的待辦事項數。"
  [{:keys [nodes notes ui]} title]
  (let [selected (:selected ui)
        todos (note/todo-counts nodes notes)
        questions (note/question-counts nodes notes)
        here (set (cons selected (tree/ancestor-ids nodes selected)))]
    [:nav.outline {:aria-label "大綱"}
     [:div.outline-head "大綱"]
     [:ol.outline-list
      (for [id (tree/document-order nodes)
            :when (tree/heading? nodes id)
            :let [d (tree/depth nodes id)]]
        [:li {:replicant/key id}
         [:button.outline-item {:type "button"
                                :title (title id)
                                :class [(str "outline-d" d)
                                        (when (here id) "outline-here")
                                        (when (= id selected) "outline-selected")]
                                :aria-current (when (= id selected) "true")
                                :on {:click [[:ui/select id]]}}
          [:span.outline-text (title id)]
          (note-badges (note/subtree-todo-count nodes todos id)
                       (note/subtree-todo-count nodes questions id) true)]])]]))

(defn- ft-plan
  "全文頁中還沒有本文的部分：第一行說明是什麼，下面是意圖（UI 字體，與本文區分）。"
  [label intent]
  [:div.ft-plan
   [:div.ft-plan-label label]
   (when-not (str/blank? intent)
     [:p.ft-plan-intent intent])])

(defn- fulltext
  "全文：依序呈現整份報告的標題與本文。點標題或段落選取，右側編輯區隨之切換；
  從大綱等處選取時捲到該處。沒有本文的段落顯示名稱與意圖，讓使用者在這裡審閱大綱。"
  [{:keys [nodes facts ui] :as state} title]
  (let [selected (:selected ui)
        intents? (ui/fulltext-intents? state)
        todos (note/todo-counts (:nodes state) (:notes state))
        questions (note/question-counts (:nodes state) (:notes state))
        first-q (note/first-questions (:nodes state) (:notes state))
        noted? #(pos? (+ (get todos % 0) (get questions % 0)))
        badges (fn [id] (when (noted? id)
                          [:div.ft-badges {:replicant/key (str id "-badges")}
                           (note-badges (get todos id) (get questions id) false id (first-q id))]))
        select-on #(hash-map :replicant/key % :data-id (str %)
                             :class (cond-> [] (= % selected) (conj "ft-selected") (noted? %) (conj "ft-noted"))
                             :on {:click [[:ui/select %]]})]
    [:section.fulltext {:aria-label "全文"
                        :replicant/on-render [[:dom/reveal-selected (str selected)]]}
     (conflict-view (:conflict ui))
     (toast-view (:toast ui))
     [:div.fulltext-page
      [:label.ft-toggle
       [:input {:type "checkbox" :checked intents?
                :on {:change [[:ui/toggle-fulltext-intents]]}}]
       "顯示意圖"]
      (for [id (tree/document-order nodes)
            :let [node (get nodes id)
                  intent (:node/draft node)]]
        (case (:node/kind node)
          :heading
          (let [d (tree/depth nodes id)]
            (list
             [(keyword (str "h" (inc d) ".ft-heading")) (select-on id) (title id)]
             (when (and intents? (not (str/blank? intent)))
               [:div.ft-plan-wrap {:replicant/key (str id "-plan")}
                (ft-plan (str "本" (get heading-level-names (dec d)) "意圖") intent)])))

          :intro
          [:div.ft-block.ft-intro (select-on id)
           (badges id)
           ;; 只在畫面上標示，匯出時引言就是一般的文字
           [:div.ft-kind "引言"]
           (if-let [doc (:node/prose node)]
             (prose-view facts doc (:blocks state))
             (if (and intents? (not (str/blank? intent)))
               (ft-plan "引言 · 尚未撰寫" intent)
               [:p.prose.muted "（引言尚未生成，將由 agent 依下層內容補上）"]))]

          :paragraph
          [:div.ft-block (assoc (select-on id) :role "button" :tabindex 0)
           (badges id)
           (if-let [doc (:node/prose node)]
             (prose-view facts doc (:blocks state))
             (if intents?
               (ft-plan (str (:node/title node) " · 尚未撰寫")
                        (if (str/blank? intent) "（尚未撰寫，也還沒有意圖）" intent))
               [:p.prose.muted (str "（尚未撰寫：" (:node/title node) "）")]))]

          :table
          [:div.ft-float.ft-table (assoc (select-on id) :role "button" :tabindex 0)
           (badges id)
           [:div.ft-caption
            [:span.chip.chip-ref (xref-text (:node/label node))]
            [:span (:node/title node)]]
           (if (table-filled? node)
             (table-card-view (:node/table-content node))
             [:div.ft-float-empty "（尚未填寫）"])]

          :figure
          [:div.ft-float.ft-figure (assoc (select-on id) :role "button" :tabindex 0)
           (badges id)
           [:div.figure-placeholder "圖檔在匯出 LaTeX 時處理"]
           [:div.ft-caption
            [:span.chip.chip-ref (xref-text (:node/label node))]
            [:span (:node/title node)]]]))]]))

;; ---------------------------------------------------------------------------
;; 編輯區
;;
;; 由上而下照生成的流程排成固定的區塊（SPEC 5.4）：
;; 生成依據（使用者在這張卡片上設定的）→ 撰寫時參考的內容（自動組成，只能看）→ 本文（產出）。
;; 標題與表、圖不生成本文，只有「設定」與「結構」。

(defn- zone
  "編輯區的一個區塊。opts：:hint 標題下的一行說明、:aside 標題右側（標記或操作）、:class 區塊樣式。"
  [k title {:keys [hint aside class]} & body]
  [:section.zone {:replicant/key k :class class}
   [:div.zone-head
    [:div.zone-title [:h3 title] (when hint [:p.zone-hint hint])]
    (when aside [:div.zone-aside aside])]
   body])

(defn- field
  "區塊內的一個欄位。opts：:count 標題旁的數量、:action 標題右側的按鈕。"
  [k title {n :count action :action editing? :editing?} & body]
  [:div.field {:replicant/key k :class (when editing? "field-editing")}
   [:div.field-head
    [:h4 title (when n [:span.field-count n])
     (when editing? [:span.editing-badge "編輯中"])]
    action]
   body])

(defn- item-row
  "依據、關係、下層表與圖共用的項目列：開頭是類型標籤，接著名稱，右側是補充資訊與操作。"
  [k {:keys [tag tag-class lead position title on-title meta control warn?]}]
  [:div.item {:replicant/key k :class (when warn? "item-warn")}
   (or lead [:span.tag.item-tag {:class tag-class} tag])
   (when position [:span.item-position position])
   (if on-title
     [:button.item-title.item-link {:type "button" :on {:click on-title}} title]
     [:span.item-title title])
   (when meta [:span.item-meta meta])
   control])

(defn- remove-button [label actions]
  [:button.cite-remove {:type "button" :aria-label label :on {:click actions}} "×"])

(defn- field-button [label actions]
  [:button.text-btn {:type "button" :on {:click actions}} label])

(defn- name-input [id field value placeholder focus?]
  [:input.title-input {:replicant/key (str id field)
                       :replicant/on-mount (when focus? [[:dom/select-all] [:ui/clear-focus-title]])
                       :type "text"
                       :aria-label "名稱"
                       :placeholder placeholder
                       :value (or value "")
                       :on {:input [[:ui/set-node-field id field [:event/target-value]]]
                            :compositionend [[:ui/set-node-field id field [:event/target-value]]]}}])

(defn- edit-box
  "意圖與本文的編輯框：平常是閱讀樣式，滑過提示「點擊編輯」，點進去後顯示「編輯中」。"
  [k child]
  [:div.edit-box {:replicant/key k
                  :data-hint "點擊編輯"
                  :data-editing "編輯中 · Esc 結束"}
   child])

(defn- intent-field [id value placeholder]
  (field "intent" "意圖" nil
         (edit-box "intent-box"
          [:textarea.intent {:replicant/key (str id "-intent")
                            :rows 3
                            :aria-label "意圖"
                            :placeholder placeholder
                            :value (or value "")
                            :on {:input [[:ui/set-node-field id :node/draft [:event/target-value]]]
                                 :compositionend [[:ui/set-node-field id :node/draft [:event/target-value]]]}}])))

(defn- target-field
  "目標字數：數字輸入，可清空；note 是輸入框旁的補充（例如標題的整章合計）。"
  [id value note]
  (field "target" "目標字數" nil
         [:div.target-row
          [:input.target-input {:replicant/key (str id "-target")
                                :type "number" :min 1 :step 1 :inputmode "numeric"
                                :aria-label "目標字數"
                                :placeholder "例如 400"
                                :value (if value (str value) "")
                                :on {:input [[:ui/set-target-chars id [:event/target-value]]]}}]
          note]))

(defn- relation-select
  "相鄰關係的類型，放在項目列開頭、取代類型標籤。"
  [from to rel]
  [:select.rel-select.item-tag {:aria-label "關係類型"
                                :class (when (:relation/pending rel) "rel-pending")
                                :on {:change [[:ui/set-relation from to [:event/target-keyword]]]}}
   [:option {:value "" :selected (nil? (:relation/type rel))} "未標註"]
   (for [t relation-options]
     [:option {:replicant/key t :value (name t) :selected (= t (:relation/type rel))}
      (relation-label t)])])

(defn- relation-confirm
  "移動後標為待確認的相鄰關係：有類型時可直接確認。"
  [from to rel]
  (when (:relation/pending rel)
    (if (:relation/type rel)
      [:button.btn.btn-small.btn-amber {:type "button"
                                        :on {:click [[:ui/set-relation from to (:relation/type rel)]]}}
       "確認"]
      [:span.relation-chip.relation-pending "待確認"])))

(defn- relation-rows
  "只列在這張卡片上設定的關係：與前後段的相鄰關係、本段新增的不相鄰關係。
  其他段落指向本段的關係由對方設定，列在「撰寫時參考的內容」。"
  [{:keys [nodes relations]} title id editing?]
  ;; 每一列是「關係類型、位置、卡片名稱」；唯讀時類型是標籤，編輯時是下拉選單
  (let [prev (tree/prev-paragraph nodes id)
        nxt (let [n (tree/next-sibling nodes id)] (when (tree/paragraph? nodes n) n))
        numbers (tree/numbering nodes)
        adjacent (fn [k position from to other]
                   (let [rel (tree/relation-between relations from to)]
                     (item-row k {:lead (if editing?
                                          (relation-select from to rel)
                                          [:span.tag.item-tag {:class (when (:relation/pending rel) "rel-pending")}
                                           (or (some-> (:relation/type rel) relation-label) "未標註")])
                                  :position position
                                  :title (title other) :on-title [[:ui/select other]]
                                  :control (if editing?
                                             (relation-confirm from to rel)
                                             (when (:relation/pending rel)
                                               [:span.relation-chip.relation-pending "待確認"]))})))]
    (concat
     (when prev [(adjacent "prev" "前一段" prev id prev)])
     (when nxt [(adjacent "next" "後一段" id nxt nxt)])
     (for [{:relation/keys [to type]} (:outgoing (tree/remote-relations relations id))]
       (item-row (str "out-" (name type) "-" to)
                 {:tag (relation-label type) :tag-class "tag-teal"
                  :lead (when editing?
                          [:select.rel-select.item-tag.tag-teal
                           {:aria-label "關係類型"
                            :on {:change [[:ui/set-remote-relation-type id to type [:event/target-keyword]]]}}
                           (for [t tree/remote-relation-types]
                             [:option {:replicant/key t :value (name t) :selected (= t type)}
                              (relation-label t)])])
                  :position (tree/position-label nodes numbers to)
                  :title (title to) :on-title [[:ui/select to]]
                  :control (when editing?
                             (remove-button "移除關係" [[:ui/remove-remote-relation id to type]]))})))))

(def remote-relation-hint
  {:summarizes "本段彙整目標段落的結果"
   :basis "本段的論點建立在目標段落的結果上；目標改變時，本段會收到待辦事項"
   :echo "前後呼應，不影響論點"
   :contrast "與目標段落比較"})

(defn- rel-picker
  "新增不相鄰的關係：選類型，再以名稱或意圖搜尋目標段落。"
  [{:keys [nodes relations ui]} title id]
  (let [{:keys [type query]} (:rel-picker ui)
        q (str/trim (or query ""))
        existing (set (for [r (:outgoing (tree/remote-relations relations id))
                            :when (= type (:relation/type r))]
                        (:relation/to r)))
        candidates (->> (tree/document-order nodes)
                        (filter #(and (tree/paragraph? nodes %) (not= id %)))
                        (filter #(or (str/blank? q)
                                     (str/includes? (title %) q)
                                     (str/includes? (or (get-in nodes [% :node/draft]) "") q))))]
    [:div.picker {:role "dialog" :aria-label "新增關係"}
     [:div.picker-head
      [:select.rel-select {:aria-label "關係類型"
                           :on {:change [[:ui/set-rel-picker-type [:event/target-keyword]]]}}
       (for [t tree/remote-relation-types]
         [:option {:replicant/key t :value (name t) :selected (= t type)} (relation-label t)])]
      [:input.picker-search {:type "search"
                             :placeholder "搜尋段落名稱或意圖"
                             :value query
                             :replicant/on-mount [[:dom/focus]]
                             :on {:input [[:ui/set-rel-picker-query [:event/target-value]]]
                                  :compositionend [[:ui/set-rel-picker-query [:event/target-value]]]}}]
      [:button.btn.btn-small {:type "button" :on {:click [[:ui/close-rel-picker]]}} "完成"]]
     [:span.muted.small (remote-relation-hint type)]
     [:div.picker-list
      (if (seq candidates)
        (for [c (take list-limit candidates)
              :let [path (str/join " / " (map title (rest (reverse (tree/ancestor-ids nodes c)))))]]
          [:div.picker-row {:replicant/key c}
           [:span.picker-title (title c)]
           [:span.muted.small.picker-source path]
           (if (existing c)
             [:span.small.text-teal "已連結"]
             [:button.btn.btn-small {:type "button" :on {:click [[:ui/add-remote-relation id c type]]}} "連結"])])
        [:span.muted.small "沒有符合的段落"])]]))

(defn- picker [{:keys [blocks ui]} node]
  (let [{:keys [query filter]} (:picker ui)
        linked (set (map :block (:node/citations node)))
        shown (filter-blocks blocks filter query)
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
     (filter-chips filter :ui/set-picker-filter)
     [:div.picker-list
      (if (seq shown)
        (for [{:block/keys [id title source location] :as b} (take list-limit shown)]
          [:div.picker-row {:replicant/key id}
           (block-tag b)
           [:span.picker-title title]
           [:span.muted.small.picker-source (str source (when location (str " · " location)))]
           (if (linked id)
             [:span.small.text-teal "已加入"]
             [:button.btn.btn-small {:type "button" :on {:click [[:ui/add-citation (:node/id node) id]]}} "加入"])])
        [:span.muted.small "沒有符合的素材"])]
     (when (pos? more)
       [:span.muted.small (str "還有 " more " 個符合的素材")])]))

(defn- citations [{:keys [blocks ui] :as state} node]
  (let [by-id (block-index blocks)
        cites (:node/citations node)
        id (:node/id node)]
    (field "cites" "依據"
           {:count (when (seq cites) (count cites))
            :action (when-not (:picker ui) (field-button "加入" [[:ui/open-picker]]))}
     (if (seq cites)
       [:div.items {:replicant/key "cites"}
        (for [{:keys [block where]} cites
              :let [b (by-id block)
                    warn? (sources/discarded? b)]]
          (item-row block {:tag (if (= :table (:block/kind b)) "表格" "節錄")
                           :tag-class (when (= :table (:block/kind b)) "tag-navy")
                           :title (:block/title b)
                           :on-title [[:ui/open-block block]]
                           :meta (if warn? [:span.text-amber "已捨棄"] (or where (:block/source b)))
                           :warn? warn?
                           :control (remove-button "移除依據" [[:ui/remove-citation id block]])}))]
       [:span.notice-amber {:replicant/key "empty"} "尚未加入依據：agent 只能依意圖撰寫，無法引用素材。"])
     (when (:picker ui) (picker state node)))))

;; 撰寫時參考的內容 --------------------------------------------------------------

(defn- source-tag [source]
  (case source
    :prose [:span.source-tag.source-prose "本文"]
    :intent [:span.source-tag "意圖"]
    nil))

(defn- has-prose-tag [has-prose]
  (when has-prose [:span.source-tag.source-prose "已有本文"]))

(defn- section-gist
  "摘要列的一項：只有一筆時列出名稱，多筆時列出筆數。"
  [{:keys [title entries]}]
  (let [[e & more] entries]
    (cond
      ;; 本段的名稱已在標題列，只標出帶的是意圖
      (= title "本段") [:span "本段意圖" (source-tag (:source e))]
      more [:span (str title " " (count entries) " 項")]
      :else [:span title "：" (:label e) (source-tag (:source e)) (has-prose-tag (:has-prose e))])))

(defn- context-zone [state id]
  (when-let [{:keys [sections chars]} (context/generation-context state id)]
    ;; 風格整份共用、不常改，編輯區不列（在風格頁檢視）；agent 的撰寫參考仍帶提示
    (let [sections (remove #(= :style (:group %)) sections)
          by-group (group-by :group sections)]
      (zone "context" "撰寫時參考的內容"
            {:class "zone-auto"
             :hint "由上方的設定與周邊卡片自動組成；周邊卡片只帶意圖，已有本文的標示「已有本文」，agent 需要時再取"
             :aside [:span.auto-size (str "約 " chars " 字")]}
            [:div.context-summary
             (for [[g label] context/group-label
                   :let [ss (get by-group g)]
                   :when (seq ss)]
               [:div.context-row {:replicant/key g}
                [:span.context-group label]
                [:span.context-items (interpose [:span.sep "·"] (map section-gist ss))]])]
            [:details.context
             [:summary "查看完整內容"]
             (for [{:keys [title entries]} sections]
               [:div.context-section {:replicant/key title}
                [:h4 title]
                (for [[i {:keys [label text relation source warn has-prose discarded]}] (map-indexed vector entries)]
                  [:div.context-entry {:replicant/key i :class (when warn "cite-warn")}
                   [:span.context-label label (source-tag source) (has-prose-tag has-prose)
                    (when discarded [:span.text-amber.small "已捨棄"])
                    (when relation [:span.relation-chip relation])]
                   [:span.context-text (if (str/blank? text) "（未填意圖）" (gist text))]])])]))))

;; 本文 ----------------------------------------------------------------------

(defn- diff-view [ops]
  [:p.prose.diff
   (for [[i [op s]] (map-indexed vector ops)]
     (case op
       :same [:span {:replicant/key i} s]
       :del [:del {:replicant/key i} s]
       :ins [:ins {:replicant/key i} s]))])

(defn- diff-entry [before after]
  (let [ops (diff/diff before after)]
    (if (diff/changed? ops)
      [:div.prose-box (diff-view ops)]
      [:span.muted.small "與前一版相同"])))

(defn- previous-view
  "與前一版比較：只保留最近一次修改之前的內容（連續的逐字輸入算同一次）。"
  [{:keys [facts ui]} node]
  (let [{:keys [id data]} (:previous ui)
        text #(some->> % (context/prose-text facts))]
    (cond
      (not= id (:node/id node)) [:span.muted.small "讀取前一版…"]
      (empty? data) [:span.muted.small "本文還沒有修改過。"]
      :else
      [:div.diff-list
       (let [{:keys [at label]} (:node/prose data)]
         [:div.diff-meta
          [:span (str "前一版：" (str/join " · " (remove str/blank? [label (time-text at)])) " 之前")]
          [:span.diff-legend.diff [:del "刪除"] [:ins "新增"]]])
       (diff-entry (text (get-in data [:node/prose :value])) (text (:node/prose node)))])))

(defn- change-view
  "看某次修改的差異：那次修改前後的本文（不是和目前比較）。"
  [{:keys [facts ui]} node]
  (let [{:keys [id data]} (:change ui)
        text #(some->> % (context/prose-text facts))]
    (if (or (not= id (:node/id node)) (nil? data))
      [:span.muted.small "讀取修改…"]
      (let [{:keys [at label by before after prose?]} data]
        [:div.diff-list
         [:div.diff-meta
          [:span (str (str/join " · " (remove str/blank? [label (time-text at)])) " 的修改"
                      (when (= :agent by) "（agent 修改）"))]
          (when prose? [:span.diff-legend.diff [:del "刪除"] [:ins "新增"]])
          [:button.text-btn {:type "button" :on {:click [[:ui/show-current]]}} "回到目前內容"]]
         (if prose?
           (diff-entry (text before) (text after))
           [:p.muted.small "這次修改沒有改動本文；改了什麼見下方「留言與歷程」。"])]))))

(def ^:private unbound-limit 12)

(defn- unbound-note
  "本文中不在標記內的數字（SPEC 5.4「驗證」）：由本文即時算出，沒有時不顯示。"
  [facts node]
  (when-let [nums (seq (prose/unbound-numbers facts (:node/prose node)))]
    [:div.unbound-note {:replicant/key "unbound"}
     [:span.unbound-label "未綁定的數字："]
     (for [n (take unbound-limit nums)]
       [:span.chip.chip-warn {:replicant/key n} n])
     (when (> (count nums) unbound-limit)
       [:span.unbound-more (str "等 " (count nums) " 個")])
     [:span.unbound-hint "（素材與事實接上後可改為引用事實）"]]))

(defn- prose-zone
  "本文區。empty-note：尚未有本文時的說明；actions：本文下方的說明或操作（可省略）。"
  [{:keys [facts ui] :as state} node empty-note & actions]
  (let [change? (some? (:change ui))
        diff? (and (boolean (:show-previous ui)) (not change?))]
    (zone "prose" "本文"
          {:class "zone-output"
           :aside (list
                   (when-let [info (count-info (prose/char-count facts (:node/prose node)) (:node/target-chars node))]
                     (count-span "zone-count" info))
                   [:div.segmented.segmented-small {:replicant/key "seg" :role "group" :aria-label "本文顯示方式"}
                   [:button {:type "button" :aria-pressed (str (not (or diff? change?)))
                             :on {:click (when (or diff? change?) [[:ui/show-current]])}} "目前內容"]
                   [:button {:type "button" :aria-pressed (str diff?)
                             :on {:click (when-not diff? [[:ui/toggle-previous]])}} "與前一版比較"]])}
          (cond
            change? (change-view state node)
            diff? (previous-view state node)
            :else
            (let [id (:node/id node)]
              (list
               (when (str/blank? (context/prose-text facts (:node/prose node)))
                 [:p.muted.small {:replicant/key "empty"} empty-note])
               ;; 編輯器的內容由 Tiptap 管理，Replicant 只負責建立與同步
               (edit-box "prose-box"
                         [:div.prose-box.prose-editor {:replicant/key (str id "-prose")
                                                       :replicant/on-render [[:prose/render id true]]
                                                       :replicant/on-unmount [[:prose/destroy]]}])
               (unbound-note facts node))))
          (when (seq actions) [:div.prose-actions actions]))))

;; 留言與待辦 -----------------------------------------------------------------

(defn- author-tag [author]
  (case author
    :agent [:span.by-tag.by-agent "agent"]
    :knot [:span.by-tag.by-knot "Knot"]
    [:span.by-tag.by-user "使用者"]))

(defn- note-line
  "一則留言：作者、待辦的勾選框、時間與操作，下面是內容。"
  [{:keys [ui nodes]} {:note/keys [id author text todo at changeset node triggers] :as n} reply?]
  (let [done? (= :done (:note/state n))]
    [:div.note {:replicant/key id
                :class [(when reply? "note-reply") (when todo "note-todo") (when done? "note-done")]}
     [:div.note-head
      (when todo
        [:input.note-check {:type "checkbox" :checked done?
                            :aria-label (if done? "重新打開待辦事項" "標為完成")
                            :on {:change [[:notes/set-state id (if done? :open :done)]]}}])
      (author-tag author)
      (when todo [:span.note-kind (if done? "已完成" "待辦事項")])
      (when (and (:note/modified-after n) (not done?))
        [:span.note-modified {:title "提醒之後，這一段自己的意圖或本文又被修改過"} "之後已修改"])
      [:span.small.muted (time-text at)]
      [:span.note-actions
       (when (and done? changeset node)
         [:button.text-btn {:type "button" :on {:click [[:ui/open-change node changeset]]}} "看修改"])
       (when-not (or reply? (= id (:reply-to ui)))
         [:button.text-btn {:type "button" :on {:click [[:ui/start-reply id]]}} "回覆"])
       (when (and (= author :user) (not reply?))
         [:button.text-btn {:type "button" :on {:click [[:notes/set-todo id (not todo)]]}}
          (if todo "取消待辦事項" "標為待辦事項")])
       (when (#{:user :knot} author)
         [:button.text-btn.text-danger {:type "button" :on {:click [[:notes/delete id]]}} "刪除"])]]
     ;; Knot 待辦：說明不重複寫段落名稱，觸發的段落列成下方可點的標籤（觸發多時自行換行）；
     ;; 觸發的卡片都已刪除時，改顯示留言原本的文字
     (let [live (filter #(contains? nodes (:id %)) triggers)
           tids (map :id live)]
       (if (empty? tids)
         [:p.note-text text]
         (let [numbers (tree/numbering nodes)]
           (list
            [:p.note-text {:replicant/key "lead"} (note/todo-lead live)]
            [:div.note-triggers {:replicant/key "triggers"}
            (for [tid tids]
              [:button.note-trigger {:replicant/key (str "trigger-" tid) :type "button"
                                     :title "開啟這張卡片"
                                     :on {:click [[:ui/open-note-card tid]]}}
               (note/card-ref nodes numbers tid)])]))))]))

(defn- reply-box [{:keys [ui]} {:note/keys [id]}]
  (let [draft (:reply-draft ui "")]
    [:div.note-compose.note-reply {:replicant/key (str "reply-" id)}
     [:textarea.note-input {:rows 2
                            :placeholder "回覆…"
                            :value draft
                            :replicant/on-mount [[:dom/focus]]
                            :on {:input [[:ui/set-reply-draft [:event/target-value]]]
                                 :compositionend [[:ui/set-reply-draft [:event/target-value]]]}}]
     [:div.note-compose-actions
      [:button.text-btn {:type "button" :on {:click [[:ui/cancel-reply]]}} "取消"]
      [:button.btn {:type "button" :disabled (str/blank? draft)
                    :on {:click [[:notes/reply id draft]]}} "回覆"]]]))

(defn- note-thread [state {n :note replies :replies}]
  [:div.thread {:replicant/key (:note/id n)}
   (note-line state n false)
   (for [r replies] (note-line state r true))
   (when (= (:note/id n) (get-in state [:ui :reply-to]))
     (reply-box state n))])

(defn- note-threads [state node-id]
  (for [t (note/threads (:notes state) node-id)]
    (note-thread state t)))

(defn- note-compose
  "新增留言；k 是卡片 id 或 :report。"
  [state k node-id placeholder]
  (let [draft (get-in state [:ui :note-drafts k] "")
        todo? (ui/note-todo? state k)]
    [:div.note-compose {:replicant/key (str "compose-" k)}
     [:textarea.note-input {:rows 2
                            :placeholder placeholder
                            :value draft
                            :on {:input [[:ui/set-note-draft k [:event/target-value]]]
                                 :compositionend [[:ui/set-note-draft k [:event/target-value]]]}}]
     [:div.note-compose-actions
      [:label.note-todo-toggle
       [:input {:type "checkbox" :checked todo?
                :on {:change [[:ui/set-note-todo k [:event/target-checked]]]}}]
       "交代 agent 處理（待辦事項）"]
      [:button.btn {:type "button" :disabled (str/blank? draft)
                    :on {:click [[:notes/add {:node node-id :text draft :todo todo?} k]]}}
       "留言"]]]))

;; 歷程（SPEC 5.4 卡片歷程）----------------------------------------------------

(def ^:private field-labels
  [[:name "名稱"] [:intent "意圖"] [:prose "本文"] [:table "表格內容"] [:basis "依據"]
   [:relations "關係"] [:position "位置"] [:status "狀態"] [:settings "設定"]])

(def ^:private kind-labels
  {:create "建立卡片" :import "匯入" :delete "刪除" :undo "復原" :change "修改"})

(defn- shown-fields
  "改了什麼：有本文時不列狀態（本文改了狀態自然跟著變）；依 field-labels 的順序。"
  [fields]
  (let [fields (cond-> fields (contains? fields :prose) (disj :status))]
    (for [[k label] field-labels :when (contains? fields k)] label)))

(defn- icon
  "時間軸圓形圖示裡的線條圖；path 是 16×16 的 SVG path。"
  ([path] (icon path 1.6))
  ([path stroke-width]
   [:svg {:width 12 :height 12 :viewBox "0 0 16 16" :fill "none" :stroke "currentColor"
          :stroke-width stroke-width :stroke-linecap "round" :stroke-linejoin "round"
          :aria-hidden "true"}
    [:path {:d path}]]))

(def ^:private icon-paths
  {:create "M8 3v10M3 8h10"
   :import "M8 2v8M5 7l3 3 3-3M3 13h10"
   :delete "M4 4l8 8M12 4l-8 8"
   :undo "M6 3L3 6l3 3M3 6h6.5a3.5 3.5 0 010 7H7"
   :pen "M3 13l1-3.5L10.5 3 13 5.5 6.5 12z"
   :link "M6.5 9.5l3-3M7 4.5l1-1a2.8 2.8 0 014 4l-1 1M9 11.5l-1 1a2.8 2.8 0 01-4-4l1-1"
   :move "M8 2v12M5 5l3-3 3 3M5 11l3 3 3-3"
   :comment "M3 3.5h10v7H7.5L4.5 13v-2.5H3z"
   :todo "M3.5 3.5h9v9h-9zM6 8l1.5 1.5L10.5 6"
   :more "M4 8h.01M8 8h.01M12 8h.01"})

(defn- event-icon-key [kind fields]
  (cond
    (#{:create :import :delete :undo} kind) kind
    (some fields [:prose :table :name :intent :basis]) :pen
    (= #{:relations} fields) :link
    (= #{:position} fields) :move
    :else :pen))

(defn- event-icon [{:keys [kind fields by]}]
  [:span.tl-icon {:class (when (= by :agent) "tl-agent")}
   (icon (icon-paths (event-icon-key kind (or fields #{}))))])

(defn- text-change [label {:keys [before after]}]
  (let [row (fn [sign cls v]
              [:p.tl-diff-row {:class cls}
               [:span.tl-diff-sign sign]
               (cond
                 (str/blank? v) [:span.tl-diff-empty "（空）"]
                 (= "−" sign) [:del.tl-diff-text v]
                 :else [:span.tl-diff-text v])])]
    [:div.tl-diff-field {:replicant/key label}
     [:div.tl-diff-name label]
     (row "−" "tl-diff-old" before)
     (row "+" "tl-diff-new" after)]))

(defn- history-event
  "時間軸上的一筆修改：圖示在左；右邊第一行是時間、操作者、修改名稱、改了什麼，名稱與意圖的變動列在下面。"
  [id {:keys [changeset-ids at label by kind undone? fields text-changes merged] :as entry}]
  (let [label (if (str/blank? label) (kind-labels kind) label)]
    [:div.tl-item.tl-event {:replicant/key (str "h-" (last changeset-ids))
                            :class [(when (= by :agent) "tl-event-agent") (when undone? "tl-undone")]}
     (event-icon entry)
     [:div.tl-body
      [:div.tl-line
       [:span.tl-time (time-text at)]
       [:span.tl-by {:class (if (= by :agent) "tl-by-agent" "tl-by-user")} (if (= by :agent) "agent" "使用者")]
       [:span.tl-label label]
       (for [f (shown-fields (or fields #{}))]
         [:span.tl-field {:replicant/key f} f])
       (when (and merged (> merged 1)) [:span.tl-merged (str "連續輸入 " merged " 次")])
       (when undone? [:span.tl-undone-tag "已復原"])
       (when (contains? fields :prose)
         [:button.text-btn.tl-show {:type "button"
                                    :on {:click [[:ui/show-change id (last changeset-ids)]]}} "看差異"])]
      ;; 已復原的修改與復原本身只留一行，同一個改動不重複列兩次前後文字
      (when (and (seq text-changes) (not undone?) (not= :undo kind))
        [:div.tl-diff
         (for [[k label] [[:name "名稱"] [:intent "意圖"]]
               :when (contains? text-changes k)]
           (text-change label (get text-changes k)))])]]))

(defn- thread-icon [{:note/keys [author todo] :as n}]
  (let [done? (= :done (:note/state n))]
    [:span.tl-icon {:class [(when (and (= author :agent) (not todo)) "tl-agent")
                            (when (and todo (not done?)) "tl-todo")]}
     (icon (icon-paths (if todo :todo :comment)))]))

(defn- timeline-thread [state {n :note :as thread}]
  [:div.tl-item.tl-thread {:replicant/key (:note/id n)}
   (thread-icon n)
   [:div.tl-body (note-thread state thread)]])

(defn- descendant-note-list
  "標題的「留言與歷程」最上方：底下各卡片等你回覆的提問與未完成的待辦事項，點一列開啟那張卡片的留言。"
  [{:keys [nodes notes]} id]
  (when (tree/heading? nodes id)
    (let [items (note/descendant-items nodes notes id)
          numbers (tree/numbering nodes)]
      (when (seq items)
        [:div.note-descendants {:replicant/key "descendants"}
         [:div.note-desc-head (str "下層的待辦事項與提問（" (count items) "）")]
         (for [{:keys [kind note]} items
               :let [{:note/keys [id node text]} note
                     question? (= :question kind)]]
           [:button.item.item-button {:replicant/key (str "d-" id) :type "button"
                                      :title (if question? "回覆這則提問" "開啟這張卡片的留言")
                                      :on {:click [(cond-> [:ui/open-note-thread node] question? (conj id))]}}
            [:span.item-tag {:class (when question? "tag-warm")} (if question? "等你回覆" "待辦事項")]
            [:span.item-position.note-desc-ref (note/card-ref nodes numbers node)]
            [:span.item-title (gist (first (str/split-lines (or text ""))))]])]))))

(defn- notes-zone
  "編輯區最後的「留言與歷程」：這張卡片的留言串與修改紀錄依時間穿插成一條時間軸（最新的在下方），最後是新增留言。"
  [state id]
  (let [n (get (note/todo-counts (:nodes state) (:notes state)) id 0)
        q (get (note/question-counts (:nodes state) (:notes state)) id 0)
        {h-id :id :keys [data]} (get-in state [:ui :history])
        {:keys [entries more?]} (when (= h-id id) data)
        items (history/timeline (note/threads (:notes state) id) entries)]
    (zone "notes" "留言與歷程"
          {:class "zone-notes"
           :hint "留言、待辦事項與這張卡片的修改紀錄；不會出現在報告中"
           :aside (when (pos? (+ n q))
                    [:span.note-tags
                     (when (pos? q) [:span.tag.tag-warm (str q " 則等你回覆")])
                     (when (pos? n) [:span.tag (str n " 則待辦事項")])])}
          (descendant-note-list state id)
          [:div.tl
           (when more?
             [:div.tl-item.tl-more {:replicant/key "more"}
              [:span.tl-icon (icon (icon-paths :more) 2.2)]
              [:div.tl-body
               [:div.tl-line
                [:button.text-btn {:type "button" :on {:click [[:history/show-all]]}}
                 "顯示較早的紀錄"]]]])
           (for [{:keys [type thread entry]} items]
             (if (= :thread type)
               (timeline-thread state thread)
               (history-event id entry)))]
          (note-compose state id id "交代 agent 的事、或給自己的備忘…"))))

;; 各類卡片 -------------------------------------------------------------------

(defn- paragraph-editor [{:keys [nodes] :as state} title id]
  (let [node (get nodes id)
        editing? (get-in state [:ui :editing-relations])
        rels (relation-rows state title id editing?)
        kids (tree/child-ids nodes id)]
    (list
     (zone "inputs" "生成依據"
           {:hint "意圖、依據與關係是 agent 撰寫時的依據，不會出現在報告中"}
           (intent-field id (:node/draft node) "這段要說什麼、要強調什麼、用什麼語氣。可以分行列點。")
           (target-field id (:node/target-chars node) nil)
           (citations state node)
           (field "rels" "關係"
                  {:editing? editing?
                   :action [:div.field-actions
                            (when (and editing? (not (get-in state [:ui :rel-picker])))
                              (field-button "新增" [[:ui/open-rel-picker]]))
                            (field-button (if editing? "完成" "編輯") [[:ui/toggle-relations-edit]])]}
                  (if (seq rels)
                    [:div.items rels]
                    [:span.muted.small "與前後段的關係會在這裡出現；不相鄰的段落可在「編輯」後新增。"])
                  (when (get-in state [:ui :rel-picker])
                    (rel-picker state title id)))
           (field "kids" "下層表與圖"
                  {:count (when (seq kids) (count kids))
                   :action [:div.field-actions
                            (field-button "新增表格" [[:ui/add-child id :table]])
                            (field-button "新增圖" [[:ui/add-child id :figure]])]}
                  (if (seq kids)
                    [:div.items
                     (for [k kids]
                       (item-row k {:tag (if (= :figure (tree/kind nodes k)) "圖" "表格")
                                    :tag-class "tag-warm"
                                    :title (title k) :on-title [[:ui/select k]]
                                    :meta [:span.mono (get-in nodes [k :node/label])]}))]
                    [:span.muted.small "這段要搭配的表格或圖，建立後在本文以 label 引用。"])))
     (context-zone state id)
     (prose-zone state node "尚未有本文：可直接在下方撰寫，或寫好意圖並加入依據後在「留言與歷程」交代 agent 撰寫。")
     (notes-zone state id))))

(defn- heading-editor [{:keys [nodes] :as state} title id]
  (let [node (get nodes id)
        kids (tree/child-ids nodes id)
        intro (tree/intro-id nodes id)
        has-paras (some #(tree/paragraph? nodes %) kids)
        can-sub (< (tree/depth nodes id) tree/max-heading-level)]
    (list
     (zone "inputs" "設定" {:hint "agent 撰寫下層段落與引言時會參考這裡的意圖"}
           (intent-field id (:node/draft node) "這一章／節要涵蓋什麼、回應哪些契約項目、與前後章節的關係。")
           (let [level (get heading-level-names (dec (tree/depth nodes id)))
                 t (:node/target-chars node)
                 chars (get (tree/char-counts state) id)
                 info (when (pos? chars) (count-info chars t))]
             (target-field id t
                           (when info
                             (count-span "target-total" (update info :text #(str "整" level "合計 " %))))))
           (when-let [items (seq (:node/contract-items node))]
             (field "contract" "契約項目" {:count (count items)}
                    [:div.chips (for [c items] [:span.tag {:replicant/key c} c])])))
     (zone "structure" "結構" {:hint "引言固定在最前面；其後依序是段落、下層標題"}
           (field "intro" "引言"
                  {:action (when-not intro (field-button "新增" [[:ui/add-child id :intro]]))}
                  (if intro
                    [:button.kid {:type "button" :on {:click [[:ui/select intro]]}}
                     (let [d (get-in nodes [intro :node/draft])]
                       (if (str/blank? d) "（由 agent 依下層內容撰寫）" (gist d)))]
                    [:span.muted.small "這一層還沒有引言。"]))
           (let [shown (remove #{intro} kids)]
             (field "kids" "下層內容"
                    {:count (when (seq shown) (count shown))
                     :action [:div.field-actions
                              (field-button "新增段落" [[:ui/add-child id :paragraph]])
                              (when can-sub
                                (field-button "新增下層標題" [[:ui/add-child id :heading]]))
                              (when (and can-sub has-paras)
                                (field-button "新增下層標題並移入段落" [[:ui/wrap-paragraphs id]]))]}
                    (if (seq shown)
                      [:div.kids
                       (for [k shown]
                         [:button.kid {:replicant/key k :type "button" :on {:click [[:ui/select k]]}}
                          (title k)])]
                      [:span.muted.small "這一層還沒有內容。"]))))
     (notes-zone state id))))

(defn- intro-editor [{:keys [nodes] :as state} _title id]
  (let [node (get nodes id)]
    (list
     (zone "inputs" "生成依據" {:hint "可留空；agent 會依這一層的下層內容撰寫引言"}
           (intent-field id (:node/draft node) "想強調的重點或語氣寫在這裡，agent 會依下層內容撰寫引言。")
           (target-field id (:node/target-chars node) nil))
     (context-zone state id)
     (prose-zone state node "尚未有本文：可直接撰寫，或在下層內容大致完成後於「留言與歷程」交代 agent 撰寫。"
                 [:span.small.muted {:replicant/key "note"} "引言適合在下層內容大致完成後才撰寫"])
     (notes-zone state id))))

(defn- float-editor [{:keys [nodes] :as state} id]
  (let [node (get nodes id)
        table? (= :table (:node/kind node))]
    (list
     (zone "inputs" "設定" {:hint "段落撰寫時會以 label 引用這張表／圖"}
           (intent-field id (:node/draft node) "這張表／圖要呈現什麼、圖說的重點。")
           (field "label" "LaTeX label" nil [:span.mono.boxed-muted (:node/label node)])
           (when-let [f (:node/figure-file node)]
             (field "file" "圖檔" nil [:span.mono.boxed-muted f]))
           (when-not table?
             [:div.figure-placeholder {:replicant/key "placeholder"} "圖檔在匯出 LaTeX 時處理"]))
     (when table?
       (zone "content" "表格內容"
             {:aside [:span.small.muted "agent 以 write_table 填寫"]}
             (if (table-filled? node)
               (table-card-view (:node/table-content node))
               [:p.muted.small "尚未填寫：在下方「留言與歷程」交代 agent 填寫表格內容。"])))
     (notes-zone state id))))

;; 抽屜 ----------------------------------------------------------------------

(defn- chevron [direction]
  [:svg {:width 16 :height 16 :viewBox "0 0 16 16" :fill "none" :stroke "currentColor"
         :stroke-width 2 :stroke-linecap "round" :stroke-linejoin "round" :aria-hidden "true"}
   [:path {:d (if (= direction :left) "M10 3 5 8l5 5" "M6 3l5 5-5 5")}]])

(defn- editor-rail
  "收合後的編輯區：看板右緣的把手，顯示選取的卡片並可展開。"
  [nodes title id]
  [:aside.editor-rail {:aria-label "編輯區（已收合）"}
   [:button.rail-toggle {:type "button" :title "展開編輯區" :aria-label "展開編輯區" :aria-expanded "false"
                         :on {:click [[:ui/toggle-editor]]}}
    (chevron :left)]
   [:button.rail-title {:type "button" :title "展開編輯區" :on {:click [[:ui/toggle-editor]]}}
    [:span.rail-kind (kind-label nodes id)]
    [:span.rail-name (title id)]]])

(defn- structure-actions
  "結構操作（不影響本文），放在標題列的「⋯」選單。"
  [nodes id]
  (let [parent-id (tree/parent nodes id)]
    (cond-> []
      (and (tree/paragraph? nodes id)
           (not (tree/intro-id nodes parent-id))
           (empty? (tree/child-ids nodes id)))
      (conj [:button.menu-item {:replicant/key "to-intro" :type "button"
                                :on {:click [[:ui/paragraph->intro id]]}}
             "改為引言"])
      ;; 全文頁沒有看板的工具列，刪除也放在這裡（與工具列的「刪除」相同，可以復原）
      id
      (conj [:button.menu-item.text-danger {:replicant/key "delete" :type "button"
                                            :title "刪除這張卡片與其下層卡片；可以復原"
                                            :on {:click [[:ui/delete-node id]]}}
             "刪除"]))))

(defn- editor-pane
  "編輯區。卡片頁是覆蓋看板的抽屜，可收合；docked? 為真時（全文頁）固定在右側、不收合。"
  [{:keys [nodes ui] :as state} title & {:keys [docked?]}]
  (let [id (:selected ui)
        node (get nodes id)
        k (:node/kind node)
        crumbs (crumb-titles nodes title id)
        focus? (= id (:focus-title ui))
        actions (structure-actions nodes id)]
    (if (and (:editor-collapsed ui) (not docked?))
      (editor-rail nodes title id)
      [:aside.editor {:aria-label "編輯區" :class (when docked? "editor-docked")}
       [:div.detail-head
        [:div.editor-top
         [:span.muted.small (str (kind-label nodes id) " · " (if (seq crumbs) (str/join " / " crumbs) "報告"))]
         (when (seq actions)
           ;; 換卡片時重建，避免選單停在打開的狀態
           [:details.more-menu {:replicant/key (str "menu-" id)}
            [:summary {:title "其他操作" :aria-label "其他操作"} "⋯"]
            [:div.menu (seq actions)]])
         (when-not docked?
           [:button.collapse-btn {:type "button" :title "收合編輯區" :aria-label "收合編輯區" :aria-expanded "true"
                                  :on {:click [[:ui/toggle-editor]]}}
            (chevron :right)])]
        (case k
          ;; 名稱框：編號是固定的前綴，與名稱同在一個框內，只有名稱可以改
          :heading [:div.title-row
                    [:label.title-field
                     [:span.title-number (some-> (title id) (str/split #" " 2) first)]
                     (name-input id :node/name (:node/name node) "標題名稱" focus?)]]
          :intro [:h2 "引言"]
          [:div.title-row
           [:label.title-field (name-input id :node/title (:node/title node) "名稱" focus?)]
           (when-let [{:keys [label class]} (and (= :paragraph k) (status-pill (or (:node/status node) :none)))]
             [:span.pill.title-pill {:class class} label])])]
       (case k
         :paragraph (paragraph-editor state title id)
         :heading (heading-editor state title id)
         :intro (intro-editor state title id)
         (float-editor state id))])))

;; ---------------------------------------------------------------------------
;; 素材庫頁面

(defn- section [heading & body]
  [:div.detail-section [:h3 heading] body])

(defn- table-view
  "表格素材的內容；max-cols、max-rows 為 nil 時顯示全部。
  marked 為 [列索引 欄名] 的集合：標出事實指向的儲存格。"
  [block & [max-cols max-rows marked]]
  (let [all-cols (:table/columns block)
        cols (cond->> all-cols max-cols (take max-cols))
        rows (cond->> (:table/rows block) max-rows (take max-rows))
        marked (set marked)
        mark-cols (set (map second marked))
        col-at (fn [ci] (nth all-cols ci nil))]
    [:div.table-preview {:style {:grid-template-columns (str "repeat(" (count cols) ", minmax(0, 1fr))")}}
     (for [c cols] [:span.th {:replicant/key c :class (when (mark-cols c) "cell-col")} c])
     (for [[ri row] (map-indexed vector rows)
           [ci cell] (map-indexed vector (take (count cols) row))]
       [:span {:replicant/key [ri ci]
               :class (when (marked [ri (col-at ci)]) "cell-mark")} cell])]))

(defn- citing-nodes [nodes block-id]
  (->> (vals nodes)
       (filter #(some (fn [c] (= block-id (:block c))) (:node/citations %)))
       (map :node/id)))

(defn block-facts
  "素材上的事實，依 key 排序。"
  [facts block-id]
  (sort-by :fact/key (filter #(= block-id (:fact/block %)) (vals facts))))

(defn fact-cell-text
  "事實指向表格儲存格時的說明「第 N 列・欄名」（N 從 1 算）；沒有指定時為 nil。"
  [{:fact/keys [row column]}]
  (when (some? row) (str "第 " (inc row) " 列・" column)))

(defn- library-card [nodes facts selected {:block/keys [id title text source location] :as block}]
  (let [n (count (citing-nodes nodes id))
        nf (count (block-facts facts id))]
    [:button.block {:replicant/key id
                    :type "button"
                    :class (cond-> []
                             (sources/discarded? block) (conj "block-discarded")
                             (= id selected) (conj "block-selected"))
                    :on {:click [[:ui/select-block id]]}}
     [:span.block-meta
      (block-tag block)
      [:span (str source (when location (str " · " location)))]
      (block-status block)]
     [:span.block-title title]
     (cond
       (sources/table? block)
       (list (table-view block 3 2)
             [:span.muted.small {:replicant/key "dims"}
              (str (count (:table/columns block)) " 欄 × " (count (:table/rows block)) " 列")])
       text [:span.block-text text])
     [:span.small
      [:span {:class (if (pos? nf) "text-navy" "muted")} (str nf " 個事實")]
      [:span.muted " · "]
      [:span {:class (if (pos? n) "text-navy" "muted")}
       (if (pos? n) (str "被 " n " 個段落引用") "尚未被引用")]]]))

(defn- fact-row [{:fact/keys [key label] :as fact}]
  [:div.fact-row {:replicant/key key}
   [:span.fact-label label]
   [:span.chip.chip-fact (sources/fact-display fact)]
   [:code.fact-key key]
   (when-let [cell (fact-cell-text fact)] [:span.fact-cell cell])])

(defn- library-detail [{:keys [nodes facts]} title block]
  (if-not block
    [:aside.lib-detail [:p.muted "選取左側的素材查看內容。"]]
    (let [{:block/keys [id source location note]} block
          citing (citing-nodes nodes id)
          bfacts (block-facts facts id)
          cells (keep (fn [f] (when (some? (:fact/row f)) [(:fact/row f) (:fact/column f)])) bfacts)]
      [:aside.lib-detail
       [:div.detail-head
        [:span.block-meta (block-tag block) [:span (str source (when location (str " · " location)))] (block-status block)]
        [:h2 (:block/title block)]]
       [:div.move-row
        (if (sources/discarded? block)
          [:button.btn.btn-small {:type "button" :on {:click [[:ui/set-block-status id :confirmed]]}} "恢復"]
          [:button.btn.btn-small {:type "button" :on {:click [[:ui/set-block-status id :discarded]]}} "捨棄"])]
       (when note
         (section "用途說明" [:p.block-note note]))
       (section (if (sources/table? block) "表格" "原文")
                (if (sources/table? block)
                  (table-view block nil nil cells)
                  [:div.boxed (:block/text block)]))
       (section (str "事實" (when (seq bfacts) (str "（" (count bfacts) "）")))
                (if (seq bfacts)
                  [:div.fact-list (map fact-row bfacts)]
                  [:span.muted.small "這份素材還沒有登記事實"]))
       (section "引用它的段落"
                (if (seq citing)
                  [:div.kids
                   (for [n citing]
                     [:button.kid {:replicant/key n :type "button" :on {:click [[:ui/open-paragraph n]]}}
                      (title n)])]
                  [:span.muted.small "尚未被引用"]))])))

(def empty-library-text
  "還沒有素材：請 agent 從原始資料節錄有用的片段與表格（MCP 的 add_excerpt、add_source_table），並登記其中的事實。")

(defn- library-page [{:keys [blocks nodes facts ui] :as state} title]
  (let [{:keys [source-filter source-query library-selected source-file show-discarded]} ui
        shown (filter-blocks blocks source-filter source-query
                             {:source source-file :include-discarded? show-discarded})
        counts (sources/source-counts blocks)
        discarded (count (filter sources/discarded? blocks))
        selected (some #(when (= library-selected (:block/id %)) %) blocks)]
    (if (empty? blocks)
      [:div.library-page
       [:div.empty-state.lib-empty
        [:h2 "素材庫"]
        [:p.muted empty-library-text]]]
      [:div.library-page
       [:aside.lib-filters
        [:div.panel-head
         [:h2 "素材庫"]
         [:span.muted.small (str (- (count blocks) discarded) " 筆素材")]]
        [:div.field
         [:label.muted.small {:for "lib-search"} "搜尋素材"]
         [:input#lib-search {:type "search"
                             :placeholder "多個詞以空白分隔"
                             :value source-query
                             :on {:input [[:ui/set-source-query [:event/target-value]]]
                                  :compositionend [[:ui/set-source-query [:event/target-value]]]}}]]
        (filter-chips source-filter :ui/set-source-filter)
        [:div.detail-section
         [:h3 "來源檔"]
         (for [{src :source n :count} counts]
           [:button.source-row {:replicant/key src
                                :type "button"
                                :title src
                                :aria-pressed (str (= src source-file))
                                :on {:click [[:ui/toggle-source-file src]]}}
            [:span.source-name src]
            [:span.muted.small.nowrap (str n " 筆")]])]
        [:label.check-row
         [:input {:type "checkbox"
                  :checked (boolean show-discarded)
                  :on {:change [[:ui/toggle-show-discarded]]}}]
         [:span (str "顯示已捨棄" (when (pos? discarded) (str "（" discarded "）")))]]]
       [:section.lib-list {:aria-label "素材"}
        [:div.lib-count.muted.small
         (str "符合 " (count shown) " 筆素材"
              (when (> (count shown) list-limit) (str "，顯示前 " list-limit " 筆；請用搜尋或篩選縮小範圍")))]
        (if (seq shown)
          (map #(library-card nodes facts library-selected %) (take list-limit shown))
          [:span.muted.small "沒有符合的素材"])]
       (library-detail state title selected)])))

;; ---------------------------------------------------------------------------
;; 風格頁（SPEC 5.7）

(def style-hint-text
  "agent 撰寫每一段時都會依這份風格。可以貼上機關的撰寫規範、過去報告的範例段落，或直接寫下要求。")

(def style-placeholder
  "可以寫的項目，例如：\n用語與語氣（例如：使用「本計畫」、避免口語）\n人稱\n數字與單位寫法\n標點與全形半形\n常見禁忌\n範例段落")

(defn- style-page
  "風格頁：置中的單欄，頁首說明與字數、最後修改，主體是與意圖同一種編輯框。"
  [{:keys [style-info ui] :as state}]
  (let [chars (ui/style-chars state)
        {:keys [updated-by updated-at]} style-info]
    [:section.style-page {:aria-label "風格"}
     (conflict-view (:conflict ui))
     (toast-view (:toast ui))
     [:div.style-paper
      [:div.style-head
       [:div.style-title
        [:h2 "風格"]
        [:p.style-hint style-hint-text]]
       [:span.style-meta
        (str chars " 字"
             (when (and (pos? chars) updated-by)
               (str " · 最後修改：" (if (= :agent updated-by) "agent" "使用者")
                    (when updated-at (str " · " (time-text updated-at))))))]]
      (edit-box "style-box"
                [:textarea.intent.style-input {:replicant/key "style-input"
                                               :aria-label "風格"
                                               :placeholder style-placeholder
                                               :value (ui/style-text state)
                                               :on {:input [[:ui/set-style-text [:event/target-value]]]
                                                    :compositionend [[:ui/set-style-text [:event/target-value]]]}}])]]))

;; ---------------------------------------------------------------------------

(defn app [state]
  (cond
    (:error state)
    [:main.app [:div.empty-state [:h2 "無法載入報告"] [:p.muted (:error state)]]]

    (not (:report state))
    [:main.app [:div.loading "載入中…"]]

    (ui/report-setup-needed? state)
    (let [{:keys [title subtitle]} (ui/report-setup-draft state)
          input (fn [field label value placeholder]
                  [:label.field {:replicant/key (name field)}
                   [:span.small label]
                   [:input {:type "text" :value value :placeholder placeholder
                            :on {:input [[:ui/set-report-setup-draft field [:event/target-value]]]
                                 :compositionend [[:ui/set-report-setup-draft field [:event/target-value]]]
                                 :keydown [[:report/setup-keydown [:event/key]]]}}]])]
      [:main.app
       [:div.empty-state.report-setup
        [:h2 "為這份報告取個名字"]
        (input :title "報告名稱" title "")
        (input :subtitle "副標（選填）" subtitle "例如：成果報告")
        [:div.add-row
         [:button.btn.btn-primary {:type "button" :disabled (not (ui/report-setup-valid? state))
                                   :on {:click [[:report/save-setup]]}}
          "確定"]]]])

    (empty? (:nodes state))
    [:main.app
     [:div.empty-state
      [:h2 (get-in state [:report :report/title])]
      [:p.muted "這份報告還沒有內容"]
      [:div.add-row
       [:button.btn.btn-primary {:type "button" :on {:click [[:ui/add-child nil :heading]]}} "新增第一章"]
       ;; 開發模式才有（SPEC：一般模式不提供）
       (when (:dev? state)
         [:button.btn {:type "button" :on {:click [[:server/import-sample]]}} "匯入範例內容"])]]]

    :else
    (let [nodes (:nodes state)
          numbers (tree/numbering nodes)
          title #(tree/display-title nodes numbers %)
          page (get-in state [:ui :page])]
      [:main.app
       (topbar state title)
       (case page
         :library (library-page state title)
         :style (style-page state)
         :fulltext [:div.workspace.workspace-fulltext
                    (outline state title)
                    (fulltext state title)
                    (editor-pane state title :docked? true)]
         [:div.workspace (tree-panel state title) (editor-pane state title)])])))
