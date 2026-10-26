(ns knot.app
  (:require [cljs.reader :as reader]
            [knot.ui :as ui]
            [knot.views :as views]
            [nexus.core :as nexus]
            [replicant.dom :as r]))

(defonce store (atom {}))

;; 純狀態轉換：action 只把結果交給 :store/reset
(def transitions
  {:ui/select ui/select
   :ui/navigate ui/navigate
   :ui/dismiss-toast ui/dismiss-toast
   :ui/move-selected ui/move-selected
   :ui/edge-scroll ui/edge-scroll
   :ui/scroll-column ui/scroll-column
   :ui/add-sibling ui/add-sibling
   :ui/delete-node ui/delete-node
   :ui/clear-focus-title ui/clear-focus-title
   :ui/set-viewport-height ui/set-viewport-height
   :ui/add-child ui/add-child
   :ui/wrap-paragraphs ui/wrap-paragraphs
   :ui/paragraph->intro ui/paragraph->intro
   :ui/toggle-lock ui/toggle-lock
   :ui/undo ui/undo
   :ui/set-source-filter ui/set-source-filter
   :ui/set-source-query ui/set-source-query
   :ui/set-page ui/set-page
   :ui/set-node-field ui/set-node-field
   :ui/add-citation ui/add-citation
   :ui/remove-citation ui/remove-citation
   :ui/set-relation ui/set-relation
   :ui/open-picker ui/open-picker
   :ui/close-picker ui/close-picker
   :ui/set-picker-query ui/set-picker-query
   :ui/set-picker-filter ui/set-picker-filter
   :ui/select-block ui/select-block
   :ui/toggle-source-file ui/toggle-source-file
   :ui/set-block-status ui/set-block-status
   :ui/open-paragraph ui/open-paragraph
   :drag/end ui/drag-end})

(defn- transition-action [f]
  (fn [state & args]
    ;; 中文輸入法選字中（isComposing）不更新，等 compositionend 再寫入
    (when-not (some #{::composing} args)
      (let [state' (apply f state args)]
        (when-not (identical? state state')
          [[:store/reset state']])))))

(defn- reduced-motion? []
  (.-matches (js/window.matchMedia "(prefers-reduced-motion: reduce)")))

(defn- reveal-columns!
  "選取時水平捲動，讓選取卡片所在欄與其上、下一欄都在可視範圍內。只在選取改變時捲動。"
  [^js el depth selected n-cols]
  (when (not= selected (.-knotRevealed el))
    (let [first-time? (nil? (.-knotRevealed el))
          _ (set! (.-knotRevealed el) selected)
          col-w (some-> (.querySelector el ".tree-col") .-offsetWidth)
          stride (+ (or col-w 0) views/column-gap)
          start (* stride (max 0 (- depth 2)))
          end (- (* stride (min n-cols (inc depth))) views/column-gap)
          left (.-scrollLeft el)
          width (.-clientWidth el)
          target (cond
                   (or (< start left) (> (- end start) width)) start
                   (> end (+ left width)) (- end width)
                   :else left)]
      (when (not= target left)
        (.scrollTo el #js {:left target
                           :behavior (if (or first-time? (reduced-motion?)) "auto" "smooth")})))))

(declare dispatch!)

(def nexus
  {:nexus/system->state deref

   :nexus/actions
   (merge
    (update-vals transitions transition-action)
    {:drag/start
     (fn [state id]
       [[:event/set-drag-data id]
        [:store/reset (ui/drag-start state id)]])

     :drag/over
     (fn [state target half]
       (let [state' (ui/drag-over state target half)]
         (cond-> []
           (get-in state' [:ui :over]) (conj [:event/prevent-default])
           (not (identical? state state')) (conj [:store/reset state']))))

     :drag/drop
     (fn [state]
       [[:event/prevent-default]
        [:store/reset (ui/drop-dragged state)]])

     ;; 滾輪在卡片欄位上只捲動該欄，不捲動整頁
     :ui/wheel-column
     (fn [_ column delta lo hi]
       [[:event/prevent-default]
        [:ui/scroll-column column delta lo hi]])})

   :nexus/effects
   {:store/reset
    (fn [_ store state] (reset! store state))

    :event/prevent-default
    (fn [{:keys [dispatch-data]} _]
      (some-> (:replicant/dom-event dispatch-data) .preventDefault))

    :event/set-drag-data
    (fn [{:keys [dispatch-data]} _ id]
      (when-let [dt (some-> (:replicant/dom-event dispatch-data) .-dataTransfer)]
        (set! (.-effectAllowed dt) "move")
        (.setData dt "text/plain" id)))

    :dom/focus
    (fn [{:keys [dispatch-data]} _]
      (some-> (:replicant/node dispatch-data) .focus))

    :dom/select-all
    (fn [{:keys [dispatch-data]} _]
      (when-let [el (:replicant/node dispatch-data)]
        (.focus el)
        (.select el)))

    ;; 卡片欄位的可視高度由 CSS 決定；量測後交給版面計算選取置中
    :dom/observe-viewport
    (fn [_ _]
      (let [measure (fn []
                      (when-let [vp (js/document.querySelector ".col-viewport")]
                        (let [h (.-clientHeight vp)]
                          (when (and (pos? h) (not= h (get-in @store [:ui :viewport-h])))
                            (dispatch! {} [[:ui/set-viewport-height h]])))))]
        (when-let [el (js/document.querySelector ".tree-scroll")]
          (.observe (js/ResizeObserver. measure) el))
        (measure)))

    :dom/reveal-columns
    (fn [{:keys [dispatch-data]} _ depth selected n-cols]
      (when-let [el (:replicant/node dispatch-data)]
        (reveal-columns! el depth selected n-cols)))}

   :nexus/placeholders
   {:event/target-value
    (fn [{:replicant/keys [dom-event]}]
      (if (some-> dom-event .-isComposing)
        ::composing
        (some-> dom-event .-target .-value)))

    ;; <select> 的值轉為 keyword；空字串為 nil
    :event/target-keyword
    (fn [{:replicant/keys [dom-event]}]
      (let [v (some-> dom-event .-target .-value)]
        (when-not (or (nil? v) (= "" v)) (keyword v))))

    ;; 滾輪往下時內容往上移
    :event/wheel-delta
    (fn [{:replicant/keys [dom-event]}]
      (when dom-event
        (let [unit (case (.-deltaMode dom-event) 1 16 2 400 1)]
          (- (* unit (.-deltaY dom-event))))))

    ;; 指標在卡片的上半或下半
    :event/half
    (fn [{:replicant/keys [dom-event]}]
      (when dom-event
        (let [rect (.getBoundingClientRect (.-currentTarget dom-event))]
          (if (< (.-clientY dom-event) (+ (.-top rect) (/ (.-height rect) 2))) :top :bottom))))

    ;; 拖曳時指標靠近欄位上下緣：往下捲為負、往上捲為正
    :event/edge-delta
    (fn [{:replicant/keys [dom-event]}]
      (when dom-event
        (let [rect (.getBoundingClientRect (.-currentTarget dom-event))
              y (- (.-clientY dom-event) (.-top rect))]
          (cond (< y 56) 12
                (> y (- (.-height rect) 56)) -12
                :else 0))))}})

(defn dispatch! [dispatch-data actions]
  (nexus/dispatch nexus store dispatch-data actions))

(defn render! [state]
  (r/render (js/document.getElementById "app") (views/app state)))

(defn reload! []
  (render! @store))

(defn- editable-target? [^js e]
  (let [t (.-target e)]
    (or (#{"INPUT" "TEXTAREA" "SELECT"} (.-tagName t))
        (.-isContentEditable t))))

(defn- on-keydown [^js e]
  (when-not (editable-target? e)
    (let [k (.-key e)
          undo? (and (or (.-metaKey e) (.-ctrlKey e)) (not (.-shiftKey e)) (= "z" (.toLowerCase k)))
          dir ({"ArrowLeft" :left "ArrowRight" :right "ArrowUp" :up "ArrowDown" :down} k)]
      (cond
        undo? (do (.preventDefault e) (dispatch! {} [[:ui/undo]]))
        (and dir (= :tree (get-in @store [:ui :page])))
        (do (.preventDefault e) (dispatch! {} [[:ui/navigate dir]]))))))

(defn- load-sample! []
  (-> (js/fetch "/api/sample-report" #js {:headers #js {"Accept" "application/edn"}})
      (.then (fn [res]
               (if (.-ok res)
                 (.text res)
                 (throw (js/Error. (str "載入範例報告失敗：" (.-status res)))))))
      (.then (fn [text]
               ;; ?select=<id>、?page=library|tree|fulltext 指定初始畫面，方便對照設計稿與截圖
               (let [params (js/URLSearchParams. js/location.search)
                     select (.get params "select")
                     page (.get params "page")]
                 (reset! store (cond-> (ui/init-state (reader/read-string text))
                                 select (ui/select select)
                                 page (ui/set-page (keyword page)))))))
      (.catch (fn [err] (js/console.error err)))))

(defn init []
  (r/set-dispatch! dispatch!)
  (add-watch store ::render (fn [_ _ _ state] (render! state)))
  (js/document.addEventListener "keydown" on-keydown)
  (render! @store)
  (load-sample!))
