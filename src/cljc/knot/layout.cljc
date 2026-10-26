(ns knot.layout
  "卡片樹依深度分欄的版面計算：每張卡片在欄內的位置，以及選取後各欄的垂直位移。"
  (:require [knot.tree :as tree]))

(def card-height {:heading 84 :intro 76 :paragraph 104 :table 76 :figure 76})
(def gap-in-group 8)
(def gap-between-paragraphs 30) ; 留空間顯示段落關係
(def gap-between-groups 24)
(def top-margin 24)
(def max-columns 6)

(defn columns
  "第 n 欄（0 起算）是深度 n+1 的所有卡片，依閱讀順序排列，同一 parent 為一組。
  回傳 [[{:id :top :height :gap-before :order} ...] ...]。"
  [nodes]
  (let [order (tree/document-order nodes)
        order-index (zipmap order (range))
        by-depth (group-by #(tree/depth nodes %) order)
        n (min max-columns (reduce max 0 (keys by-depth)))]
    (vec
     (for [d (range 1 (inc n))]
       (loop [[id & more] (get by-depth d)
              prev nil
              y 0
              acc []]
         (if-not id
           acc
           (let [node (get nodes id)
                 h (card-height (:node/kind node))
                 gap (cond
                       (nil? prev) 0
                       (not= (:node/parent node) (get-in nodes [prev :node/parent])) gap-between-groups
                       (and (tree/paragraph? nodes id) (tree/paragraph? nodes prev)) gap-between-paragraphs
                       :else gap-in-group)
                 top (+ y gap)]
             (recur more id (+ top h)
                    (conj acc {:id id :top top :height h :gap-before gap
                               :order (order-index id)})))))))))

(defn column-height [col]
  (if-let [last-card (peek col)]
    (+ (:top last-card) (:height last-card))
    0))

(defn- span-offset
  "讓 [t, b] 對齊欄位中線的位移；範圍高於可視區時改為頂端對齊。"
  [t b viewport-h]
  (if (> (- b t) (- viewport-h (* 2 top-margin)))
    (- top-margin t)
    (- (/ viewport-h 2) (/ (+ t b) 2))))

(defn offsets
  "選取 selected 後每欄的垂直位移（SPEC 5.3 選取置中）：祖先欄對齊該祖先；
  同層欄對齊選取的卡片；子孫欄對齊所有子孫的整體範圍；沒有子孫時對齊它們
  在樹中應有的位置。"
  [nodes cols selected viewport-h]
  (let [anc (set (tree/ancestor-ids nodes selected))
        desc (set (tree/descendant-ids nodes selected))
        sel-depth (tree/depth nodes selected)
        sel-order (some #(when (= selected (:id %)) (:order %)) (get cols (dec sel-depth)))]
    (vec
     (map-indexed
      (fn [i col]
        (let [d (inc i)
              [t b] (cond
                      (< d sel-depth)
                      (let [a (some #(when (anc (:id %)) %) col)]
                        [(:top a) (+ (:top a) (:height a))])

                      (= d sel-depth)
                      (let [a (some #(when (= selected (:id %)) %) col)]
                        [(:top a) (+ (:top a) (:height a))])

                      :else
                      (let [ds (filter #(desc (:id %)) col)]
                        (if (seq ds)
                          [(:top (first ds)) (+ (:top (last ds)) (:height (last ds)))]
                          (let [nx (some #(when (> (:order %) sel-order) %) col)
                                y (if nx
                                    (- (:top nx) (/ (:gap-before nx) 2))
                                    (+ (column-height col) (/ gap-between-groups 2)))]
                            [y y]))))]
          (span-offset t b viewport-h)))
      cols))))
