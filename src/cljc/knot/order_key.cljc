(ns knot.order-key
  "分數索引：同一 parent 內以字串排序，插入時只產生新 key，不改動其他卡片。
  key 由 base62 字元組成，且不以最小字元結尾，因此任兩個 key 之間一定還能再插入。")

(def ^:private digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
(def ^:private base (count digits))

(defn- digit [c] (.indexOf ^String digits (str c)))
(defn- char-at [s i] (when (< i (count s)) (nth s i)))

(defn- midpoint
  "回傳嚴格介於 a 與 b 之間的 key。a 可為空字串（下界），b 可為 nil（無上界）。"
  [a b]
  (let [n (if b
            (loop [n 0]
              (if (and (< n (count b))
                       (= (or (char-at a n) \0) (char-at b n)))
                (recur (inc n))
                n))
            0)]
    (if (pos? n)
      (str (subs b 0 n)
           (midpoint (if (< n (count a)) (subs a n) "") (subs b n)))
      (let [da (if (seq a) (digit (first a)) 0)
            db (if b (digit (first b)) base)]
        (if (> (- db da) 1)
          (str (nth digits (quot (+ da db) 2)))
          (if (and b (> (count b) 1))
            (subs b 0 1)
            (str (nth digits da)
                 (midpoint (if (seq a) (subs a 1) "") nil))))))))

(defn between
  "產生介於 a 與 b 之間的 key；a 為 nil 表示最前面，b 為 nil 表示最後面。"
  [a b]
  (when (and a b (not (neg? (compare a b))))
    (throw (ex-info "order-key 的下界必須小於上界" {:a a :b b})))
  (midpoint (or a "") b))

(defn initial-keys
  "依序產生 n 個遞增的 key。"
  [n]
  (vec (take n (rest (iterate #(between % nil) nil)))))
