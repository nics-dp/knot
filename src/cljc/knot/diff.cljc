(ns knot.diff
  "段落修改前後的文字差異。中文逐字比較，英數字以連續字串為單位，避免把一個字拆開。")

(defn- tokens [s]
  (vec (re-seq #"[A-Za-z0-9_.%,]+|\s+|[\s\S]" (or s ""))))

(defn- common-prefix [a b]
  (count (take-while true? (map = a b))))

(defn- lcs-ops
  "a、b 為 token 向量，回傳 [[op token] ...]，op 為 :same :del :ins。"
  [a b]
  (let [n (count a) m (count b)
        w (inc m)
        ;; t[i*w+j]：a[i..]、b[j..] 的最長共同子序列長度
        t #?(:clj (long-array (* (inc n) w)) :cljs (.fill (js/Array. (* (inc n) w)) 0))
        at (fn [i j] (aget t (+ (* i w) j)))]
    (doseq [i (range (dec n) -1 -1)
            j (range (dec m) -1 -1)]
      (let [v (if (= (a i) (b j))
                (inc (at (inc i) (inc j)))
                (max (at (inc i) j) (at i (inc j))))]
        (aset t (+ (* i w) j) #?(:clj (long v) :cljs v))))
    (loop [i 0 j 0 acc (transient [])]
      (cond
        (and (< i n) (< j m) (= (a i) (b j))) (recur (inc i) (inc j) (conj! acc [:same (a i)]))
        (and (< i n) (or (= j m) (>= (at (inc i) j) (at i (inc j)))))
        (recur (inc i) j (conj! acc [:del (a i)]))
        (< j m) (recur i (inc j) (conj! acc [:ins (b j)]))
        :else (persistent! acc)))))

(defn- merge-runs
  "相鄰同類的 token 合併成一段。"
  [ops]
  (reduce (fn [acc [op s]]
            (let [[lop ls] (peek acc)]
              (if (= op lop)
                (conj (pop acc) [op (str ls s)])
                (conj acc [op s]))))
          [] ops))

(defn diff
  "回傳 [[op text] ...]，op 為 :same（不變）、:del（刪除）、:ins（新增）。
  先去掉共同的開頭與結尾再比較，一般的修改只需比較中間一小段。"
  [before after]
  (let [a (tokens before) b (tokens after)
        pre (common-prefix a b)
        a' (subvec a pre) b' (subvec b pre)
        suf (common-prefix (rseq a') (rseq b'))
        mid-a (subvec a' 0 (- (count a') suf))
        mid-b (subvec b' 0 (- (count b') suf))]
    (merge-runs
     (concat (map #(vector :same %) (subvec a 0 pre))
             (lcs-ops mid-a mid-b)
             (map #(vector :same %) (subvec a' (- (count a') suf)))))))

(defn changed? [ops]
  (boolean (some #(not= :same (first %)) ops)))
