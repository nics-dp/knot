(ns knot.sync
  "前端狀態與後端資料的同步：比較兩個狀態，只取出變動的實體。
  卡片、素材以 id、事實以 key、關係以 tree/relation-key 識別。"
  (:require [knot.tree :as tree]))

(def collections
  "同步的集合：狀態中的位置與識別方式。"
  {:nodes {:path [:nodes] :id :node/id}
   :relations {:path [:relations] :id tree/relation-key}
   :blocks {:path [:blocks] :id :block/id}
   :facts {:path [:facts] :id :fact/key}})

(defn- index-by [f coll]
  (into {} (map (juxt f identity)) coll))

(defn- as-coll [x]
  (if (map? x) (vals x) x))

(defn diff
  "回傳 {集合 {:upsert [實體...] :delete [識別鍵...]}}，只含有變動的集合；沒有變動時為 nil。"
  [old new]
  (not-empty
   (into {}
         (for [[k {:keys [path id]}] collections
               :let [a (index-by id (as-coll (get-in old path)))
                     b (index-by id (as-coll (get-in new path)))
                     upsert (vec (for [[i e] b :when (not= e (get a i))] e))
                     delete (vec (for [i (keys a) :when (not (contains? b i))] i))]
               :when (or (seq upsert) (seq delete))]
           [k (cond-> {}
                (seq upsert) (assoc :upsert upsert)
                (seq delete) (assoc :delete delete))]))))

(defn data
  "狀態中需要同步的部分。"
  [state]
  (select-keys state (map (comp first :path) (vals collections))))
