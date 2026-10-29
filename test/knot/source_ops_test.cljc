(ns knot.source-ops-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.prose :as prose]
            [knot.source-ops :as sops]
            [knot.tree :as tree]))

(defn- p [id & {:as m}] (merge {:node/id id :node/kind :paragraph :node/title id :node/status :none :node/version 1} m))

(def para-id (random-uuid))
(def table-id (random-uuid))
(def gone-id (random-uuid))

(defn- state []
  {:nodes (tree/from-nested
           [{:node/id "A" :node/kind :heading :node/name "甲"
             :children [{:node/id "iA" :node/kind :intro}
                        (p "a1")
                        (p "a2" :node/citations [{:block para-id}]
                           :node/prose (:doc (prose/markup->doc "節電率 {{fact:rate}}")))]}
            {:node/id "T" :node/kind :heading :node/name "乙" :children [{:node/id "f1" :node/kind :figure :node/title "圖"}]}])
   :blocks [{:block/id para-id :block/kind :para :block/status :confirmed :block/source "報告.pdf"
             :block/title "節電成效" :block/text "三處場域的平均節電率達 47%。"}
            {:block/id table-id :block/kind :table :block/status :confirmed :block/source "紀錄.xlsx"
             :block/title "量測結果" :table/kind :data
             :table/columns ["場域" "節電率"] :table/rows [["A" "47%"] ["B" "40%"]]}
            {:block/id gone-id :block/kind :para :block/status :discarded :block/source "報告.pdf"
             :block/title "舊資料" :block/text "已捨棄的內容 99"}]
   :facts {"rate" {:fact/key "rate" :fact/label "節電率" :fact/value "47" :fact/unit "%" :fact/block para-id}}
   :relations []})

(defn- block [s id] (some #(when (= id (:block/id %)) %) (:blocks s)))

(deftest add-excerpt-test
  (let [s (state)
        r (sops/add-excerpt s {:source " 契約.pdf " :title "計畫目標" :text "建置路燈。" :location "p.3" :note "  "})
        id (first (:blocks r))
        b (block (:state r) id)]
    (is (uuid? id))
    (is (= {:block/kind :para :block/status :confirmed :block/source "契約.pdf" :block/title "計畫目標"
            :block/text "建置路燈。" :block/location "p.3"}
           (dissoc b :block/id)) "沒有用途說明時不存欄位")
    (is (= :manual (:origin r)))
    (is (= "新增節錄「計畫目標」" (:label r)))
    (is (= 4 (count (:blocks (:state r)))))
    (testing "note 選填"
      (let [r2 (sops/add-excerpt s {:source "x" :title "t" :text "x" :note "用於第一章"})]
        (is (= "用於第一章" (:block/note (block (:state r2) (first (:blocks r2))))))))
    (testing "錯誤"
      (is (re-find #"source" (:error (sops/add-excerpt s {:title "t" :text "x"}))))
      (is (re-find #"title" (:error (sops/add-excerpt s {:source "x" :title " " :text "x"}))))
      (is (re-find #"text" (:error (sops/add-excerpt s {:source "x" :title "t"}))))
      (is (re-find #"location" (:error (sops/add-excerpt s {:source "x" :title "t" :text "x" :location 1}))))
      (is (nil? (:state (sops/add-excerpt s {:source "x" :title "t" :text "x" :location 1}))) "錯誤不回傳狀態"))
    (testing "同來源相同原文（正規化後）拒絕；已捨棄的不算重複；不同來源可以"
      (let [dup (sops/add-excerpt s {:source "報告.pdf" :title "另一個名字" :text "三處場域的平均節電率達４７％。"})]
        (is (re-find #"已有相同原文" (:error dup)))
        (is (re-find (re-pattern (str para-id)) (:error dup))))
      (is (:state (sops/add-excerpt s {:source "報告.pdf" :title "t" :text "已捨棄的內容 99"})))
      (is (:state (sops/add-excerpt s {:source "別份.pdf" :title "t" :text "三處場域的平均節電率達 47%。"}))))))

(deftest add-source-table-test
  (let [s (state)
        r (sops/add-source-table s {:source "紀錄.xlsx" :title "年用電" :columns ["場域" "kWh"]
                                    :rows [["A" 192920] ["B" "156000"]] :location "工作表 3"})
        b (block (:state r) (first (:blocks r)))]
    (is (= :table (:block/kind b)))
    (is (= [["A" "192920"] ["B" "156000"]] (:table/rows b)) "數字轉成字串")
    (is (= ["場域" "kWh"] (:table/columns b)))
    (is (= :confirmed (:block/status b)))
    (is (= "工作表 3" (:block/location b))))
  (testing "錯誤"
    (is (re-find #"columns" (:error (sops/add-source-table (state) {:source "x" :title "t" :columns [] :rows []}))))
    (is (re-find #"2 欄" (:error (sops/add-source-table (state) {:source "x" :title "t" :columns ["a" "b"] :rows [["1"]]}))))
    (is (re-find #"字串或數字" (:error (sops/add-source-table (state) {:source "x" :title "t" :columns ["a"] :rows [[nil]]}))))
    (is (re-find #"source" (:error (sops/add-source-table (state) {:title "t" :columns ["a"] :rows []}))))))

(deftest update-source-block-test
  (let [s (state)]
    (testing "改名稱、位置、用途說明；空字串清除"
      (let [r (sops/update-source-block s {:id para-id :title "新名稱" :location "p.9" :note "用途"})
            b (block (:state r) para-id)]
        (is (= ["新名稱" "p.9" "用途"] ((juxt :block/title :block/location :block/note) b)))
        (is (= [para-id] (:blocks r)))
        (is (empty? (:fact-problems r)))
        (let [b2 (block (:state (sops/update-source-block (:state r) {:id para-id :location "" :note ""})) para-id)]
          (is (not (contains? b2 :block/location)))
          (is (not (contains? b2 :block/note))))))
    (testing "改原文：值仍在則沒有問題"
      (let [r (sops/update-source-block s {:id para-id :text "平均節電率達 47%（修正錯字）。"})]
        (is (= [] (:fact-problems r)))
        (is (nil? (:message r)))))
    (testing "改原文使事實對不上：仍寫入，列出事實並提示用 update_fact"
      (let [r (sops/update-source-block s {:id para-id :text "平均節電率達 48%。"})]
        (is (= "平均節電率達 48%。" (:block/text (block (:state r) para-id))))
        (is (= ["rate"] (map :key (:fact-problems r))))
        (is (re-find #"update_fact" (:message r)))))
    (testing "改表格：欄與列要一起給；綁定儲存格的事實也會重新檢查"
      (let [s2 (assoc-in s [:facts "b-rate"] {:fact/key "b-rate" :fact/label "B" :fact/value "40" :fact/block table-id
                                              :fact/row 1 :fact/column "節電率"})
            r (sops/update-source-block s2 {:id table-id :columns ["場域" "節電率"] :rows [["A" "47%"]]})]
        (is (= [["A" "47%"]] (:table/rows (block (:state r) table-id))))
        (is (= ["b-rate"] (map :key (:fact-problems r))))
        (is (re-find #"超出範圍" (:error (first (:fact-problems r)))))
        (is (re-find #"一起給" (:error (sops/update-source-block s {:id table-id :rows [["A" "1"]]}))))))
    (testing "錯誤"
      (is (re-find #"已被捨棄" (:error (sops/update-source-block s {:id gone-id :title "x"}))))
      (is (re-find #"找不到素材" (:error (sops/update-source-block s {:id (random-uuid) :title "x"}))))
      (is (re-find #"沒有要修改" (:error (sops/update-source-block s {:id para-id}))))
      (is (re-find #"title" (:error (sops/update-source-block s {:id para-id :title " "}))))
      (is (re-find #"表格素材沒有 text" (:error (sops/update-source-block s {:id table-id :text "x"}))))
      (is (re-find #"節錄沒有" (:error (sops/update-source-block s {:id para-id :columns ["a"] :rows []})))))))

(deftest discard-source-block-test
  (let [s (state)
        r (sops/discard-source-block s {:id para-id})]
    (is (= :discarded (:block/status (block (:state r) para-id))))
    (is (= [(:node/id (some #(when (= "a2" (:node/title %)) %) (vals (:nodes s))))] (:citing r))
        "回傳仍引用它的段落")
    (is (re-find #"a2" (:message r)))
    (is (= "捨棄素材「節電成效」" (:label r)))
    (is (= ["a2"] (mapv :node/id (map (:nodes s) (:citing r)))))
    (is (= [{:block para-id}] (get-in (:state r) [:nodes "a2" :node/citations])) "依據保留")
    (is (empty? (:citing (sops/discard-source-block s {:id table-id})))))
  (is (re-find #"已經是捨棄" (:error (sops/discard-source-block (state) {:id gone-id}))))
  (is (re-find #"找不到素材" (:error (sops/discard-source-block (state) {:id (random-uuid)})))))

(deftest add-fact-test
  (let [s (state)]
    (testing "節錄上的事實"
      (let [r (sops/add-fact s {:key "rate.avg" :label "平均節電率" :value "47" :unit "%" :block-id para-id})]
        (is (= {:fact/key "rate.avg" :fact/label "平均節電率" :fact/value "47" :fact/unit "%" :fact/block para-id}
               (get-in r [:state :facts "rate.avg"])))
        (is (= ["rate.avg"] (:facts r)))
        (is (= "新增事實「平均節電率」" (:label r)))))
    (testing "數字值轉成字串；空白的單位不存"
      (let [r (sops/add-fact s {:key "n" :label "n" :value 47 :unit " " :block-id para-id})]
        (is (= {:fact/key "n" :fact/label "n" :fact/value "47" :fact/block para-id} (get-in r [:state :facts "n"])))))
    (testing "表格儲存格"
      (let [r (sops/add-fact s {:key "b" :label "B" :value "40" :unit "%" :block-id table-id :row 1 :column "節電率"})]
        (is (= [1 "節電率"] ((juxt :fact/row :fact/column) (get-in r [:state :facts "b"]))))
        (is (some? (:state (sops/add-fact s {:key "any" :label "x" :value "40" :block-id table-id}))) "不指定時比對整張表")))
    (testing "錯誤：key"
      (is (re-find #"需要 key" (:error (sops/add-fact s {:label "x" :value "47" :block-id para-id}))))
      (is (re-find #"格式不正確" (:error (sops/add-fact s {:key "Rate_1" :label "x" :value "47" :block-id para-id}))))
      (is (re-find #"已經存在" (:error (sops/add-fact s {:key "rate" :label "x" :value "47" :block-id para-id})))))
    (testing "錯誤：欄位"
      (is (re-find #"label" (:error (sops/add-fact s {:key "k" :value "47" :block-id para-id}))))
      (is (re-find #"value" (:error (sops/add-fact s {:key "k" :label "x" :block-id para-id}))))
      (is (re-find #"block_id" (:error (sops/add-fact s {:key "k" :label "x" :value "47"}))))
      (is (re-find #"找不到素材" (:error (sops/add-fact s {:key "k" :label "x" :value "47" :block-id (random-uuid)}))))
      (is (re-find #"已被捨棄" (:error (sops/add-fact s {:key "k" :label "x" :value "99" :block-id gone-id})))))
    (testing "錯誤：值不在素材中"
      (let [e (:error (sops/add-fact s {:key "k" :label "x" :value "48" :block-id para-id}))]
        (is (re-find #"沒有出現在素材" e)))
      (is (re-find #"40%" (:error (sops/add-fact s {:key "k" :label "x" :value "47" :block-id table-id :row 1 :column "節電率"}))))
      (is (re-find #"同時給" (:error (sops/add-fact s {:key "k" :label "x" :value "40" :block-id table-id :row 1}))))
      (is (re-find #"0 以上" (:error (sops/add-fact s {:key "k" :label "x" :value "40" :block-id table-id :row -1 :column "節電率"})))))))

(deftest update-fact-test
  (let [s (state)]
    (testing "改值、標籤；列出用到它的段落"
      (let [r (sops/update-fact s {:key "rate" :value "47" :label "平均節電率"})]
        (is (= "平均節電率" (get-in r [:state :facts "rate" :fact/label])))
        (is (= ["a2"] (mapv #(:node/id (get (:nodes s) %)) (:citing r))))
        (is (re-find #"a2" (:message r)))))
    (testing "單位可清除"
      (is (not (contains? (get-in (sops/update-fact s {:key "rate" :unit ""}) [:state :facts "rate"]) :fact/unit))))
    (testing "改指向表格儲存格，再改回不指定"
      (let [r (sops/update-fact s {:key "rate" :block-id table-id :row 0 :column "節電率"})
            f (get-in r [:state :facts "rate"])]
        (is (= [table-id 0 "節電率"] ((juxt :fact/block :fact/row :fact/column) f)))
        (let [f2 (get-in (sops/update-fact (:state r) {:key "rate" :row nil :column nil}) [:state :facts "rate"])]
          (is (not (contains? f2 :fact/row)))
          (is (not (contains? f2 :fact/column))))))
    (testing "錯誤"
      (is (re-find #"找不到事實" (:error (sops/update-fact s {:key "nope" :value "1"}))))
      (is (re-find #"沒有要修改" (:error (sops/update-fact s {:key "rate"}))))
      (is (re-find #"沒有出現" (:error (sops/update-fact s {:key "rate" :value "99"}))))
      (is (re-find #"已被捨棄" (:error (sops/update-fact s {:key "rate" :block-id gone-id}))))
      (is (re-find #"label" (:error (sops/update-fact s {:key "rate" :label " "}))))
      (is (re-find #"同時給" (:error (sops/update-fact s {:key "rate" :block-id table-id :row 1}))))
      (is (re-find #"節錄" (:error (sops/update-fact s {:key "rate" :row 0 :column "節電率"})))))
    (testing "素材原文改過後，事實可以改成新值"
      (let [s2 (:state (sops/update-source-block s {:id para-id :text "平均節電率達 48%。"}))]
        (is (some? (:state (sops/update-fact s2 {:key "rate" :value "48"}))))))
    (testing "素材已捨棄時不能改事實"
      (is (re-find #"已被捨棄" (:error (sops/update-fact (:state (sops/discard-source-block s {:id para-id}))
                                                        {:key "rate" :value "47"})))))))

(deftest citation-test
  (let [s (state)]
    (testing "加入依據（段落與引言）"
      (let [r (sops/add-citation s {:id "a1" :block-id table-id :where "欄：節電率"})]
        (is (= [{:block table-id :where "欄：節電率"}] (get-in r [:state :nodes "a1" :node/citations])))
        (is (= ["a1"] (:touched r))))
      (is (= [{:block table-id}] (get-in (sops/add-citation s {:id "iA" :block-id table-id}) [:state :nodes "iA" :node/citations]))))
    (testing "重複：不寫入，說明已經是依據"
      (let [r (sops/add-citation s {:id "a2" :block-id para-id})]
        (is (= s (:state r)))
        (is (re-find #"已經是" (:note r)))))
    (testing "錯誤"
      (is (re-find #"已被捨棄" (:error (sops/add-citation s {:id "a1" :block-id gone-id}))))
      (is (re-find #"找不到素材" (:error (sops/add-citation s {:id "a1" :block-id (random-uuid)}))))
      (is (re-find #"段落與引言" (:error (sops/add-citation s {:id "A" :block-id para-id}))))
      (is (re-find #"找不到卡片" (:error (sops/add-citation s {:id "zzz" :block-id para-id})))))
    (testing "移除依據：最後一筆移除後不留空陣列"
      (let [r (sops/remove-citation s {:id "a2" :block-id para-id})]
        (is (not (contains? (get-in r [:state :nodes "a2"]) :node/citations))))
      (is (re-find #"不是" (:error (sops/remove-citation s {:id "a1" :block-id para-id})))))
    (testing "已捨棄的素材仍可從依據移除"
      (let [s2 (:state (sops/discard-source-block s {:id para-id}))]
        (is (some? (:state (sops/remove-citation s2 {:id "a2" :block-id para-id}))))))))

(deftest add-facts-batch-test
  (let [s (state)
        f (fn [k & {:as m}] (merge {:key k :label (str "標籤" k) :value "47" :unit "%" :block-id para-id} m))]
    (testing "全部合法：依序新增，一次回傳"
      (let [r (sops/add-facts s {:facts [(f "a.1") (f "a.2" :value "47" :unit nil)
                                         (f "t.1" :block-id table-id :value "40%" :row 1 :column "節電率")]})]
        (is (nil? (:error r)))
        (is (= ["a.1" "a.2" "t.1"] (:facts r)))
        (is (= "新增 3 筆事實" (:label r)))
        (is (every? #(contains? (get-in r [:state :facts]) %) ["a.1" "a.2" "t.1"]))))
    (testing "有錯誤：整批不寫，列出每一筆"
      (let [r (sops/add-facts s {:facts [(f "ok.1") (f "bad.2" :value "99") (f "Bad Key") (f "rate") (f "no.block" :block-id (random-uuid))]})]
        (is (nil? (:state r)))
        (is (re-find #"整批沒有寫入，有 4 筆" (:error r)))
        (is (re-find #"facts\[1\]（bad\.2）：值「99」" (:error r)))
        (is (re-find #"facts\[2\]（Bad Key）：key" (:error r)))
        (is (re-find #"facts\[3\]（rate）：key「rate」已經存在" (:error r)))
        (is (re-find #"facts\[4\]（no\.block）：找不到素材" (:error r)))
        (is (not (re-find #"facts\[0\]" (:error r))))))
    (testing "同一批內 key 重複"
      (let [r (sops/add-facts s {:facts [(f "dup") (f "dup")]})]
        (is (re-find #"facts\[1\]（dup）：key「dup」已經存在" (:error r)))))
    (testing "上限與空陣列"
      (is (re-find #"最多 100 筆" (:error (sops/add-facts s {:facts (mapv #(f (str "k" %)) (range 101))}))))
      (is (nil? (:error (sops/add-facts s {:facts (mapv #(f (str "k" %)) (range 100))}))))
      (is (re-find #"不能是空陣列" (:error (sops/add-facts s {:facts []}))))
      (is (re-find #"需要 facts" (:error (sops/add-facts s {})))))))

(deftest add-excerpts-batch-test
  (let [s (state)
        e (fn [t & {:as m}] (merge {:source "新.pdf" :title (str "名" t) :text t} m))]
    (testing "全部合法"
      (let [r (sops/add-excerpts s {:excerpts [(e "甲") (e "乙")]})]
        (is (= 2 (count (:blocks r))))
        (is (= 5 (count (get-in r [:state :blocks]))))
        (is (= "新增 2 筆節錄" (:label r)))))
    (testing "錯誤與同批重複：整批不寫"
      (let [r (sops/add-excerpts s {:excerpts [(e "甲") (e "甲 ") {:source "新.pdf" :title "無原文"} (e "丙" :source "報告.pdf" :text "三處場域的平均節電率達 47%。")]})]
        (is (nil? (:state r)))
        (is (re-find #"有 3 筆" (:error r)))
        (is (re-find #"excerpts\[1\]（名甲）：同一來源已有相同原文" (:error r)))
        (is (re-find #"excerpts\[2\]（無原文）：需要 text" (:error r)))
        (is (re-find #"excerpts\[3\]（名丙）：同一來源已有相同原文" (:error r)))))
    (testing "上限"
      (is (re-find #"最多 50 筆" (:error (sops/add-excerpts s {:excerpts (mapv #(e (str "t" %)) (range 51))}))))
      (is (nil? (:error (sops/add-excerpts s {:excerpts (mapv #(e (str "t" %)) (range 50))})))))))
