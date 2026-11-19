(ns knot.sources
  "素材與事實的純函式（SPEC 5.2、5.5）：正規化、事實的值是否出現在素材中、素材搜尋、事實 key 格式。
  素材是前端形式的 map：節錄 {:block/kind :para :block/text ...}，
  表格 {:block/kind :table :table/columns [欄名] :table/rows [[儲存格]]}。"
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; 正規化

(defn nfkc
  "NFKC 正規化（全形英數與標點轉半形、相容字元合併），不處理空白。"
  [s]
  #?(:clj (java.text.Normalizer/normalize (str s) java.text.Normalizer$Form/NFKC)
     :cljs (.normalize (str s) "NFKC")))

(defn- strip-thousands
  "去掉數字間的千分位逗號（逗號前是數字、後面剛好三位數字）：「1,210」→「1210」。"
  [s]
  (str/replace s #"(?<=\d),(?=\d{3}(?!\d))" ""))

(defn normalize
  "比對用的正規化：NFKC 後去掉所有空白（含全形空白與換行），並忽略數字的千分位逗號：
  「2,871」與「2871」視為相同（2026-10-29 使用者決定）。"
  [s]
  (strip-thousands (str/replace (nfkc s) #"\s+" "")))

;; ---------------------------------------------------------------------------
;; 事實 key

(def fact-key-pattern #"^[a-z0-9]+([.-][a-z0-9]+)*$")

(defn fact-key-valid?
  "事實 key 由英文小寫字母、數字、「.」、「-」組成，不能以符號開頭或結尾、不能連續兩個符號。"
  [k]
  (and (string? k) (boolean (re-matches fact-key-pattern k))))

;; ---------------------------------------------------------------------------
;; 素材內容

(defn table? [block] (= :table (:block/kind block)))

(defn discarded? [block] (= :discarded (:block/status block)))

(defn block-text
  "素材的全部文字：節錄為原文；表格為欄名與所有儲存格（以換行分隔）。"
  [block]
  (if (table? block)
    (str/join "\n" (concat (:table/columns block) (mapcat identity (:table/rows block))))
    (or (:block/text block) "")))

(defn- clip [s n]
  (if (> (count s) n) (str (subs s 0 n) "…") s))

;; ---------------------------------------------------------------------------
;; 事實的值是否出現在素材中

(defn- escape-re [s]
  (str/replace s #"[\\^$.|?*+()\[\]{}]" #(str "\\" %)))

(defn- squash-spaces
  "去掉空白，但兩個數字之間的空白縮成一格保留（避免「12 34」被當成「1234」）。"
  [s]
  (-> (str/replace s #"\s+" " ")
      (str/replace #"(?<!\d) | (?!\d)" "")))

(defn- contains-value?
  "text 是否含 value（比對前經 normalize）。值以數字開頭或結尾時，該端不能接在其他數字上：
  「12」不算出現在「1210」「3.12」「1,210」中；千分位逗號兩邊都先去掉。這時原文與值都只去掉
  不在兩個數字之間的空白（「2026 年 6 月 1 日」與「2026年6月1日」相同），避免以空白分隔的兩個數字被接在一起。"
  [text value]
  (let [v (normalize value)]
    (if (re-find #"^\d|\d$" v)
      (let [v (strip-thousands (squash-spaces (nfkc value)))]
        (boolean (re-find (re-pattern (str (when (re-find #"^\d" v) "(?<!\\d)(?<!\\d[.,])")
                                           (escape-re v)
                                           (when (re-find #"\d$" v) "(?!\\d)(?!\\.\\d)(?!,\\d{3}(?!\\d))")))
                          (strip-thousands (squash-spaces (nfkc text))))))
      (str/includes? (normalize text) v))))

(defn- table-contains-value?
  "表格的欄名或任一儲存格含 value（逐格比對，避免相鄰儲存格的數字接在一起）。"
  [block value]
  (boolean (some #(contains-value? % value)
                 (concat (:table/columns block) (mapcat identity (:table/rows block))))))

(defn fact-value-error
  "事實的值（:fact/value）是否出現在素材（block）的原文中；符合時為 nil，否則回傳給 agent 看的原因。
  比對時兩邊都經 normalize（NFKC、忽略空白與數字的千分位）。值與單位分開記錄時只檢查值，不看 :fact/unit。
  節錄：整段原文；表格：沒有 :fact/row、:fact/column 時為整張表（欄名與所有儲存格），
  有的話只看指定的儲存格（row 為 0 起算的列索引，column 為欄名）。"
  [block {:fact/keys [value row column]}]
  (let [title (:block/title block)
        value (some-> value str)
        has-row? (some? row)
        has-col? (some? column)]
    (cond
      (nil? block) "找不到事實所屬的素材"
      (str/blank? value) "事實的值不能是空白"
      (not= has-row? has-col?) "row 與 column 必須同時給（指定表格的某個儲存格）"
      (and has-row? (not (table? block)))
      (str "素材「" title "」是節錄，沒有列與欄；row、column 只用於表格素材")
      (table? block)
      (let [columns (:table/columns block)
            rows (:table/rows block)]
        (if-not has-row?
          (when-not (table-contains-value? block value)
            (str "值「" value "」沒有出現在表格素材「" title "」的任何儲存格或欄名中"
                 "；事實的值必須照抄素材裡的內容（可用 get_source_block 查看整張表）"))
          (let [ci (or (first (keep-indexed #(when (= %2 column) %1) columns)) -1)]
            (cond
              (not (and (integer? row) (<= 0 row) (< row (count rows))))
              (str "row " row " 超出範圍：表格素材「" title "」有 " (count rows) " 列（row 從 0 起算）")
              (neg? ci)
              (str "表格素材「" title "」沒有欄「" column "」；欄名有：" (str/join "、" columns))
              :else
              (let [cell (str (get-in rows [row ci]))]
                (when-not (contains-value? cell value)
                  (str "表格素材「" title "」row " row "、欄「" column "」的儲存格內容是「" cell
                       "」，沒有出現值「" value "」；請改用正確的 row 與 column，或不指定儲存格（比對整張表）")))))))
      :else
      (let [text (:block/text block)]
        (when-not (contains-value? text value)
          (str "值「" value "」沒有出現在素材「" title "」的原文中（原文：" (clip text 80) "）"
               "；事實的值必須照抄素材裡的內容，自行計算的數字不是事實，直接寫在本文即可"))))))

;; ---------------------------------------------------------------------------
;; 事實的顯示

(defn- group-thousands [digits]
  (str/replace digits #"\B(?=(\d{3})+(?!\d))" ","))

(defn format-fact-value
  "事實值的顯示：不含逗號的數字且整數部分 4 位以上時加千分位（3760 → 3,760、1234.5 → 1,234.5）；
  其他寫法（已有千分位、非數字文字）照原樣。"
  [value]
  (let [v (str value)]
    (if-let [[_ sign int-part frac] (re-matches #"([-−]?)([1-9]\d{3,})(\.\d+)?" v)]
      (str sign (group-thousands int-part) frac)
      v)))

(defn fact-display
  "事實的顯示文字（只用於顯示與輸出，儲存的值不變）：值經 format-fact-value；
  單位以中日韓文字開頭時與數字間空一格（「20 站」），符號單位（%、‰、℃、°）緊接；沒有單位只顯示值。"
  [{:fact/keys [value unit]}]
  (let [v (format-fact-value value)
        u (some-> unit str str/trim not-empty)]
    (cond
      (nil? u) v
      (re-find #"^[\u2e80-\u9fff\uf900-\ufaff\u3040-\u30ff\uac00-\ud7af]" u) (str v " " u)
      :else (str v u))))

;; ---------------------------------------------------------------------------
;; 搜尋

(defn- tokens
  "查詢字串以空白（含全形空白）分詞；詞經 NFKC、轉小寫與去空白。"
  [query]
  (->> (str/split (str/lower-case (nfkc (or query ""))) #"\s+")
       (remove str/blank?)
       vec))

(defn- field [s] (normalize (str/lower-case (nfkc (or s "")))))

(defn- fields
  "素材可搜尋的欄位：名稱、原文（表格為欄名與儲存格）、來源檔名；回傳 [名稱 內文 來源]。"
  [block]
  [(field (:block/title block)) (field (block-text block)) (field (:block/source block))])

(defn search
  "素材搜尋（SPEC 5.2）：query 以空白分詞，名稱、原文、儲存格、來源檔名（正規化後、不分大小寫）
  同時包含所有詞才符合；空字串列出全部。
  opts：:kind（:para 或 :table）、:source（來源檔名，完全相同）、:include-discarded?（預設排除已捨棄）。
  排序：相關度高者在前（詞出現在名稱加 3 分、來源檔名加 1 分、原文加 1 分，各詞加總），
  同分維持 blocks 原本的順序（來源、位置、名稱）。"
  [blocks query opts]
  (let [{:keys [kind source include-discarded?]} opts
        ts (mapv normalize (tokens query))]
    (->> blocks
         (filter #(or include-discarded? (not (discarded? %))))
         (filter #(or (nil? kind) (= kind (:block/kind %))))
         (filter #(or (nil? source) (= source (:block/source %))))
         (keep (fn [b]
                 (let [[title text src :as fs] (fields b)
                       hay (str/join "\u0001" fs)]
                   (when (every? #(str/includes? hay %) ts)
                     [(reduce + (for [t ts]
                                  (+ (if (str/includes? title t) 3 0)
                                     (if (str/includes? src t) 1 0)
                                     (if (str/includes? text t) 1 0))))
                      b]))))
         (sort-by (comp - first))
         (mapv second))))

(defn source-counts
  "來源檔名 → 素材數（不含已捨棄），依檔名排序；回傳 [{:source :count}]。"
  [blocks]
  (->> blocks
       (remove discarded?)
       (group-by :block/source)
       (sort-by key)
       (mapv (fn [[s bs]] {:source s :count (count bs)}))))
