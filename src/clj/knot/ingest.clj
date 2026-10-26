(ns knot.ingest
  "把 Markdown 切成素材區塊（SPEC 5.2）。標題成為區塊的分組與標題來源；
  每個段落、每個 GFM 表格各成一個區塊；頁碼以 <!-- page: N --> 標記。
  解析結果一律是候選區塊。"
  (:require [clojure.string :as str])
  (:import (org.commonmark.ext.gfm.tables TableBlock TableCell TableHead TablesExtension)
           (org.commonmark.node Code HardLineBreak Heading HtmlBlock Node Paragraph SoftLineBreak Text)
           (org.commonmark.parser Parser)))

(def ^:private parser
  (-> (Parser/builder)
      (.extensions [(TablesExtension/create)])
      (.build)))

(defn- children [^Node n]
  (take-while some? (iterate #(.getNext ^Node %) (.getFirstChild n))))

(defn- inline-text [^Node n]
  (cond
    (instance? Text n) (.getLiteral ^Text n)
    (instance? Code n) (.getLiteral ^Code n)
    (or (instance? SoftLineBreak n) (instance? HardLineBreak n)) " "
    :else (apply str (map inline-text (children n)))))

(defn- node-text [^Node n]
  (-> (inline-text n)
      (str/replace #"\s+" " ")
      str/trim))

(defn- table-rows
  "回傳 [columns rows]。"
  [^TableBlock t]
  (let [sections (children t)
        row-cells (fn [row] (mapv node-text (filter #(instance? TableCell %) (children row))))
        head (some #(when (instance? TableHead %) %) sections)
        body-rows (mapcat children (remove #(instance? TableHead %) sections))]
    [(if head (row-cells (first (children head))) [])
     (mapv row-cells body-rows)]))

(def ^:private page-marker #"<!--\s*page:\s*(\d+)\s*-->")

(def min-paragraph-chars
  "比這更短的段落視為頁首、頁碼等雜訊，不成為區塊。"
  8)

(defn markdown->blocks
  "回傳候選區塊的 vector；id-prefix 用來產生區塊 id。"
  [md {:keys [source id-prefix]}]
  (let [doc (.parse ^Parser parser md)]
    (loop [[n & more] (children doc)
           heading nil
           page nil
           i 0
           acc []]
      (if-not n
        acc
        (let [base {:block/source source
                    :block/status :candidate
                    :block/location (cond-> []
                                      page (conj (str "p." page))
                                      heading (conj heading))}
              id (str id-prefix "-" i)
              finish (fn [block]
                       (-> block
                           (update :block/location #(when (seq %) (str/join " · " %)))
                           (assoc :block/id id)))]
          (cond
            (instance? Heading n)
            (recur more (node-text n) page i acc)

            (and (instance? HtmlBlock n)
                 (re-find page-marker (.getLiteral ^HtmlBlock n)))
            (recur more heading (parse-long (second (re-find page-marker (.getLiteral ^HtmlBlock n)))) i acc)

            (instance? TableBlock n)
            (let [[cols rows] (table-rows n)]
              (recur more heading page (inc i)
                     (conj acc (finish (assoc base
                                              :block/kind :table
                                              :block/title (or heading (str/join "、" (take 3 cols)))
                                              :table/columns cols
                                              :table/rows rows)))))

            (instance? Paragraph n)
            (let [text (node-text n)]
              (if (< (count text) min-paragraph-chars)
                (recur more heading page i acc)
                (recur more heading page (inc i)
                       (conj acc (finish (assoc base
                                                :block/kind :para
                                                :block/title (or heading (subs text 0 (min 20 (count text))))
                                                :block/text text))))))

            :else
            (recur more heading page i acc)))))))
