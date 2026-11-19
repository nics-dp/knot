(ns knot.context-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [knot.context :as context]
            [knot.ui :as ui]))

(def state
  (ui/init-state
   {:style {:style/key "report" :style/text "使用「本計畫」，避免口語。\n人稱一律第三人稱。"}
    :tree [{:node/id "A" :node/kind :heading :node/name "甲" :node/draft "章的意圖"
            :children [{:node/id "iA" :node/kind :intro :node/draft "引言意圖"}
                       {:node/id "A1" :node/kind :heading :node/name "甲一" :node/draft "節的意圖"
                        :children [{:node/id "a1" :node/kind :paragraph :node/title "一" :node/draft "前段意圖"
                                    :node/prose {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "很長的本文"}]}]}}
                                   {:node/id "a2" :node/kind :paragraph :node/title "二" :node/draft "本段意圖"
                                    :node/citations [{:block "b1"}]
                                    :children [{:node/id "t1" :node/kind :table :node/title "表一" :node/label "tab:x" :node/draft "表的意圖"}]}
                                   {:node/id "a3" :node/kind :paragraph :node/title "三" :node/draft "後段意圖"}]}]}]
    :relations [{:relation/from "a1" :relation/to "a2" :relation/type :causal}
                {:relation/from "a2" :relation/to "a3" :relation/type :progression :relation/pending true}]
    :blocks [{:block/id "b1" :block/kind :para :block/status :discarded :block/title "素材" :block/text "素材內容"}]
    :facts [{:fact/key "k" :fact/label "事實" :fact/value "1" :fact/unit "%" :fact/block "b1"}
            {:fact/key "other" :fact/label "無關的事實" :fact/value "2" :fact/unit "" :fact/block "b9"}]}))

(defn- section [ctx title]
  (some #(when (= title (:title %)) %) (:sections ctx)))

(deftest paragraph-context-test
  (let [ctx (context/generation-context state "a2")]
    (testing "章節路徑帶各層標題的意圖"
      (is (= [["第一章 甲" "章的意圖"] ["1.1 甲一" "節的意圖"]]
             (mapv (juxt :label :text) (:entries (section ctx "章節路徑"))))))
    (testing "所屬標題沒有引言時不列出"
      (is (nil? (section ctx "所屬標題的引言"))))
    (testing "前一段有本文時帶本文與關係類型；下一段只帶意圖"
      (is (= {:label "一" :text "很長的本文" :source :prose :relation "因果"}
             (select-keys (first (:entries (section ctx "前一段"))) [:label :text :source :relation :has-prose])))
      (is (= {:text "後段意圖" :source :intent :relation "遞進，待確認"}
             (select-keys (first (:entries (section ctx "下一段"))) [:text :source :relation :has-prose]))
          "沒有本文時沒有 :has-prose"))
    (testing "只帶引用素材上的事實"
      (is (= ["k"] (mapv :key (:entries (section ctx "可用事實"))))))
    (testing "每一節都有分組"
      (is (every? :group (:sections ctx))))
    (testing "依據標示已捨棄的素材"
      (is (:warn (first (:entries (section ctx "依據"))))))
    (testing "下層表格帶 label 與意圖"
      (is (= "表的意圖" (:text (first (:entries (section ctx "下層表格與圖")))))))
    (is (pos? (:chars ctx)))))

(defn- doc [text]
  {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text text}]}]})

(deftest intro-context-test
  (let [ctx (context/generation-context state "iA")]
    (is (= ["引言意圖"] (mapv :text (:entries (section ctx "引言的意圖")))))
    (testing "下層是標題、沒有引言：帶底下所有段落，有本文帶本文，否則意圖，label 標明所屬標題"
      (is (= [["1.1 甲一／一" "很長的本文" :prose]
              ["1.1 甲一／二" "本段意圖" :intent]
              ["1.1 甲一／三" "後段意圖" :intent]]
             (mapv (juxt :label :text :source) (:entries (section ctx "這一層的下層內容")))))))
  (testing "下層標題的引言有本文時只帶引言本文"
    (let [s (ui/init-state
             {:tree [{:node/id "A" :node/kind :heading :node/name "甲"
                      :children [{:node/id "iA" :node/kind :intro}
                                 {:node/id "A1" :node/kind :heading :node/name "甲一"
                                  :children [{:node/id "iA1" :node/kind :intro :node/prose (doc "節引言本文")}
                                             {:node/id "a1" :node/kind :paragraph :node/title "一" :node/prose (doc "不該出現")}]}]}]})
          es (:entries (section (context/generation-context s "iA") "這一層的下層內容"))]
      (is (= [["1.1 甲一 引言" "節引言本文" :prose]] (mapv (juxt :label :text :source) es))))))

(deftest intro-children-limit-test
  (let [big (apply str (repeat 6001 "字"))
        s (assoc-in state [:nodes "a2" :node/prose] (doc big))
        es (:entries (section (context/generation-context s "iA") "這一層的下層內容"))]
    (testing "超過上限：標題底下的段落改帶意圖並標已有本文，另加提示"
      (is (every? #(= :intent (:source %)) (butlast es)))
      (is (= [true true nil] (mapv :has-prose (butlast es))) "a1、a2 有本文")
      (is (:prose-too-long (first es)))
      (is (= {:label "提示" :source :note} (select-keys (last es) [:label :source])))
      (is (re-find #"get_cards" (:text (last es)))))
    (testing "直接下層的段落不受影響"
      (let [s2 (ui/init-state
                {:tree [{:node/id "A" :node/kind :heading :node/name "甲"
                         :children [{:node/id "iA" :node/kind :intro}
                                    {:node/id "p0" :node/kind :paragraph :node/title "直屬" :node/prose (doc big)}
                                    {:node/id "A1" :node/kind :heading :node/name "甲一"
                                     :children [{:node/id "a1" :node/kind :paragraph :node/title "一" :node/prose (doc big)}]}]}]})
            es2 (:entries (section (context/generation-context s2 "iA") "這一層的下層內容"))]
        (is (= :prose (:source (first es2))))
        (is (= :intent (:source (second es2))))))))

(deftest ref-display-test
  (let [ref-doc {:type "doc" :content [{:type "paragraph"
                                        :content [{:type "text" :text "見"}
                                                  {:type "ref" :attrs {:label "tab:x"}}
                                                  {:type "ref" :attrs {:label "fig:y"}}
                                                  {:type "ref" :attrs {:label "tab:none"}}]}]}
        s (assoc-in state [:nodes "a1" :node/prose] ref-doc)
        s (assoc-in s [:nodes "t1" :node/kind] :table)
        s (update s :nodes assoc "f1" {:node/id "f1" :node/kind :figure :node/title "趨勢圖" :node/label "fig:y"})
        e (first (:entries (section (context/generation-context s "a2") "前一段")))]
    (is (= "見表「表一」（tab:x）圖「趨勢圖」（fig:y）{tab:none}" (:text e)))
    (testing "context/prose-text 與 prose/doc->text 維持原樣"
      (is (= "見{tab:x}{fig:y}{tab:none}" (context/prose-text {} ref-doc))))))

(deftest cited-table-rows-test
  (let [tbl {:block/id "bt" :block/kind :table :block/title "指標表" :table/columns ["指標" "值"]
             :table/rows [["人次" "2871"] ["達成率" "95.7%"]]}
        mk (fn [t where]
             (-> state
                 (update :blocks (fnil conj []) t)
                 (assoc-in [:nodes "a2" :node/citations] [{:block "bt" :where where}])))
        entry (fn [s] (first (:entries (section (context/generation-context s "a2") "依據"))))]
    (testing "合計不超過上限：欄位與全部列，where 照附"
      (is (= "指標｜值\n人次｜2871\n達成率｜95.7%\n（第一列）" (:text (entry (mk tbl "第一列"))))))
    (testing "超過上限：只帶欄位與列數，提示 get_source_block"
      (let [big (assoc tbl :table/rows [[(apply str (repeat 3001 "字")) "1"]])
            t (:text (entry (mk big "第一列")))]
        (is (str/starts-with? t "欄位：指標、值；1 列（第一列）"))
        (is (re-find #"get_source_block" t))))))

(deftest mixed-structure-context-test
  (let [s (ui/add-child state "A" :paragraph)           ; 接在引言後面、下層標題 1.1 之前
        pid (get-in s [:ui :selected])
        s (update-in s [:nodes pid] assoc :node/title "章的段落" :node/draft "章段意圖")
        ctx (context/generation-context s pid)]
    (testing "段落的後一段依全文順序：後面是下層標題時取下層標題的第一段（跨標題）"
      (is (= ["一"] (mapv :label (:entries (section ctx "下一段")))))
      (is (nil? (section ctx "前一段"))))
    (testing "下層標題的第一段，前一段是上層標題的段落（跨標題），前一段的本文照常帶"
      (let [e (first (:entries (section (context/generation-context s "a1") "前一段")))]
        (is (= "章的段落" (:label e)))))
    (testing "引言的下層內容包含這一層直接的段落與下層標題"
      (let [ictx (context/generation-context s "iA")]
        (is (= [["章的段落" "章段意圖"] ["1.1 甲一／一" "很長的本文"] ["1.1 甲一／二" "本段意圖"] ["1.1 甲一／三" "後段意圖"]]
               (mapv (juxt :label :text) (:entries (section ictx "這一層的下層內容")))))))))

(deftest parent-intro-test
  (let [s (assoc-in state [:nodes "iA" :node/prose]
                    {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "引言本文"}]}]})
        s (update s :nodes assoc "a0" {:node/id "a0" :node/kind :paragraph :node/parent "A"
                                       :node/order-key "z" :node/title "零" :node/draft "x"})
        ctx (context/generation-context s "a0")]
    (is (= [{:label "引言" :text "引言意圖" :source :intent :has-prose true}]
           (:entries (section ctx "所屬標題的引言")))
        "所屬標題的引言只帶意圖，已有本文時標示")))

(deftest other-kinds-test
  (is (nil? (context/generation-context state "A")))
  (is (nil? (context/generation-context state "t1"))))

(deftest remote-relation-context-test
  (let [s (-> state
              (ui/add-remote-relation "a3" "a1" :basis)
              (ui/add-remote-relation "a3" "a1" :echo))
        ctx (context/generation-context s "a3")]
    (is (= [{:label "一" :text "很長的本文" :source :prose}]
           (:entries (section ctx "本段依據的段落")))
        "依據的目標有本文時帶本文")
    (is (= [["一" "前段意圖"]] (mapv (juxt :label :text) (:entries (section ctx "本段呼應的段落")))))
    (is (nil? (section ctx "本段對比的段落")) "沒有的類型不列出")))

(defn- prose-of [text]
  {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text text}]}]})

(deftest prev-and-basis-prose-test
  (testing "前一段不論關係類型都帶本文；沒有關係或待確認也一樣"
    (doseq [rel [{:relation/type :contrast} {:relation/type :example}
                 {:relation/type :causal :relation/pending true} nil]]
      (let [s (assoc state :relations (when rel [(merge {:relation/from "a1" :relation/to "a2"} rel)]))
            e (first (:entries (section (context/generation-context s "a2") "前一段")))]
        (is (= [:prose "很長的本文"] [(:source e) (:text e)])))))
  (testing "前一段沒有本文時維持意圖與 has-prose 標記（無本文時沒有標記）"
    (let [e (first (:entries (section (context/generation-context state "a3") "前一段")))]
      (is (= {:source :intent :text "本段意圖"} (select-keys e [:source :text])))
      (is (not (:has-prose e)))))
  (testing "摘要、呼應、對比的目標與摘要本段的段落仍只帶意圖"
    (let [s (-> state
                (ui/add-remote-relation "a3" "a1" :summarizes)
                (ui/add-remote-relation "a3" "a1" :echo)
                (ui/add-remote-relation "a3" "a1" :contrast))
          ctx (context/generation-context s "a3")]
      (doseq [t ["本段摘要的段落" "本段呼應的段落" "本段對比的段落"]]
        (is (= [:intent true] ((juxt :source :has-prose) (first (:entries (section ctx t))))) t))
      (is (= [:intent "後段意圖"] ((juxt :source :text)
                                  (first (:entries (section (context/generation-context s "a1") "摘要本段的段落"))))))))
  (testing "前一段與依據目標合計超過 2000 字：全部改帶意圖並標示過長"
    (let [long (apply str (repeat 1500 "字"))
          s (-> state
                (assoc-in [:nodes "a1" :node/prose] (prose-of long))
                (ui/add-remote-relation "a3" "a2" :basis)
                (assoc-in [:nodes "a2" :node/prose] (prose-of long)))
          ctx (context/generation-context s "a3")
          es (concat (:entries (section ctx "前一段")) (:entries (section ctx "本段依據的段落")))]
      (is (= 2 (count es)))
      (is (every? #(= [:intent true true] ((juxt :source :has-prose :prose-too-long) %)) es))
      (is (not-any? #(= long (:text %)) es))))
  (testing "未超過時前一段與依據目標都帶本文"
    (let [s (-> state
                (ui/add-remote-relation "a3" "a1" :basis)
                (assoc-in [:nodes "a2" :node/prose] (prose-of "二的本文")))
          ctx (context/generation-context s "a3")]
      (is (= [:prose "二的本文"] ((juxt :source :text) (first (:entries (section ctx "前一段"))))))
      (is (= [:prose "很長的本文"] ((juxt :source :text) (first (:entries (section ctx "本段依據的段落")))))))))

(deftest prose-text-test
  (is (= "保留 31,405 筆，見{tab:x}。\n第二段"
         (context/prose-text {"k" {:fact/value "31,405" :fact/unit ""}}
                             {:type "doc"
                              :content [{:type "paragraph"
                                         :content [{:type "text" :text "保留 "}
                                                   {:type "fact" :attrs {:key "k"}}
                                                   {:type "text" :text " 筆，見"}
                                                   {:type "ref" :attrs {:label "tab:x"}}
                                                   {:type "text" :text "。"}]}
                                        {:type "paragraph" :content [{:type "text" :text "第二段"}]}]}))))

(deftest prose-text-lists-test
  (is (= "如下：\n- 甲\n- 乙\n1. 一\n2. 二"
         (context/prose-text {} {:type "doc"
                                 :content [{:type "paragraph" :content [{:type "text" :text "如下："}]}
                                           {:type "bulletList"
                                            :content (mapv (fn [t] {:type "listItem" :content [{:type "paragraph" :content [{:type "text" :text t}]}]}) ["甲" "乙"])}
                                           {:type "orderedList"
                                            :content (mapv (fn [t] {:type "listItem" :content [{:type "paragraph" :content [{:type "text" :text t}]}]}) ["一" "二"])}]}))))

(deftest table-content-in-context-test
  (let [s (assoc-in state [:nodes "t1" :node/table-content] {:columns ["指標" "數值"] :rows [["人次" "2871"] ["達成率" "95.7%"]]})
        e (first (:entries (section (context/generation-context s "a2") "下層表格與圖")))]
    (testing "label 在項目名稱，意圖之後接欄名與各列（每列一行、以「｜」分隔）"
      (is (= "tab:x 表一" (:label e)))
      (is (= "表的意圖\n指標｜數值\n人次｜2871\n達成率｜95.7%" (:text e)))
      (is (= :prose (:source e))))))

(deftest target-chars-context-test
  (let [st (-> state
               (ui/set-target-chars "a2" "400")
               (ui/set-target-chars "A" "2000"))
        ctx (context/generation-context st "a2")
        entries (fn [title] (:entries (section ctx title)))]
    (testing "本段帶目標字數 entry"
      (let [e (some #(when (= "目標字數" (:label %)) %) (entries "本段"))]
        (is (= 400 (:target-chars e)))
        (is (= :target (:source e)))
        (is (= "約 400 字（撰寫時控制篇幅）" (:text e)))))
    (testing "上層標題的目標一併帶上"
      (is (= [2000] (keep :target-chars (entries "章節路徑")))))
    (testing "沒設定時沒有"
      (is (not-any? #(= "目標字數" (:label %))
                    (mapcat :entries (:sections (context/generation-context state "a2"))))))))

(deftest style-entry-test
  (testing "有風格：一個 entry，text 是給 agent 的提示（字數、請依 get_style），不含全文；summary 供畫面顯示"
    (doseq [id ["a2" "iA"]
            :let [e (first (:entries (section (context/generation-context state id) "風格")))]]
      (is (= :style (:source e)))
      (is (= 23 (:chars e)))
      (is (str/includes? (:text e) "共 23 字"))
      (is (str/includes? (:text e) "get_style"))
      (is (not (str/includes? (:text e) "本計畫")))
      (is (= "使用「本計畫」，避免口語。 人稱一律第三人稱。" (:summary e)))))
  (testing "風格很長時摘要只取開頭約 60 字"
    (let [s (ui/set-style-text state (apply str (repeat 100 "字")))
          e (first (:entries (section (context/generation-context s "a2") "風格")))]
      (is (= (str (apply str (repeat 60 "字")) "…") (:summary e)))))
  (testing "提示帶最後修改的時間與修改者"
    (let [s (assoc state :style-info {:updated-by :agent :updated-at #?(:clj (java.util.Date. 0) :cljs (js/Date. 0))})
          e (first (:entries (section (context/generation-context s "a2") "風格")))]
      (is (str/includes? (:text e) "最後修改：agent"))))
  (testing "沒有風格"
    (let [s (dissoc state :style)
          e (first (:entries (section (context/generation-context s "a2") "風格")))]
      (is (= "尚未設定風格" (:text e)))
      (is (nil? (:summary e)))
      (is (zero? (:chars e)))))
  (testing "空白的風格視同沒有"
    (is (= "尚未設定風格" (:text (first (:entries (section (context/generation-context (ui/set-style-text state "  ") "a2") "風格"))))))))

(deftest cross-heading-prev-prose-test
  (let [s (-> (ui/add-child state "A" :paragraph)
              (as-> s (let [pid (get-in s [:ui :selected])]
                        (update-in s [:nodes pid] assoc :node/title "章的段落" :node/prose (doc "章段本文")))))
        e (first (:entries (section (context/generation-context s "a1") "前一段")))]
    (testing "前一段在上層標題底下（跨標題）：本文照常帶"
      (is (= {:label "章的段落" :text "章段本文" :source :prose}
             (select-keys e [:label :text :source]))))))
