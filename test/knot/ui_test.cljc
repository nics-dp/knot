(ns knot.ui-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.tree :as tree]
            [knot.ui :as ui]))

(def report
  {:report/title "測試報告"
   :ui/selected "a1"
   :tree [{:node/id "A" :node/kind :heading :node/name "甲"
           :children [{:node/id "a1" :node/kind :paragraph :node/title "一" :node/status :ai}
                      {:node/id "a2" :node/kind :paragraph :node/title "二" :node/status :human}]}
          {:node/id "B" :node/kind :heading :node/name "乙"
           :children [{:node/id "b1" :node/kind :paragraph :node/title "三" :node/status :none}]}]
   :relations [{:relation/from "a1" :relation/to "a2" :relation/type :causal}]})

(def state (ui/init-state report))

(deftest navigate-test
  (is (= "a2" (get-in (ui/navigate state :down) [:ui :selected])))
  (is (= "A" (get-in (ui/navigate state :left) [:ui :selected])))
  (is (= "a1" (get-in (ui/navigate state :up) [:ui :selected])) "沒有上一個兄弟時不動"))

(deftest drag-and-drop-test
  (let [s (-> state
              (ui/drag-start "a1")
              ;; 欄 0 的 B 卡片在 92–176
              (ui/drag-over 0 120))]
    (is (= {:id "B" :pos :into} (get-in s [:ui :over])))
    (let [s (ui/drop-dragged s)]
      (testing "移動後被移動的卡片成為選取狀態，並顯示訊息"
        (is (= "a1" (get-in s [:ui :selected])))
        (is (= ["b1" "a1"] (tree/child-ids (:nodes s) "B")))
        (is (re-find #"待確認" (get-in s [:ui :toast]))))
      (testing "拖曳狀態清除"
        (is (nil? (get-in s [:ui :drag]))))
      (testing "整個移動可以復原"
        (let [u (ui/undo s)]
          (is (= (:nodes state) (:nodes u)))
          (is (= (:relations state) (:relations u)))
          (is (empty? (:history u))))))))

(deftest illegal-drop-test
  (let [s (-> state (ui/drag-start "a1") (ui/drag-over 3 0))]
    (is (nil? (get-in s [:ui :over])))
    (is (= (:nodes state) (:nodes (ui/drop-dragged s))))))

(deftest move-selected-test
  (let [s (ui/move-selected (ui/select state "a2") -1)]
    (is (= ["a2" "a1"] (tree/child-ids (:nodes s) "A")))
    (is (:relation/pending (tree/relation-between (:relations s) "a2" "a1")))))

(deftest add-child-test
  (testing "新增引言放在最前面"
    (let [s (ui/add-child state "A" :intro)
          id (get-in s [:ui :selected])]
      (is (= :intro (tree/kind (:nodes s) id)))
      (is (= id (first (tree/child-ids (:nodes s) "A"))))
      (testing "引言最多一張"
        (is (= (:nodes s) (:nodes (ui/add-child s "A" :intro)))))))
  (testing "已有段落的標題可以直接新增下層標題，接在段落後面"
    (let [s (ui/add-child state "A" :heading)
          id (get-in s [:ui :selected])]
      (is (= ["a1" "a2" id] (tree/child-ids (:nodes s) "A")))
      (testing "再新增段落：接在最後一個段落之後、下層標題之前"
        (let [s2 (ui/add-child s "A" :paragraph)
              pid (get-in s2 [:ui :selected])]
          (is (= ["a1" "a2" pid id] (tree/child-ids (:nodes s2) "A")))))))
  (testing "把現有段落移到新的下層標題"
    (let [s (ui/wrap-paragraphs state "A")
          sub (get-in s [:ui :selected])]
      (is (= [sub] (tree/child-ids (:nodes s) "A")))
      (is (= ["a1" "a2"] (tree/child-ids (:nodes s) sub))))))

(deftest sibling-kind-mixed-test
  (let [s (ui/add-child state "A" :heading)
        s (ui/add-child s "A" :intro)
        nodes (:nodes s)]
    (is (= :heading (ui/sibling-kind nodes (last (tree/child-ids nodes "A")))))
    (is (= :paragraph (ui/sibling-kind nodes "a1")))
    (is (= :paragraph (ui/sibling-kind nodes (first (tree/child-ids nodes "A"))))
        "選取引言時新增段落（接在引言後面，段落一律在下層標題之前）")))

(deftest add-sibling-mixed-test
  (let [s (ui/add-child state "A" :heading)
        s (ui/add-child s "A" :intro)
        intro-id (first (tree/child-ids (:nodes s) "A"))
        s2 (ui/add-sibling s intro-id)
        nid (get-in s2 [:ui :selected])]
    (is (= :paragraph (tree/kind (:nodes s2) nid)))
    (is (= nid (second (tree/child-ids (:nodes s2) "A"))) "緊接在引言後面")))

(deftest mixed-drag-test
  (let [s (ui/add-child state "A" :heading)
        sub (get-in s [:ui :selected])
        s (ui/add-child s "A" :paragraph)
        pid (get-in s [:ui :selected])
        s (ui/add-child s "B" :paragraph)
        extra (get-in s [:ui :selected])]
    (testing "段落不能拖到下層標題之後"
      (is (nil? (tree/placement (:nodes s) "b1" sub :after)))
      (is (some? (tree/placement (:nodes s) "b1" sub :before))))
    (testing "往上下移動：段落與下層標題不互換"
      (is (= (:nodes s) (:nodes (ui/move-selected (ui/select s pid) 1))))
      (is (= (:nodes s) (:nodes (ui/move-selected (ui/select s sub) -1))))
      (is (= ["a1" "a2" pid sub] (tree/child-ids (:nodes s) "A"))))
    (testing "into 一張段落到有下層標題的標題：放在段落群的最後"
      (let [s2 (ui/apply-move s extra (tree/placement (:nodes s) extra "A" :into))
            kids (tree/child-ids (:nodes s2) "A")]
        (is (= sub (last kids)))
        (is (= extra (last (butlast kids))))))))

(deftest paragraph->intro-test
  (let [s (ui/paragraph->intro state "a1")]
    (is (= :intro (tree/kind (:nodes s) "a1")))
    (is (= ["a1" "a2"] (tree/child-ids (:nodes s) "A")))
    (is (empty? (:relations s)) "引言不再有段落關係")))

(deftest edit-intent-test
  (let [s (ui/set-node-field state "a1" :node/draft "新的意圖")]
    (is (= "新的意圖" (get-in s [:nodes "a1" :node/draft])))
    (is (empty? (:history s)) "逐字輸入不進復原紀錄"))
  (is (= state (ui/set-node-field state "a1" :node/status :human)) "只能編輯名稱與意圖"))

(deftest citation-test
  (let [s (ui/add-citation state "a1" "b9")]
    (is (= [{:block "b9"}] (get-in s [:nodes "a1" :node/citations])))
    (is (= s (ui/add-citation s "a1" "b9")) "不重複加入")
    (is (empty? (get-in (ui/remove-citation s "a1" "b9") [:nodes "a1" :node/citations]))))
  (is (= state (ui/add-citation state "A" "b9")) "只有段落有依據"))

(deftest set-relation-test
  (let [moved (ui/move-selected (ui/select state "a2") -1)
        s (ui/set-relation moved "a2" "a1" :contrast)
        rel (tree/relation-between (:relations s) "a2" "a1")]
    (is (= :contrast (:relation/type rel)))
    (is (false? (:relation/pending rel)) "設定類型即確認")
    (is (nil? (tree/relation-between (:relations (ui/set-relation s "a2" "a1" nil)) "a2" "a1"))
        "選「未標註」移除關係")))

(deftest pages-test
  (let [s (-> state (ui/set-page :library) (ui/open-paragraph "b1"))]
    (is (= :tree (get-in s [:ui :page])))
    (is (= "b1" (get-in s [:ui :selected])))))

(deftest add-sibling-test
  (let [s (ui/add-sibling state "a1")
        id (get-in s [:ui :selected])]
    (testing "新段落接在選取段落的後面，並準備編輯名稱"
      (is (= ["a1" id "a2"] (tree/child-ids (:nodes s) "A")))
      (is (= id (get-in s [:ui :focus-title]))))
    (testing "原本的下一段改為承接新段落，關係待確認"
      (is (nil? (tree/relation-between (:relations s) "a1" "a2")))
      (is (:relation/pending (tree/relation-between (:relations s) id "a2"))))
    (testing "可以復原"
      (is (= (:nodes state) (:nodes (ui/undo s))))))
  (let [s (ui/add-sibling state "A")]
    (is (= ["A" (get-in s [:ui :selected]) "B"] (tree/child-ids (:nodes s) nil)) "新章接在後面")))

(deftest sibling-kind-test
  (let [s (ui/add-child state "B" :intro)
        intro (get-in s [:ui :selected])]
    (is (= :paragraph (ui/sibling-kind (:nodes s) intro)) "選取引言時新增段落")
    (let [s (ui/add-sibling s intro)]
      (is (= [intro (get-in s [:ui :selected]) "b1"] (tree/child-ids (:nodes s) "B"))))))

(deftest delete-node-test
  (let [s (ui/delete-node state "A")]
    (testing "連同下層卡片一起刪除，相關關係移除"
      (is (= ["B" "b1"] (tree/document-order (:nodes s))))
      (is (empty? (:relations s))))
    (is (= "B" (get-in s [:ui :selected])))
    (is (= (:nodes state) (:nodes (ui/undo s))) "可以復原"))
  (let [s (ui/delete-node state "a1")]
    (is (= ["a2"] (tree/child-ids (:nodes s) "A")))
    (is (= "a2" (get-in s [:ui :selected])))))

(deftest scroll-column-test
  (is (= -30 (get-in (ui/scroll-column state 2 -30 -100 0) [:ui :bias 2])))
  (is (= -100 (get-in (ui/scroll-column state 2 -500 -100 0) [:ui :bias 2])) "不超出範圍")
  (is (= state (ui/edge-scroll state 2 -30 -100 0)) "沒有拖曳時邊緣不捲動"))

(deftest toggle-editor-test
  (is (true? (get-in state [:ui :editor-collapsed])) "編輯區預設收合")
  (is (false? (get-in (ui/toggle-editor state) [:ui :editor-collapsed])))
  (is (true? (get-in (ui/toggle-editor (ui/toggle-editor state)) [:ui :editor-collapsed]))))

(deftest remote-relation-test
  (let [s (ui/add-remote-relation state "b1" "a1" :basis)
        {:keys [outgoing incoming]} (tree/remote-relations (:relations s) "b1")]
    (testing "任意距離的段落都能建立關係"
      (is (= [{:relation/from "b1" :relation/to "a1" :relation/type :basis :relation/remote true}] outgoing))
      (is (empty? incoming))
      (is (= 1 (count (:incoming (tree/remote-relations (:relations s) "a1"))))))
    (testing "不重複、不能連到自己或非段落"
      (is (= (:relations s) (:relations (ui/add-remote-relation s "b1" "a1" :basis))))
      (is (= state (ui/add-remote-relation state "a1" "a1" :echo)))
      (is (= state (ui/add-remote-relation state "a1" "A" :echo))))
    (testing "不相鄰的對比不會被當成相鄰關係"
      (let [s (ui/add-remote-relation state "a1" "a2" :contrast)]
        (is (= :causal (:relation/type (tree/relation-between (:relations s) "a1" "a2"))))))
    (testing "移除"
      (is (empty? (:outgoing (tree/remote-relations (:relations (ui/remove-remote-relation s "b1" "a1" :basis)) "b1")))))
    (testing "移動段落時不影響不相鄰的關係"
      (let [moved (ui/move-selected (ui/select s "a2") -1)]
        (is (= outgoing (:outgoing (tree/remote-relations (:relations moved) "b1"))))))))

(deftest open-close-editor-test
  (let [closed (ui/close-editor state)]
    (is (true? (get-in closed [:ui :editor-collapsed])))
    (is (false? (get-in (ui/open-editor closed) [:ui :editor-collapsed])))
    (is (true? (get-in (ui/select closed "a2") [:ui :editor-collapsed])) "選取卡片不會自動打開")
    (is (false? (get-in (ui/add-sibling closed "a2") [:ui :editor-collapsed])) "新增卡片時打開以輸入名稱")))

(deftest history-test
  (let [s (ui/select state "a2")]
    (testing "歷程只收目前選取的卡片，回應慢到時捨棄"
      (is (= {:id "a2" :data {:entries [] :more? false}}
             (get-in (ui/set-history s "a2" {:entries [] :more? false}) [:ui :history])))
      (is (nil? (get-in (ui/set-history s "a1" {:entries []}) [:ui :history]))))
    (testing "顯示較早的紀錄；換卡片後回到預設筆數"
      (let [s' (ui/show-all-history s)]
        (is (true? (get-in s' [:ui :history-all])))
        (is (nil? (get-in (ui/select s' "a1") [:ui :history-all])))))))

(deftest change-view-test
  (let [s (-> state (ui/select "a2") (ui/show-change "a2" "cs1"))]
    (testing "看某次修改：打開編輯區，兩個切換都不是 pressed"
      (is (false? (get-in s [:ui :editor-collapsed])))
      (is (false? (get-in s [:ui :show-previous])))
      (is (= {:id "a2" :changeset-id "cs1"} (get-in s [:ui :change]))))
    (testing "讀回的內容只在還是同一個檢視時寫入"
      (is (= {:before "a"} (get-in (ui/set-change s "a2" "cs1" {:before "a"}) [:ui :change :data])))
      (is (= s (ui/set-change s "a2" "cs2" {:before "a"})))
      (is (= s (ui/set-change s "a1" "cs1" {:before "a"}))))
    (testing "離開檢視"
      (is (nil? (get-in (ui/close-change s) [:ui :change])))
      (is (nil? (get-in (ui/select s "a1") [:ui :change])) "換卡片時清掉")
      (let [s' (ui/toggle-previous s)]
        (is (nil? (get-in s' [:ui :change])))
        (is (true? (get-in s' [:ui :show-previous]))))
      (let [s' (ui/show-current (ui/toggle-previous s))]
        (is (nil? (get-in s' [:ui :change])))
        (is (false? (get-in s' [:ui :show-previous])))))
    (testing "再看同一張卡片的差異不重設選取的狀態"
      (is (true? (get-in (-> s ui/show-all-history (ui/show-change "a2" "cs3")) [:ui :history-all]))))))

(deftest previous-test
  (let [s (ui/toggle-previous state)]
    (is (true? (get-in s [:ui :show-previous])))
    (is (= {:id "a2" :data {}} (get-in (ui/set-previous s "a2" {}) [:ui :previous])))))

(deftest set-prose-test
  (let [doc {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "新的本文"}]}]}]
    (testing "AI 生成的段落改了本文就轉為人工改過"
      (let [s (ui/set-prose state "a1" doc)]
        (is (= doc (get-in s [:nodes "a1" :node/prose])))
        (is (= :human (get-in s [:nodes "a1" :node/status])))
        (is (= (:history state) (:history s)) "逐字輸入不進復原紀錄")))
    (testing "尚未生成的段落直接撰寫也算人工"
      (is (= :human (get-in (ui/set-prose state "b1" doc) [:nodes "b1" :node/status]))))
    (testing "引言不套用段落狀態"
      (let [s (ui/add-child state "A" :intro)
            intro (get-in s [:ui :selected])]
        (is (nil? (get-in (ui/set-prose s intro doc) [:nodes intro :node/status])))))))

(deftest relations-edit-test
  (let [s (ui/toggle-relations-edit state)]
    (is (not (get-in state [:ui :editing-relations])) "關係預設唯讀")
    (is (true? (get-in s [:ui :editing-relations])))
    (is (nil? (get-in (-> s ui/open-rel-picker ui/toggle-relations-edit) [:ui :rel-picker]))
        "完成時關閉關係選取器")
    (is (false? (get-in (ui/select s "b1") [:ui :editing-relations])) "換卡片時回到唯讀")))

(deftest set-remote-relation-type-test
  (let [s (ui/add-remote-relation state "b1" "a1" :echo)
        changed (ui/set-remote-relation-type s "b1" "a1" :echo :basis)
        remote #(set (map :relation/type (:outgoing (tree/remote-relations (:relations %) "b1"))))]
    (is (= #{:basis} (remote changed)) "呼應改為依據")
    (is (= (inc (count (:history s))) (count (:history changed))) "一次可復原的修改")
    (testing "已有該類型時合併"
      (let [both (ui/add-remote-relation s "b1" "a1" :basis)]
        (is (= #{:basis} (remote (ui/set-remote-relation-type both "b1" "a1" :echo :basis))))))
    (is (identical? s (ui/set-remote-relation-type s "b1" "a1" :echo :echo)))))

(deftest apply-versions-test
  (let [a (random-uuid) b (random-uuid)
        state {:nodes {a {:node/id a :node/version 1}}}]
    (is (= {:nodes {a {:node/id a :node/version 3}}}
           (ui/apply-versions state {a 3 b 2}))
        "送出期間已刪除的卡片略過")))

(deftest lost-input-test
  (let [saved {:nodes (:nodes state)}
        prose {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "新本文"}]}]}
        local (-> state
                  (ui/set-node-field "a1" :node/draft "新意圖")
                  (ui/set-node-field "a2" :node/title "改名")
                  (ui/set-prose "b1" prose))]
    (is (= [{:id "a1" :field "意圖" :text "新意圖"}
            {:id "a2" :field "名稱" :text "改名"}
            {:id "b1" :field "本文" :text "新本文"}]
           (mapv #(dissoc % :title) (ui/lost-input local saved))))
    (is (= [] (ui/lost-input state saved)) "沒有改動時為空")
    (is (= [] (ui/lost-input (ui/set-node-field state "a1" :node/draft "  ") saved)) "空白不列")))

(deftest conflict-test
  (let [s (ui/show-conflict (assoc-in state [:ui :toast] "舊訊息") "「一」" [{:id "a1" :text "x"}])]
    (is (= {:names "「一」" :lost [{:id "a1" :text "x"}]} (get-in s [:ui :conflict])))
    (is (nil? (get-in s [:ui :toast])))
    (is (nil? (get-in (ui/dismiss-conflict s) [:ui :conflict])))))

(deftest agent-change-test
  (let [s (ui/agent-changed state {:label "建立大綱" :nodes ["a2" "b1"]})]
    (is (= {:label "建立大綱" :ids ["a2" "b1"] :n 1} (get-in s [:ui :agent-change])))
    (is (= 2 (get-in (ui/agent-changed s {:label "y" :nodes []}) [:ui :agent-change :n])))))

(deftest recent-test
  (let [s (-> state
              (ui/set-recent-cards [{:node/id "b1" :label "x" :by :user}])
              ui/toggle-recent)]
    (is (true? (get-in s [:ui :recent-open])))
    (is (= [{:node/id "b1" :label "x" :by :user}] (get-in s [:ui :recent])))
    (is (not (get-in (ui/toggle-recent s) [:ui :recent-open])))
    (testing "選一張卡片：選取並關閉清單"
      (let [s (ui/open-recent-card s "b1")]
        (is (= "b1" (get-in s [:ui :selected])))
        (is (nil? (get-in s [:ui :recent-open])))))
    (testing "在素材庫選卡片時切到卡片頁，其他頁不變"
      (is (= :tree (get-in (ui/open-recent-card (ui/set-page s :library) "b1") [:ui :page])))
      (is (= :fulltext (get-in (ui/open-note-card (ui/set-page s :fulltext) "b1") [:ui :page]))))))

(deftest notes-ui-test
  (testing "新增留言的輸入框與待辦預設"
    (let [s (ui/set-note-draft state "a1" "寫短一點")]
      (is (= "寫短一點" (get-in s [:ui :note-drafts "a1"])))
      (is (true? (ui/note-todo? s "a1")) "預設交代 agent 處理")
      (is (false? (ui/note-todo? (ui/set-note-todo s "a1" false) "a1")))
      (is (nil? (get-in (ui/clear-note-draft s "a1") [:ui :note-drafts "a1"])))))
  (testing "回覆"
    (let [s (-> state (ui/start-reply "n1") (ui/set-reply-draft "好"))]
      (is (= ["n1" "好"] [(get-in s [:ui :reply-to]) (get-in s [:ui :reply-draft])]))
      (is (nil? (get-in (ui/cancel-reply s) [:ui :reply-to])))))
  (testing "從完成的待辦看修改：選取、打開編輯區、切到那一次修改的差異"
    (let [s (-> state ui/toggle-todos (ui/open-change "b1" "cs1"))]
      (is (= "b1" (get-in s [:ui :selected])))
      (is (false? (get-in s [:ui :editor-collapsed])))
      (is (false? (get-in s [:ui :show-previous])))
      (is (= {:id "b1" :changeset-id "cs1"} (get-in s [:ui :change])))
      (is (nil? (get-in s [:ui :todos-open])))))
  (testing "重新載入時以後端的留言取代"
    (is (= [{:note/id "n"}] (:notes (ui/replace-data state {:nodes (vals (:nodes state)) :notes [{:note/id "n"}]}))))))

(deftest report-edit-test
  (let [s (ui/open-report-edit (assoc state :report {:report/title "舊名" :report/subtitle "舊副標"}))]
    (testing "開啟時以目前的名稱與副標當草稿"
      (is (= {:title "舊名" :subtitle "舊副標"} (get-in s [:ui :report-edit]))))
    (testing "沒有副標時草稿是空字串"
      (is (= {:title "測試報告" :subtitle ""} (get-in (ui/open-report-edit state) [:ui :report-edit]))))
    (testing "名稱空白不能儲存，副標可以留空"
      (is (true? (ui/report-draft-valid? s)))
      (is (false? (ui/report-draft-valid? (ui/set-report-draft s :title "  "))))
      (is (true? (ui/report-draft-valid? (ui/set-report-draft s :subtitle "")))))
    (testing "儲存成功：以回應取代、關閉面板；副標清除時移除"
      (let [saved (ui/report-saved (ui/set-report-draft s :title "新名")
                                   {:report/title "新名" :report/subtitle "新副標"})]
        (is (= "新名" (get-in saved [:report :report/title])))
        (is (= "新副標" (get-in saved [:report :report/subtitle])))
        (is (nil? (get-in saved [:ui :report-edit]))))
      (let [cleared (ui/report-saved s {:report/title "舊名"})]
        (is (not (contains? (:report cleared) :report/subtitle)))))
    (testing "取消：關閉面板，不改資料"
      (let [closed (ui/close-report-edit (ui/set-report-draft s :title "改了"))]
        (is (nil? (get-in closed [:ui :report-edit])))
        (is (= "舊名" (get-in closed [:report :report/title])))))))

(deftest fulltext-intents-test
  (is (true? (ui/fulltext-intents? state)) "預設顯示")
  (let [off (ui/toggle-fulltext-intents state)]
    (is (false? (ui/fulltext-intents? off)))
    (is (true? (ui/fulltext-intents? (ui/toggle-fulltext-intents off))))))

(deftest add-table-and-figure-test
  (testing "段落底下新增表格：label 自動產生、選取並進入名稱編輯"
    (let [s (ui/add-child state "a1" :table)
          id (get-in s [:ui :selected])
          n (get-in s [:nodes id])]
      (is (= :table (:node/kind n)))
      (is (= "a1" (:node/parent n)))
      (is (re-matches #"tab:[a-z0-9]{4}" (:node/label n)))
      (is (= id (get-in s [:ui :focus-title])))
      (is (= 1 (count (:history s))) "可復原")
      (testing "再新增圖，label 前綴為 fig:"
        (let [s2 (ui/add-child s "a1" :figure)
              fid (get-in s2 [:ui :selected])]
          (is (re-matches #"fig:[a-z0-9]{4}" (get-in s2 [:nodes fid :node/label])))
          (is (= [id fid] (tree/child-ids (:nodes s2) "a1")))))))
  (testing "標題底下不能新增表格"
    (is (= (:nodes state) (:nodes (ui/add-child state "A" :table)))))
  (testing "選取表格時，新增是在它後面加一張同類卡片"
    (let [s (ui/add-child state "a1" :table)
          t (get-in s [:ui :selected])
          s2 (ui/add-sibling s t)
          t2 (get-in s2 [:ui :selected])]
      (is (= :table (ui/sibling-kind (:nodes s) t)))
      (is (not= t t2))
      (is (= :table (tree/kind (:nodes s2) t2)))
      (is (not= (get-in s2 [:nodes t :node/label]) (get-in s2 [:nodes t2 :node/label])))
      (is (= [t t2] (tree/child-ids (:nodes s2) "a1"))))))

(deftest set-target-chars-test
  (let [t #(get-in % [:nodes %2 :node/target-chars])
        s (ui/set-target-chars state "a1" "400")]
    (testing "正整數（字串或數字）設定"
      (is (= 400 (t s "a1")))
      (is (= 250 (t (ui/set-target-chars state "a1" 250) "a1")))
      (is (= 12 (t (ui/set-target-chars state "a1" " 12 ") "a1"))))
    (testing "標題也可以"
      (is (= 3000 (t (ui/set-target-chars state "A" "3000") "A"))))
    (testing "空白或 nil 清除（key 移除）"
      (is (not (contains? (get-in (ui/set-target-chars s "a1" "") [:nodes "a1"]) :node/target-chars)))
      (is (not (contains? (get-in (ui/set-target-chars s "a1" nil) [:nodes "a1"]) :node/target-chars))))
    (testing "0、負數、非整數、非數字不改"
      (doseq [bad ["0" "-3" "1.5" "abc" 0 -1]]
        (is (= s (ui/set-target-chars s "a1" bad)) (pr-str bad))))
    (testing "不存在的卡片不改"
      (is (= state (ui/set-target-chars state "nope" "10"))))))

(deftest block-status-test
  (let [s (assoc state :blocks [{:block/id "b1" :block/kind :para :block/status :confirmed}])
        d (ui/set-block-status s "b1" :discarded)]
    (is (= :discarded (get-in d [:blocks 0 :block/status])))
    (is (= :confirmed (get-in (ui/set-block-status d "b1" :confirmed) [:blocks 0 :block/status])))
    (is (= s (ui/set-block-status s "b1" :candidate)) "不再有候選狀態")))

(deftest toggle-show-discarded-test
  (is (true? (get-in (ui/toggle-show-discarded state) [:ui :show-discarded])))
  (is (false? (get-in (-> state ui/toggle-show-discarded ui/toggle-show-discarded) [:ui :show-discarded]))))

(deftest open-block-test
  (let [s (-> (assoc state :blocks [{:block/id "b1" :block/status :confirmed}
                                    {:block/id "b2" :block/status :discarded}])
              (assoc-in [:ui :source-filter] :table)
              (assoc-in [:ui :source-query] "x")
              (assoc-in [:ui :source-file] "a.pdf"))
        r1 (ui/open-block s "b1")
        r2 (ui/open-block s "b2")]
    (is (= :library (get-in r1 [:ui :page])))
    (is (= "b1" (get-in r1 [:ui :library-selected])))
    (is (= [:all "" nil] ((juxt #(get-in % [:ui :source-filter]) #(get-in % [:ui :source-query]) #(get-in % [:ui :source-file])) r1))
        "清掉篩選，素材一定在清單裡")
    (is (not (get-in r1 [:ui :show-discarded])))
    (is (true? (get-in r2 [:ui :show-discarded])) "已捨棄的素材自動打開顯示已捨棄")
    (is (= "b2" (get-in r2 [:ui :library-selected])))))

(deftest style-test
  (testing "沒有風格時為空字串與 0 字"
    (is (= "" (ui/style-text state)))
    (is (zero? (ui/style-chars state))))
  (let [s (ui/set-style-text state "  使用「本計畫」 ")]
    (is (= {:style/key "report" :style/text "  使用「本計畫」 "} (:style s)))
    (is (= 7 (ui/style-chars s)) "字數不含前後空白")
    (is (= [] (:history s)) "逐字輸入不進復原紀錄")
    (testing "清空時保留空字串的實體"
      (is (= "" (ui/style-text (ui/set-style-text s ""))))
      (is (zero? (ui/style-chars (ui/set-style-text s "")))))
    (testing "沒有風格時輸入空字串不建立實體"
      (is (nil? (:style (ui/set-style-text state ""))))))
  (testing "存檔後最後修改為使用者"
    (is (= {:updated-by :user :updated-at :t} (:style-info (ui/style-saved state :t)))))
  (testing "從後端載入與取代資料"
    (let [loaded {:report {:report/title "r"} :nodes [] :relations [] :blocks [] :facts [] :notes []
                  :style {:style/key "report" :style/text "x"} :style-info {:updated-by :agent}}
          s (ui/from-server loaded)]
      (is (= "x" (ui/style-text s)))
      (is (= {:updated-by :agent} (:style-info s)))
      (is (= "" (ui/style-text (ui/replace-data s (dissoc loaded :style))))
          "取代資料時風格被清除"))))

(deftest report-setup-test
  (let [empty-s (-> (ui/init-state {:report/title "目錄名" :tree []}) (assoc-in [:report :report/subtitle] "舊副標"))]
    (testing "沒有卡片且這次還沒確認時需要先問名稱；有卡片或已確認則不需要"
      (is (true? (ui/report-setup-needed? empty-s)))
      (is (false? (ui/report-setup-needed? state)))
      (is (false? (ui/report-setup-needed? (assoc-in empty-s [:ui :report-named] true)))))
    (testing "草稿預填目前的名稱與副標，輸入後以草稿為準"
      (is (= {:title "目錄名" :subtitle "舊副標"} (ui/report-setup-draft empty-s)))
      (is (= {:title "新名" :subtitle "舊副標"}
             (ui/report-setup-draft (ui/set-report-setup-draft empty-s :title "新名")))))
    (testing "名稱空白不能確定，副標可留空"
      (is (true? (ui/report-setup-valid? empty-s)))
      (is (false? (ui/report-setup-valid? (ui/set-report-setup-draft empty-s :title " "))))
      (is (true? (ui/report-setup-valid? (ui/set-report-setup-draft empty-s :subtitle "")))))
    (testing "儲存成功後標為已確認、清掉草稿"
      (let [saved (ui/report-saved (ui/set-report-setup-draft empty-s :title "新名") {:report/title "新名"})]
        (is (= "新名" (get-in saved [:report :report/title])))
        (is (false? (ui/report-setup-needed? saved)))
        (is (nil? (get-in saved [:ui :report-setup])))))))
