# Knot 開發規格 v0.1（供 Claude Code 接手）

2026-10-26 · @維然

## 0. 文件用途與閱讀方式

本文件是開發的唯一依據，供 Claude Code 在本機接手開發。請匯出成 Markdown，放在 repo 的 `docs/SPEC.md`。

- **本規格為準。** 與其他資料衝突時，以本規格為準；需要改規格時，先提出再動手。
- **背景資料：** `docs/ideas.md` 是先前的討論文件，說明設計緣由，不是需求清單。
- **設計稿是視覺參考。** 版面、配色、互動以第 6 節描述為準，設計稿用來對照外觀。
- **標為「待定」的項目不要自行決定。** 實作到該處時先停下，列出選項請使用者決定。
- **用語：** 「卡片」指卡片樹上的節點；「本文」指 AI 生成或人工撰寫的段落文字；「素材」指使用者匯入並確認過的來源區塊；「意圖」指使用者在編輯器中為卡片輸入的要點與說明，是 AI 生成本文時的提示詞（見 5.4）。

- **有疑問時：** 把問題記在 `docs/QUESTIONS.md`，每題寫明相關章節、問題、可能的選項與各自影響。使用者會帶回本規格討論，定案後更新本文件，再從 `QUESTIONS.md` 移除該題。

## 1. 產品概要與範圍

Knot 是一個單機、單人使用的報告撰寫工具：使用者在卡片樹上組織結構與每張卡片的意圖、連結素材，AI 產生本文，最後單向匯出成 LaTeX 專案，交給使用者在自己的 LaTeX 環境完成。

初期對象是廠商撰寫政府專案的結案技術報告與案例報告，但資料模型與流程不得寫死政府報告的格式。

**範圍內**

- 報告建立（從範本或空白開始）
- 素材庫（獨立頁面）：匯入、切分成段落與表格區塊、確認、全文搜尋
- 事實表與實體表，可綁定到表格儲存格
- 卡片樹：最多四層標題（例如 3.1.1.1），加上段落、表格與圖，拖曳重組，選取置中
- 段落生成與編輯、段落關係、狀態管理
- 擴散修改：依賴分析、修改建議、審查、整組復原
- 文風設定：規則的編輯、分層繼承、報告內快照
- 交接檢查：一致性、契約項目涵蓋、缺少依據的陳述
- LaTeX 專案匯出，含局部匯出

**範圍外（不要實作）**

- 多使用者、權限、雲端同步
- 從 LaTeX 把修改讀回工具
- 審查意見回覆流程、期中到期末的延續
- Word 匯出
- Embedding 與向量搜尋：素材檢索只用 Datalevin 全文搜尋
- 工具內編譯 LaTeX：輸出正確性由使用者自行編譯驗證

## 2. 技術架構

應用程式是本機 Web 應用：Clojure 後端在 localhost 提供 API 與靜態檔，前端以 ClojureScript 撰寫，在使用者的瀏覽器中執行。前後端共用 `.cljc` 程式碼（例如編號規則、label 產生、資料驗證）。

**後端**

- Clojure（deps.edn）、http-kit（同時當伺服器與呼叫外部 API 的 HTTP client）、reitit、muuntaja（Transit JSON）、malli（資料驗證）
- integrant 管理元件生命週期，搭配 integrant.repl
- Datalevin：嵌入式資料庫；全文搜尋用於素材檢索；不使用向量與 embedding 功能
- 檔案解析交給外部工具 anydoc 轉成 Markdown（見 5.2）；後端以 commonmark-java 加 GFM 表格擴充解析 Markdown
- 呼叫 LLM 用 JDK 內建的 `java.net.http`，不另加 HTTP 函式庫

**前端**

- shadow-cljs、Replicant 負責渲染、Nexus 負責 action 分派
- 本文編輯器用 Tiptap 的核心套件 @tiptap/core（ProseMirror，不依賴 React），在 Replicant 的掛載生命週期中建立，以自訂 inline node 呈現事實引用與 LaTeX 交互參照
- 拖曳用瀏覽器原生 HTML5 drag and drop，另提供鍵盤上移、下移作為替代
- 字型優先使用系統字型（PingFang TC、Microsoft JhengHei、Noto Sans TC），不依賴網路字型，因為使用環境可能離線

選用 Replicant 與 Nexus 的理由：畫面是狀態的純函式，事件處理以資料描述，view 函式可以寫成 `.cljc`，在 JVM 上直接測試輸出的 hiccup。這是偏好而非硬性要求；遇到 Replicant 難以處理的需求時，可改用 Reagent 搭配 re-frame，但要先提出。

**LLM 呼叫**

- 不建立抽象層。`knot.llm` 命名空間提供一個 `call` 函式，用 http-kit client 呼叫 Anthropic Messages API，金鑰讀環境變數 `ANTHROPIC_API_KEY`。
- 每種 AI 工作是一個具名任務，例如 `:generate-paragraph`、`:classify-change`、`:propose-propagation`、`:split-source`、`:extract-style`。
- 每個任務的 prompt 範本放在 `resources/prompts/<任務>.md`，輸出一律要求 JSON，並以 malli schema 驗證；驗證失敗時重試一次，仍失敗就回報錯誤，不寫入資料。
- 測試以 `with-redefs` 替換 `llm/call`，回傳 `test-resources/fixtures/llm/` 下的固定回應。
- 使用的模型名稱放在設定檔，屬待定事項。

**報告目錄**

一個報告目錄就是一份報告，只有一個資料庫。啟動時讀取目錄中的報告；目錄中還沒有報告時，建立一份空白報告（名稱取自目錄名稱）。

```text
<報告目錄>/                    啟動時所在的目錄，或命令列參數指定
├─ .knot/db/                   Datalevin：這份報告的所有資料
├─ sources/                    匯入的原始檔複本
├─ figures/                    使用者提供的圖檔
└─ export/                     預設的 LaTeX 匯出位置
```

**Repo 結構**

```text
deps.edn  shadow-cljs.edn  package.json  bb.edn  CLAUDE.md
src/clj/knot/                  後端：db、api、llm、ingest、export、checks
src/cljc/knot/                 共用：schema、numbering、labels、tree、views
src/cljs/knot/                 前端：渲染迴圈、actions、editor
resources/prompts/             LLM 任務的 prompt 範本
resources/public/              index.html、css、編譯後的 js
dev/user.clj                   REPL 進入點
test/                          clj 與 cljs 測試
test-resources/fixtures/       虛構的範例報告、素材、LLM 回應
docs/SPEC.md  docs/ideas.md  docs/design/  docs/QUESTIONS.md
```

啟動方式：在報告目錄執行 `java -jar knot.jar`（或 `java -jar knot.jar <報告目錄>`），啟動伺服器並開啟瀏覽器；開發時為 `clojure -M:run [報告目錄]`，REPL 開發使用 `local/report`。jar 的打包方式待 M6 前後處理。

## 3. 開發環境

開發採 REPL 優先：後端與前端各開一個 nREPL，Claude Code 透過命令列工具對它們求值，先在 REPL 驗證行為，再寫成測試。

**兩個 nREPL**

| 用途 | 啟動指令 | Port |
|---|---|---|
| Clojure 後端 | `clojure -M:dev:nrepl` | 7888 |
| ClojureScript 前端 | `npx shadow-cljs watch app` | 7889（設定在 shadow-cljs.edn） |

- `:nrepl` alias 包含 nrepl 與 cider-nrepl，啟動後寫出 `.nrepl-port`。
- 前端 REPL：連上 7889 後執行 `(shadow.cljs.devtools.api/repl :app)`，瀏覽器需開著頁面。
- 用 babashka 的 `bb.edn` 包裝常用指令：`bb dev`、`bb test`、`bb cljs-test`、`bb lint`。
- 原則上由使用者啟動這兩個 nREPL。Claude Code 發現沒在執行時，可以在背景啟動，但不要關閉使用者已啟動的行程。

**`dev/user.clj` 提供的函式**

- `(go)`、`(halt)`、`(reset)`：透過 integrant.repl 啟動、停止、重新載入系統
- `(seed!)`：用 `test-resources/fixtures` 的虛構資料建立一份範例報告
- `(db)`：取得目前報告的 Datalevin 連線，方便直接下查詢
- `(import-file! "path")`：對單一檔案執行外部轉換與匯入，方便檢查轉換結果

**Claude Code 用的工具**

安裝 clojure-mcp-light（需要 babashka 與 bbin），它提供兩個工具：

- `clj-nrepl-eval`：從命令列對 nREPL 求值，例如 `clj-nrepl-eval -p 7888 "(user/reset)"`
- `clj-paren-repair-claude-hook`：在 Claude Code 寫入或編輯 `.clj` 檔時自動修正括號，設定在 `.claude/settings.json` 的 hooks

安裝時用 git tag 固定版本，並把版本記在 `CLAUDE.md`。

**每項工作的完成條件**

1. 修改後在 REPL 重新載入並驗證行為。
1. 補上或更新測試：後端 `clojure -M:test`，前端 `bb cljs-test`。
1. 測試全數通過，shadow-cljs 編譯沒有 warning，clj-kondo 沒有 error。

**`CLAUDE.md` 要寫的內容**

M0 時建立，至少包含：兩個 nREPL 的 port 與啟動方式、`clj-nrepl-eval` 用法、`dev/user.clj` 的函式、測試與 lint 指令、第 8 節的開發守則，以及「先讀 docs/SPEC.md」。

## 4. 資料模型

每個報告目錄一個 Datalevin 資料庫，不另設跨報告共用的資料庫。下列 schema 是 M1 的起點，實作時可補充屬性，但改名或刪除要先更新本節。

**報告資料庫**

```clojure
;; 共通：每個實體都有 :<ns>/id（:db.type/uuid、:db.unique/identity）
;; 結構化值（欄位定義、規則、快照）以 pr-str 存成字串，讀取時用 malli 驗證

;; 報告
:report/id              uuid
:report/title           string
:report/subtitle        string   ; 顯示用副標題
:report/style-name      string   ; 顯示用文風名稱
:report/style-snapshot  string   ; 套用時複製的文風規則（EDN）
:report/style-source    uuid     ; 來源文風設定，只用於追溯與提示同步

;; 卡片（標題、段落、表格、圖）
:node/kind        keyword  ; :heading :intro :paragraph :table :figure
:node/parent      ref      ; 第一層標題沒有 parent
:node/order-key   string   ; 分數索引，同一 parent 內排序
:node/name        string   ; 標題名稱，不含編號；層級由樹的深度決定（1–4），不另存
:node/title       string   ; 段落、表格、圖的標題
:node/draft       string   ; 意圖：使用者輸入的要點與說明，作為 AI 的提示詞；fulltext
:node/prose       string   ; 本文，ProseMirror JSON
:node/status      keyword  ; :none :ai :human :locked
:node/unlocked-status keyword ; 鎖定前的狀態，解除鎖定時還原
:node/version     long     ; 每次內容變更 +1
:node/label       string   ; LaTeX label，:db.unique/value
:node/facts       ref many ; 由 prose 抽出的事實引用
:node/rows        ref many ; 由 prose 抽出的實體引用
:node/table       ref      ; 表格卡片引用的素材表
:node/table-view  string   ; 欄位、篩選、排序（EDN）
:node/figure-file string   ; 圖檔相對路徑
:node/caption     string
:node/contract-items ref many

;; 段落關係
:relation/key     string   ; :db.unique/identity，見 tree/relation-key（相鄰：每對一條；不相鄰：每對每種類型一條）
:relation/from    ref
:relation/to      ref
:relation/type    keyword  ; 相鄰：:progression :causal :contrast :example
                           ; 不相鄰：:summarizes :basis :echo :contrast
:relation/remote  boolean  ; true 表示不相鄰（任意距離）的關係；:summarizes 一律視為不相鄰
:relation/pending boolean  ; true 表示「待確認」（只用於相鄰關係）

;; 素材
:source/file-name string
:source/path      string   ; 相對於報告目錄
:source/kind      keyword  ; :pdf :docx :xlsx :md :txt
:block/source     ref
:block/kind       keyword  ; :para :table
:block/title      string
:block/text       string   ; fulltext
:block/location   string   ; 例如 "p.12"、"工作表 2"
:block/status     keyword  ; :candidate :confirmed :discarded
:block/table      ref      ; 表格區塊對應的表

;; 表格與事實
:table/name       string
:table/kind       keyword  ; :data :entity
:table/columns    string   ; [{:col/id :col/name :col/type}]（EDN）
:table/name-column string  ; 實體表用來當名稱的欄位 id
:row/table        ref
:row/order-key    string
:row/cells        string   ; {col-id value}（EDN）
:fact/key         string   ; :db.unique/identity，例如 "saving-rate.site-a"
:fact/label       string
:fact/value       string   ; 未綁定儲存格時使用
:fact/unit        string
:fact/row         ref      ; 綁定儲存格時：列
:fact/column      string   ; 綁定儲存格時：欄位 id
:fact/block       ref      ; 事實所屬的素材區塊

;; 引用與契約
:citation/node    ref
:citation/block   ref
:citation/row     ref      ; 選填，引用到表格的某一列
:citation/where   string   ; 選填，引用的範圍說明（例如「欄：場域、節電率」）
:citation/order   long     ; 依據在段落中的順序
:contract/code    string   ; 例如 "2.3"
:contract/text    string

;; 修訂、修改組、修改建議
:changeset/id     uuid
:changeset/origin keyword  ; :manual :propagation :regenerate :move :import :undo
:changeset/at     instant
:changeset/label  string   ; 顯示用名稱，例如「移動「異常狀況說明」」
:changeset/undoes ref      ; :undo 修改組所復原的修改組
:revision/changeset ref
:revision/collection keyword ; :nodes :relations :blocks :facts
:revision/target  string   ; 「集合/識別鍵」，例如 "nodes/<uuid>"
:revision/node    ref      ; 卡片的修訂才有
:revision/version long
:revision/kind    keyword  ; :wording :fact :claim :structure（M5 判斷）
:revision/before  string   ; 變更前快照（前端實體形式的 EDN；新增時沒有）
:revision/after   string   ; 變更後快照（刪除時為 nil）
:proposal/trigger ref      ; 觸發的修訂
:proposal/target  ref
:proposal/base-version long
:proposal/dependency keyword ; :fact :summary :argument
:proposal/new-prose string
:proposal/state   keyword  ; :pending :accepted :rejected :stale :advisory
```

**文風與範本**（5.1、5.7 實作時加入報告資料庫，作為套用時的快照；共用的來源見 5.7）

```clojure
:style/name     string
:style/scope    keyword  ; :org :agency :project
:style/parent   ref
:style/rules    string   ; [{:rule/id :rule/kind :rule/text :rule/example}]（EDN）
:template/name  string
:template/skeleton string ; 章節樹與契約項目（EDN）
```

**資料規則**

- **對外只用 UUID。** Datalevin 的 entity id 不離開 db 命名空間，API 與前端一律用 `:x/id`。
- **編號不儲存。** 章節編號由 `.cljc` 的 numbering 函式依樹的位置計算，前後端共用。
- **排序用分數索引。** 拖曳只改被移動卡片的 `order-key`；實作放在 `.cljc` 並充分測試。
- **label 一經建立就不變。** 表格、圖在建立時產生 label，前綴 `tab:`、`fig:`，後接使用者提供的代稱或短 id；重新生成、移動都不改，只有使用者明確改名時才變。
- **本文的引用要抽出來存。** `prose` 中的事實引用、實體引用、來源引用，存檔時在同一筆交易中寫入 `:node/facts`、`:node/rows`、citation，讓「哪些段落用到這個事實」是一般 Datalog 查詢。
- **修訂只增不改。** 每次內容變更都屬於一個修改組；復原是新增一個反向的修改組，不刪除歷史。
- **前後端同步。** 前端以 `.cljc` 純函式算出新狀態，比較上次儲存的狀態（`knot.sync/diff`），只送出變動的卡片、關係、素材、事實；後端以一筆交易寫成一個修改組，並記下每個實體修改前後的快照。結構類的修改立即送出；逐字輸入在停止約 0.8 秒後送出，成為一個「編輯文字」修改組。
- **狀態轉換：** 生成後 `:none`→`:ai`；人工改本文後 `:ai`→`:human`；`:locked` 只由使用者設定或解除，AI 永遠不改鎖定的段落。引言以 AI 生成為主，不套用這組狀態。
- **段落關係的方向：** 相鄰關係由前一段指向後一段；不相鄰關係由本段（from）指向目標段落（to），例如「本段依據目標段落」。

## 5. 功能規格

每個模組列出行為與驗收條件。驗收條件中的「測試」一律以 `with-redefs` 替換 LLM 呼叫並回傳 fixture，資料使用 `test-resources/fixtures` 的虛構內容。

### 5.1 報告與範本

- 建立報告：空白，或從範本建立。範本的骨架會產生各層標題卡片與契約項目，並把標題對應到契約項目。
- 建立時選擇文風設定，把規則快照存進 `:report/style-snapshot`。
- **驗收：** 從 fixture 範本建立的報告，卡片樹結構、編號、契約對應都符合預期。

### 5.2 素材庫

- **匯入流程：** 原始檔複製到 `sources/`，以 anydoc 轉成 Markdown，後端再把 Markdown 切成區塊。解析不在 JVM 內做，因為 Clojure 生態缺少好用的文件解析套件。
- **轉換工具：** anydoc 是純 Rust 實作，不需要模型、可離線使用，支援 doc、docx、xls、xlsx、ppt、pptx、OpenDocument、RTF、EPUB、CSV、PDF。以 npm 開發相依安裝並固定版本，後端用子行程執行 `node_modules/.bin/anydoc <檔案>`，讀取 stdout。轉換失敗時依錯誤類型（不支援、加密、檔案損壞）顯示對應提示。
- **使用者可自行提供 Markdown：** 轉換結果不理想時，使用者可以修改轉出的 Markdown 再匯入；也可以直接匯入自己寫的 Markdown、純文字或 CSV。
- **Markdown 約定：** 後端解析 GFM 的標題、段落與表格。標題成為區塊的分組與標題來源；每個表格成為一個表格區塊；頁碼以 `<!-- page: N -->` 標記表示，使用者手寫 Markdown 時也可以加入。
- **掃描 PDF：** anydoc 不處理只有圖片的 PDF，本工具也不使用雲端 OCR。遇到時提示使用者改提供文字版，或自行轉成 Markdown。
- **M3 開始前的驗證：** 請使用者準備代表性檔案（含表格的中文 PDF、.doc、表頭有合併儲存格的 .xlsx）實際轉換，記錄 PDF 換頁位置是否保留、中文與表格品質、合併儲存格的輸出方式，寫進 `docs/ingest-evaluation.md`。依結果決定頁碼標記如何產生，以及本節是否需要調整。
- **候選區塊：** 解析結果一律是 `:candidate`。使用者可以確認、修改、合併相鄰段落、捨棄。
- **AI 輔助切分（選用）：** 按鈕觸發 `:split-source`，例如把散在文字中的列舉資料整理成表。結果仍是候選區塊，不會自動確認。
- **表格編輯：** 改欄名、設定欄位型別（文字、數字、日期），把表設為實體表並指定名稱欄。
- **事實綁定：** 在儲存格上建立事實，輸入 key、標籤、單位。
- **連結到段落：** 素材庫是獨立頁面，不與卡片樹並排。段落的依據在段落編輯區以「搜尋或選取」加入：開啟素材選取器，以全文搜尋與類型、狀態篩選找到區塊後加入；素材庫頁面的區塊詳情列出引用它的段落，可跳回段落編輯。不以拖曳連結。
- **搜尋：** Datalevin 全文搜尋區塊標題與內容，可依類型、狀態篩選。Datalevin 預設的分詞器以空白切詞，不適用中文，必須自訂分析器，例如中文字元以二元組（bigram）切分。
- **驗收：** fixture 的 docx、xlsx、pdf 經 anydoc 轉換後，產生正確的段落與表格區塊；含頁碼標記的 Markdown 產生帶頁碼的區塊；轉換失敗顯示對應提示；搜尋「節電」能找到含「節電率」的區塊。

### 5.3 卡片樹

- **層級：** 標題最多四層，例如第三章、3.1、3.1.1、3.1.1.1，各層稱為「章、節、項、款」。每個標題底下可以有一張選用的**引言**卡片，概述這一層的內容或說明其目的；引言最多一張，固定排在最前面，卡片上標示「引言」，標題的編輯區提供「新增引言」；引言的本文由 AI 依該層下層內容生成（見 5.4），使用者也可以先寫下意圖。引言之後，標題底下只能是段落（這一層直接產生內容）或下一層標題，兩者不混用；已有段落的標題要新增下層標題時，提供「把現有段落移到新的下層標題」或「把某段改為引言」。表格與圖只能掛在段落底下。拖曳時，插入位置的 parent 必須能容納該類卡片；移入時放在同類卡片的最後。拖曳只在同一層進行（見下方「拖曳」），因此標題的層級不會因拖曳而改變；跨層移動之後另以「移到…」選單提供。
- **依深度分欄：** 第 n 欄顯示樹中深度為 n 的所有卡片，不分類型，依 parent 分組，組間留較大間距；同一欄可能同時有標題卡片與段落卡片，以樣式區分（標題有左側深色條與灰藍底，段落白底，引言虛線，表與圖淡暖底）。欄位名稱依內容決定：標題用該層稱呼，另有段落、表與圖時一併列出，例如「項／段落」。最多六欄（四層標題、段落、表格與圖）；欄寬依看板全寬均分、每欄至少約 195px，放不下時水平捲動；編輯區以抽屜覆蓋，不影響欄寬；選取時水平捲動，讓選取卡片所在欄與其上、下一欄都在可視範圍內。卡片顯示標題與最多兩行摘要；尚未生成本文的段落另顯示「尚未生成」標籤。AI 生成、人工改過、鎖定不另加標籤（狀態仍記錄，鎖定在編輯區的本文區顯示說明）。
- **選取與透明度：** 選取卡片為白底、2px 深藍框；其祖先與子孫為淡藍底實色；同一 parent 的兄弟透明度 0.7；其他卡片 0.35。
- **選取置中：** 選取後每欄各自垂直捲動，讓相關卡片對齊欄位中線：祖先欄對齊該祖先；同層欄對齊選取的卡片；子孫欄對齊所有子孫的整體範圍；沒有子孫時對齊它們在樹中應有的位置；範圍高於可視區時改為頂端對齊。動畫約 0.45 秒，並遵守 `prefers-reduced-motion`。
- **拖曳（只在同一層）：** 整個欄位都是放置區，不必對準卡片：在同一欄（同深度）依指標高度取最近的合法間隙插入，並顯示插入線（組與組之間的間隙，靠近上方卡片底緣是接在它後面、靠近下方卡片頂緣是插在它前面）；在左邊一欄（深度少一）移入指標所在或最近的卡片，成為最後一個子卡片，該卡片顯示虛線框，可用於移到其他 parent 或還沒有子卡片的標題。拖曳中標示可放置的兩欄、其他欄淡出；按 Esc 取消。介面不放操作說明文字。不能拖到自己或自己的子孫，其餘合法性依上方的層級規則判斷。拖曳時指標靠近欄位上下緣，該欄自動捲動。
- **鍵盤替代：** Alt+↑／↓ 上移、下移選取的卡片。
- **移動之後：** 被移動的卡片成為選取狀態；編號重算；若是段落，舊位置的下一段、被移動的段落、新位置的下一段，與前一段的關係都標為待確認；顯示一行訊息說明變動；整個移動是一個 `:move` 修改組，可復原。
- **新增與刪除：** 卡片樹上方只有「新增」與「刪除」，都作用在選取卡片的層級上：「新增」在選取卡片後面加一張同類卡片（標題或段落；選取引言時依同層其他卡片的類型，沒有時為段落），新卡片立即選取並進入名稱編輯；「刪除」連同下層卡片一起刪除，可復原。空標題底下的第一張卡片與引言，從標題編輯區的「下層內容」新增。表格與圖的新增待 label 命名方式定案後提供。
- **捲動：** 頂部列固定，卡片樹與編輯區各自捲動；滾輪在卡片欄位上時只捲動該欄，不捲動整頁。窄螢幕時各區依序排列，回到整頁捲動。
- **鍵盤：** 方向鍵在卡片間移動（左右換層、上下換兄弟）。移動卡片只用拖曳與 Alt+↑／↓，編輯區不提供上移、下移按鈕。
- **驗收：** 編號、分數索引、拖曳合法性、關係標記都有單元測試；畫面行為由使用者對照設計稿確認。

### 5.4 段落詳情與生成

- **段落關係：** 相鄰關係（遞進、因果、對比、舉例）只在同一標題下前後相接的段落之間，移動後標為待確認。不相鄰關係可連到任意距離的段落，類型為摘要（本段彙整目標的結果）、依據（本段論點建立在目標的結果上）、呼應、對比；在段落編輯區以「新增關係」選類型並搜尋目標段落建立。選取段落時，卡片看板以虛線框標出與它有不相鄰關係的卡片。
- **段落編輯區：** 標題列是名稱與「⋯」選單（結構操作，例如「改為引言」，之後的「移到…」也放這裡）。內容依生成的流程由上而下分成三個固定區塊：
  1. **生成依據**（使用者在這張卡片上設定的，唯一要輸入的地方）：意圖（這段的重點）；依據（引用的素材，以搜尋或選取加入）；關係（與前一段、後一段的相鄰關係類型與確認待確認的關係，以及本段新增的不相鄰關係）；下層表與圖。
  2. **AI 會參考的內容**（自動組成，只能看）：依「上層、本段、鄰近、下層、關係、素材、文風」分組列出摘要，每一項標明帶的是本文還是意圖，展開可看全部內容；其他段落指向本段的關係（例如被摘要）也列在這裡。
  3. **本文**：本文、鎖定、生成，以及「目前內容／與前一版比較」切換。

  視覺上以表面區分三個區塊的角色：生成依據是白底表單，AI 會參考的內容是淡藍底的唯讀面板，本文以白色紙張樣式呈現。標題列固定在抽屜頂端，顯示名稱；尚未生成的段落在名稱旁標示「尚未生成」。欄位的新增操作（加入依據、新增關係等）是欄位標題右側的文字按鈕，不用整列按鈕。意圖與本文用同一種編輯框（高度隨內容）：平常是淡底的閱讀樣式，滑過時提示「點擊編輯」；點進去後是白底藍框並標示「編輯中 · Esc 結束」，按 Esc 結束編輯（再按一次才收合編輯區）；鎖定的本文提示「已鎖定」。關係預設唯讀（類型是標籤、沒有移除與新增），按欄位右側的「編輯」後整個欄位進入編輯狀態，才能改類型（相鄰與不相鄰關係都可以；不相鄰關係改類型是一次可復原的「變更關係類型」）、確認、移除與新增，按「完成」離開；換卡片時回到唯讀。依據、關係、下層表與圖用同一種底色項目列：開頭是完整的類型標籤（段落、表格、依據、呼應…），接著名稱，右側是來源等補充資訊與操作。關係列依序是「關係類型、位置、卡片名稱」：相鄰關係的類型以下拉選單直接放在開頭，位置寫「前一段／後一段」；不相鄰關係的位置是目標段落所屬標題的編號（例如 1.2.1，章底下的段落寫「第一章」）。不用數字標示區塊順序，避免與看板欄位的層級編號混淆。
- **與前一版比較：** 本文區可切換為修改前後的差異，比較目前的本文和最近一次修改之前的本文（意圖不需要歷程）（刪除的字以紅色刪除線、新增的字以綠色底線標示）。只保留前一版，不提供版本清單；連續的逐字輸入（相隔 10 分鐘內的「編輯文字」修改組）算同一次修改；建立卡片不算修改。前一版取自修改組的修訂快照（`GET /api/report/nodes/:id/previous`）。
- **標題、引言、表與圖的編輯區** 沿用相同的區塊樣式：標題為「設定」（意圖、契約項目）與「結構」（引言、下層內容）；引言為生成依據（意圖，可留空）、AI 會參考的內容、本文；表與圖為「設定」（意圖、label、圖檔）。
- **使用者輸入即意圖：** 使用者在編輯器中為卡片輸入的內容（標題名稱、意圖、關係類型與說明）都是該卡片的提示詞，存在卡片上，不直接出現在報告成品中。AI 生成某一段時，周邊卡片**有本文就帶本文，沒有就帶意圖**：一次起草多段時鄰段都還沒有本文，各段自然獨立生成；之後新增或修改一段時，已有的鄰段本文會帶進來銜接，不需要另外切換模式。
- **生成時組裝的輸入：**
  - 上層：章節路徑及各層標題的意圖；所屬標題的引言（有本文帶本文）。
  - 本段：本段意圖；下層表格與圖的 label 與意圖。
  - 鄰近：前一段、後一段（有本文帶整段本文，否則帶意圖）與相鄰關係類型。
  - 關係：本段摘要、依據的目標段落帶本文（要用到它們的結果），呼應、對比只帶意圖；以摘要關係連到本段的段落帶意圖。
  - 素材：引用區塊的內容；引用表格的欄位與相關列；引用素材上的事實（key、標籤、值、單位），不帶整份報告的事實。
  - 文風：文風規則。組裝邏輯寫成 `.cljc` 純函式，編輯區的預覽與實際呼叫共用同一份結果。
- **引言的生成：** 引言主要由 AI 補上。輸入改為標題名稱與意圖、引言的意圖（可留空），以及這一層的下層內容（下層標題或段落，有本文帶本文，否則帶意圖）。引言適合在下層內容大致完成後才生成；下層還沒有本文時，介面提示這一點。
- **輸出格式：** 模型回傳 JSON，本文以標記表示引用：`{{fact:key}}`、`{{ref:label}}`、`[[cite:block-id]]`。後端轉成 ProseMirror JSON。
- **驗證：** 標記中不存在的事實、label、區塊一律視為錯誤；本文中不在標記內的數字，標為「未綁定的數字」提醒使用者。
- **重新生成：** `:human` 段落先顯示新舊差異，經使用者確認才覆寫；`:locked` 段落不提供生成。
- **人工編輯：** 本文區即是 Tiptap 編輯器，未鎖定時可直接修改（尚未生成時也可直接撰寫）；改了本文就轉為 `:human`，並觸發 5.6 的擴散流程。鎖定的段落本文唯讀。逐字輸入與意圖一樣在停頓約 0.8 秒後存成「編輯文字」修改組。
- **驗收：** 以替換後的 LLM 呼叫生成的本文含事實引用節點；含裸數字的回應會產生提醒；鎖定段落無法生成。

### 5.5 事實

- 事實的值：綁定儲存格時讀取該格，否則用 `:fact/value`。
- 本文中的事實引用在顯示與匯出時才代入值，所以改事實不需要修改任何本文，也不需要審查。
- 事實的值改變時，列出引用它的段落讓使用者知道，但不產生修改建議。
- **驗收：** 修改來源表儲存格後，引用該事實的段落顯示新值。

### 5.6 擴散修改

「擴散修改」只是內部機制的名稱，介面上不使用這個詞，也不把它當成獨立功能呈現。使用者只會看到「相關段落有修改建議」，以及審查這些建議的畫面。設計稿中出現的「擴散」字樣以本規格為準，實作時改用一般用語。

1. **判斷修改類型：** 段落的意圖或本文存檔後，以 `:classify-change` 判斷為措辭、事實或論點。措辭修改到此結束。
1. **找出受影響的段落：** 以 Datalog 規則查詢。摘要依賴：有 `:summarizes` 關係指向此段的段落，以及此段所有上層標題的引言（引言對其所屬層的摘要依賴是隱含的，不另存關係）。論證依賴：下一段（若關係為因果或遞進），以及以「依據」關係指向此段的段落。「呼應」「對比」不構成依賴。
1. **產生建議：** 目標為 `:locked` 時只列出、不產生建議；為 `:human` 時產生 `:advisory` 建議，只給文字提示；其餘以 `:propose-propagation` 產生新本文，狀態 `:pending`，並記下 `base-version`。AI 判斷目標不需要修改時，不產生建議。
1. **審查：** 逐條接受或拒絕。接受時若目標的版本已不等於 `base-version`，改標為 `:stale`，不套用。一次接受的多條建議屬於同一個 `:propagation` 修改組。
1. **只擴散一層：** 接受建議產生的修改不再觸發擴散。

- **驗收：** 論點修改會對摘要依賴與因果依賴的段落產生建議；措辭修改不產生；過期偵測正確；整組復原正確。

### 5.7 文風設定

- 規則清單可新增、編輯、刪除，每條規則有類別、說明、範例。
- 分層合併：公司 < 機關 < 專案，同 id 的規則由下層覆蓋。
- 共用的文風與範本存成 EDN 檔，放在使用者設定目錄：`~/.knot/styles/`、`~/.knot/templates/`（不另設共用資料庫）。檔案格式待使用者提供範例後定案。
- 套用到報告時存成快照；設定目錄中的來源檔更新後，報告顯示「文風設定有更新」並提供同步。
- 從範例文字萃取規則（`:extract-style`）放在 M8。
- **驗收：** 合併結果與快照內容有單元測試。

### 5.8 交接檢查

- **一致性：** 本文中出現實體的別名寫法卻未使用實體引用（別名由使用者在實體表維護）；本文中未綁定的數字。
- **契約涵蓋：** 沒有任何卡片對應的契約項目。
- **缺少依據：** 有本文但沒有引用的段落；引用了未確認區塊的段落。
- 每個項目可以跳到對應卡片；匯出時可選擇轉為待辦標記。
- **驗收：** fixture 報告中預先埋好的每類問題都被找出。

### 5.9 LaTeX 匯出

- **專案結構依使用者的範例專案，屬待定事項。** 範例到位前先採用：每章一個 `chapters/chNN.tex`，表格各自一個 `tables/<label>.tex`，圖檔複製到 `figures/`；主檔由使用者提供，工具不產生。
- **章節檔：** 四層標題依序輸出為 `\chapter`、`\section`、`\subsection`、`\subsubsection`（第四層是否編號取決於使用者 .cls 中 secnumdepth 的設定）；引言緊接在標題指令之後輸出，段落之間空一行；事實代入值與單位；交互參照依類型輸出為 `表~\ref{tab:…}`、`圖~\ref{fig:…}`；段落之後依序輸出子表格的 `\input` 與圖的 figure 環境。
- **跳脫：** `% $ & # _ { } ~ ^ \` 都要正確跳脫，全形字元不處理。
- **表格檔：** 由表格視圖確定性產生，預設 booktabs；檔案開頭註明由工具產生、來源表與匯出時間，請勿手動修改。
- **選項：** 段落追溯註解 `% node:<uuid> src:<block-ids>`；交接檢查的未解決項目轉為 `\todo{}`（使用者的範例需載入 todonotes）。
- **局部匯出：** 只寫入選取的章節或表格，不刪除其他檔案；目標檔存在且不是工具產生的檔案時，先詢問才覆寫。
- **驗收：** 以 fixture 報告做 golden file 測試，比對輸出的 `.tex` 字串；跳脫有單元測試。能否編譯由使用者自行驗證。

### 5.10 復原

- 在本文編輯器以外按 Ctrl+Z 或 Cmd+Z，復原最近一個尚未復原的修改組，不論來源是手動、移動、擴散或重新生成；先送出尚未儲存的輸入。匯入與復原本身不在復原範圍。
- 本文編輯器內使用 Tiptap 自己的復原。
- **驗收：** 每種來源的修改組都能復原，復原後資料與修改前一致。

## 6. 介面設計稿位置與畫面規格

設計稿的正本放在 claude.ai，repo 裡放匯出的圖檔與索引。Claude Code 讀 repo 內的檔案，不直接開 claude.ai 連結，因為那需要登入。

**設計稿存放位置**

| 位置 | 內容 | 維護者 |
|---|---|---|
| 設計稿畫布 | 三個畫面的正本，主編輯畫面可實際操作 | 使用者 |
| 討論文件 | 設計緣由，匯出為 `docs/ideas.md` | 使用者 |
| `docs/design/README.md` | 畫面索引：檔名、對應的規格章節、更新日期、畫布連結 | Claude Code 建立，使用者確認 |
| `docs/design/screens/` | 從畫布匯出的畫面圖檔，例如 `01-main-editor.png`、`02-propagation-review.png`、`03-handoff-export.png` | 使用者匯出 |
| `docs/design/prototype/` | 選用：畫布的原型原始碼，其中的置中與拖曳邏輯可當參考實作，但無法單獨執行 | 使用者 |

新增或修改畫面時：先改畫布，再匯出到 `screens/`，最後更新 `README.md` 索引。本節以檔名指稱畫面。

**視覺規範**

| 名稱 | 值 | 用途 |
|---|---|---|
| ground | `#F4F3EF` | 頁面底色 |
| surface | `#FFFFFF` | 卡片、面板 |
| ink | `#1C2026` | 主要文字 |
| muted | `#59606A` | 次要文字 |
| line | `#DEDBD3` | 邊框、分隔線 |
| navy | `#27457F` | 選取、主要按鈕、AI 生成狀態 |
| navy-tint | `#E8EDF7` | 上下層卡片底色、事實引用 |
| amber | `#8A4608` 字 / `#FBF1E4` 底 | 待審、待確認、未確認素材 |
| teal | `#0E6B54` 字 / `#E3F2EC` 底 | 人工改過、已確認 |

字型：介面用無襯線（PingFang TC、Microsoft JhengHei、Noto Sans TC）；本文預覽用襯線（Noto Serif TC、Songti TC），模擬報告成品；label、檔名、LaTeX 片段用等寬字。可點擊元件最小高度 44px。

**01 主編輯畫面**（`01-main-editor.png`）

- 頂部列：左側為專案名稱與目前路徑，中間為置中的三個頁籤「素材」「卡片」「全文」，右側為匯出 LaTeX。待審的修改建議與交接檢查的入口於 M5、M7 再加入。「卡片」頁籤即本畫面；「全文」頁籤見下方「全文視圖」；「素材」頁籤見 04。
- 左側卡片樹：依深度分欄，一次顯示四欄，行為見 5.3。
- 右側編輯區：覆蓋在卡片看板上的抽屜（約 560px），不擠壓看板；預設收合，點擊卡片只選取、不會自動打開；由使用者點把手展開，新增卡片時自動打開以輸入名稱。可收合（Esc 或收合按鈕），收合後看板右緣留把手顯示選取的卡片、點擊展開。抽屜打開時，看板水平捲動讓選取卡片所在欄與其上、下一欄落在未被遮住的範圍。內容：選取卡片的編輯內容，段落時見 5.4；標題時編輯名稱與意圖，顯示契約項目、引言（可新增）與下層內容；引言時編輯意圖；表格、圖時編輯名稱與意圖並顯示 label。各類卡片的區塊安排見 5.4。
- 寬度不足時兩欄依序換行，卡片樹在自己的容器內水平捲動。
- **全文視圖：** 三欄：
  - 左側大綱：列出所有標題，依層級縮排；選取的卡片所在的章節標為目前位置；點標題即選取並把全文捲到該處。
  - 中間全文：依序呈現整份報告的標題與本文，靠左排列並限制行寬；點標題或段落即選取。從大綱或其他地方選取時，全文捲到該處。
  - 右側編輯區：固定在右側，不覆蓋全文、不收合（卡片頁的編輯區仍是可收合的抽屜）。本文在編輯區修改，存檔寫回對應卡片。

**02 修改建議審查**（`02-propagation-review.png`）

- 左側：觸發的修改（差異標示）、修改類型、受影響的段落。
- 右側：建議清單，每條標示依賴類型、目標位置、目標狀態，並以刪除線與底色標示差異。
- 四種卡片樣式：可套用（接受、略過）、人工改過（只給提示）、已過期（重新產生）、底部整批接受列。

**04 素材庫**（尚無設計稿）

- 獨立頁面：左側搜尋與篩選（類型、狀態、來源檔），中間區塊清單，右側區塊詳情（完整內容或表格、來源與位置、確認狀態、引用它的段落）。
- 候選區塊在此確認、修改、合併或捨棄；匯入來源檔也在此頁。

**03 交接檢查與 LaTeX 匯出**（`03-handoff-export.png`）

- 左側：一致性、契約涵蓋、缺少依據三組檢查結果，各項可跳到卡片或一鍵修正。
- 右側：匯出目錄預覽（標示工具產生的檔案）、追溯註解與待辦標記選項、整份或局部匯出、章節檔預覽。

**尚無設計稿的畫面**

素材庫（含候選區塊確認與合併）、素材選取器、表格編輯、文風設定、報告建立。依上述視覺規範自行設計，先做最簡可用版本，完成後請使用者確認；確認後的畫面也要補進畫布與 `screens/`。

## 7. 里程碑

原則上依序進行；目前依使用者決定先把畫面層（M2 與各畫面的前端）做到一個階段，資料層（M1）之後再接。每個里程碑開一個分支，完成時測試全數通過，並請使用者確認畫面後再合併。順序原則是先做能自動驗證的部分，最需要人工試用的介面細節放在後面迭代。

1. **M0 骨架與開發環境：** deps.edn、shadow-cljs、integrant 系統、兩個 nREPL、`dev/user.clj`、bb 任務、clj-kondo、clojure-mcp-light 設定、`CLAUDE.md`、`docs/design/README.md`、虛構 fixture。**完成條件：** `bb dev` 起得來，瀏覽器看到空白頁，兩個 nREPL 都能用 `clj-nrepl-eval` 求值。
1. **M1 資料層：** 第 4 節 schema、各實體的存取函式、編號與分數索引（cljc）、樹與依賴的 Datalog 規則、修訂與修改組、復原。**完成條件：** 資料規則都有測試。
1. **M2 卡片樹與詳情面板：** 主編輯畫面的版面、依深度分欄的卡片樹、選取置中、透明度、拖曳、鍵盤操作、段落編輯區（意圖可編輯，本文先唯讀）、素材庫頁面與素材選取器的前端。**完成條件：** 5.3 的單元測試通過，使用者對照 `01-main-editor.png` 確認。
1. **M3 素材庫與事實：** anydoc 驗證與整合、Markdown 匯入、候選區塊確認、表格編輯、實體表、事實綁定、中文全文搜尋、素材選取器接上全文搜尋。**完成條件：** 5.2 與 5.5 的驗收。
1. **M4 生成與本文編輯：**（Tiptap 編輯器、事實與交互參照節點、人工編輯的狀態轉換已在 M2 提前完成） 以 http-kit 呼叫 LLM 的函式、prompt 範本、`:generate-paragraph`、標記轉換與驗證、Tiptap 編輯器與自訂節點、狀態轉換、全文視圖。**完成條件：** 5.4 的驗收；使用者用真實 API 試生成一段。
1. **M5 擴散修改：** 修改分類、依賴查詢、建議產生、審查畫面、整組復原。**完成條件：** 5.6 的驗收，使用者對照 `02-propagation-review.png` 確認。
1. **M6 LaTeX 匯出：** 需要使用者提供範例專案；尚未提供時先依 5.9 的預設結構實作，並在 PR 說明中標註。**完成條件：** golden file 測試通過，使用者自行編譯確認。
1. **M7 交接檢查：** 5.8 的三類檢查、跳到卡片、待辦標記、匯出畫面整合。**完成條件：** 5.8 的驗收，使用者對照 `03-handoff-export.png` 確認。
1. **M8 延伸功能（待定）：** 從範例文字萃取文風、從舊報告開始。開始前先與使用者確認範圍與舊報告格式。

## 8. 開發守則與待定事項

**開發守則**

- **測試不呼叫真實 API。** 所有自動測試以 with-redefs 替換 LLM 呼叫；真實呼叫只在使用者明確要求時，於 REPL 中手動進行。
- **只用虛構資料。** repo 內不放任何真實的政府專案素材，fixture 一律虛構（例如「OO 市智慧路燈示範計畫」）。
- **純函式優先放 `.cljc`。** 編號、label、樹運算、合法性判斷、文風合併都寫成純函式，方便前後端共用與測試。
- **新增相依套件要說明。** 在 PR 說明中寫出套件名稱、版本與理由；所有版本都固定，不用範圍或 latest。
- **不擴大範圍。** 第 1 節的範圍外項目不實作；覺得需要時先提出。
- **遇到待定事項就停下。** 列出選項與各自影響，請使用者決定。

**待定事項**

- LaTeX 範例專案：目錄結構、章節與表格的寫法、表格環境（由使用者提供）
- LLM 模型的選擇與每次生成的 token 上限
- label 的命名方式：自動產生短 id，或要求使用者輸入代稱
- 可執行 jar 的打包細節（啟動方式已定：在報告目錄執行 jar）
- 共用文風與範本的 EDN 檔案格式（由使用者提供範例）
- 從舊報告開始時，舊報告的匯入格式（LaTeX、Word 或 PDF）
- Datalevin 與其他相依套件的版本，於 M0 固定並記錄

- anydoc 驗證結果：PDF 換頁位置、中文 PDF 與表格品質、合併儲存格的輸出方式
