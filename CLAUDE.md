# Knot

**先讀 `docs/SPEC.md`。** 它是開發的唯一依據；與其他資料衝突時以規格為準，要改規格先提出。

## 文件

- `docs/SPEC.md`：開發規格
- `docs/ideas.md`：設計緣由與尚未納入規格的構想（例如重新定位為 agent 的白板的理由），不是需求清單
- `docs/design/README.md`：設計稿索引；圖檔在 `docs/design/screens/`
- `docs/QUESTIONS.md`：待使用者決定的問題

## 開發環境

| 用途 | 啟動指令 | Port |
|---|---|---|
| Clojure 後端 nREPL（啟動時自動 `(user/go)`，HTTP 在 3000） | `bb dev:clj`（= `clojure -M:dev:nrepl`） | 7888 |
| ClojureScript 前端 nREPL | `bb dev:cljs`（= `npx shadow-cljs watch app`） | 7889 |

- `bb dev` 同時啟動兩者；頁面在 http://localhost:3000。
- **報告目錄：** 一個目錄一份報告，資料庫在 `<報告目錄>/.knot/db`。REPL 開發使用 `local/report`（不進 git）；`clojure -M:run [目錄]` 預設為目前目錄。空白報告的畫面提供「匯入範例內容」（`POST /api/dev/sample`，本機測試報告優先）。
- **API：** `GET /api/report`、`POST /api/report/changes`（malli 驗證）、`POST /api/report/undo`、`GET /api/report/nodes/:id/previous`（卡片的前一版）、`GET /api/report/recent-changes`（最近修改的卡片）、`GET /api/events`（SSE）。
- URL 參數 `?select=<卡片 id 或名稱>`、`?page=library|tree|fulltext`、`?drag=<卡片 id>`（拖曳中的樣子）、`?editor=1`（展開編輯區，預設收合）、`?previous=1`（本文區與前一版比較）、`?flash=<卡片 id 或名稱>`（模擬 agent 剛修改的綠色框，約 4 秒）指定初始畫面，方便截圖對照設計稿；截圖可用 headless Chrome：
  `"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --headless=new --window-size=1440,1000 --timeout=4000 --screenshot=out.png "http://localhost:3000/?select=p312&editor=1"`（頁面一直開著事件通知的連線，`--virtual-time-budget` 會等不到結束，要用 `--timeout`）
- 改了 `knot.server` 的路由要 `(user/reset)` 才會生效；reset 失敗時 `(require 'user :reload)` 再 `(user/go)`。
- **真實參考資料：** 使用者提供的 PDF 放在 repo 根目錄、轉換結果在 `local/sources/*.md`，兩者都在 `.gitignore`。`knot.ingest/markdown->blocks` 可把它們切成候選區塊（M5 的匯入會用到）。**commit 時一律指定路徑，不要 `git add -A`。**
- **本機測試報告：** `local/sample-report.edn` 存在時，「匯入範例內容」與 `(seed! "local/sample-report.edn")` 使用它。它由 `local/make_sample.py` 產生（節錄自參考報告），重新產生：`python3 -I local/make_sample.py local/sample-report.edn`。刪掉這個檔案就改用虛構的路燈範例。自動測試一律使用 `test-resources/` 的虛構資料。
- 轉換：`node_modules/.bin/anydoc <檔案.pdf> > local/sources/<檔名>.md`；評估記錄在 `docs/ingest-evaluation.md`。
- 原則上由使用者啟動兩個 nREPL；沒在執行時可在背景啟動，但不要關閉使用者已啟動的行程。

### 用 `clj-nrepl-eval` 求值

- 後端：`clj-nrepl-eval -p 7888 "(user/reset)"`；改了檔案後 `(require 'knot.xxx :reload)` 或 `(user/reset)`。
- 前端（瀏覽器需開著 http://localhost:3000）：
  - 單次求值：`clj-nrepl-eval -p 7889 "(shadow.cljs.devtools.api/cljs-eval :app \"(+ 1 2)\" {})"`
  - 或先 `(shadow.cljs.devtools.api/repl :app)` 把該 session 切成 CLJS 模式，之後直接送 CLJS 程式碼。
- `clj-nrepl-eval --discover-ports` 可列出執行中的 nREPL。

### `dev/user.clj`

- `(go)`、`(halt)`、`(reset)`：透過 integrant.repl 啟動、停止、重新載入系統；`(system)` 取得目前系統
- `(seed!)`：把虛構範例匯入目前的空白報告；`(seed! "local/sample-report.edn")`：匯入本機測試報告
- `(wipe!)`：刪除 `local/report` 的資料庫並重新啟動（得到空白報告）
- `(db)`：目前報告的 Datalevin 連線
- `(import-file! "path")`：M5 加入；`(mcp! "tool" {...})`：M3 加入

### 測試與 lint

- `bb test`（= `clojure -M:test`）：後端測試，含 `.cljc`
- `bb cljs-test`：前端測試（shadow-cljs `:test` build，node 執行）
- `bb lint`（= `clojure -M:lint --lint src test dev`）：clj-kondo，裝在 repo 的 alias 內
- `bb start`（= `clojure -M:run`）：一般模式啟動並開啟瀏覽器

## 每項工作的完成條件

1. 修改後在 REPL 重新載入並驗證行為。
2. 補上或更新測試。
3. 測試全數通過，shadow-cljs 編譯沒有 warning，clj-kondo 沒有 error。

## 開發守則

- **Knot 不呼叫 LLM，測試也不呼叫模型。** 需要模型的工作由 agent 透過 MCP 完成（SPEC 第 1 節、5.12）；測試直接呼叫 MCP 工具模擬 agent，交回的內容放在 `test-resources/fixtures/agent/`。
- **只用虛構資料。** repo 內不放真實政府專案素材。
- **純函式優先放 `.cljc`。** 編號、label、樹運算、合法性判斷、文風合併。
- **新增相依套件要說明**名稱、版本與理由；版本一律固定（npm 有 `.npmrc` 的 `save-exact`）。
- **不擴大範圍。** SPEC 第 1 節的範圍外項目不實作。
- **遇到待定事項就停下**，列出選項請使用者決定；疑問記在 `docs/QUESTIONS.md`。
- 每個里程碑開一個分支，測試通過且使用者確認畫面後再合併。

## REPL 與開發的坑

- **不要用 `(require 'x :reload-all)`。** 它會重新載入 Datalevin 的類別，之後出現 ClassCastException，只能重啟 nREPL。改用 `(require 'knot.xxx :reload)` 逐一重新載入，或 `(user/reset)`。
- **別名失效**（例如 `No such namespace: db`，常在改了被 require 的 ns 之後）：`(require 'user :reload)` 再 `(user/go)`；仍不行時對受影響的 ns 執行 `(remove-ns 'knot.xxx)` 再重新 require。
- **dev 報告是真實資料。** `local/report` 的資料庫可能是由參考報告節錄而來；在 REPL 試寫入後用 `curl -X POST localhost:3000/api/report/undo` 復原，測試一律用暫存目錄與 `test-resources/` 的虛構資料。
- **共用的 nREPL 與 port。** 後端 nREPL 7888、HTTP 3000、shadow 7889 只有一套，同時只能有一個人在改程式與重新載入；不要另外啟動或關閉這些行程。
- **前端的 JS 物件：** ProseMirror 的 attrs 是沒有原型的物件，`js->clj` 不會轉換，要先經過 JSON（見 `knot.prose-editor/json->edn`）。
- **commit：** 只 stage 指定的路徑；先看 `git status`，根目錄的 PDF 與 `local/` 絕不能進 commit。由使用者確認後才 commit。

## 環境備忘

- **JDK 25。** Datalevin 需要 `--add-opens` 與 `--enable-native-access`，已寫在 deps.edn 的 `:dev`、`:test`、`:run` alias。
- **anydoc 的 npm 套件是 `@firecrawl/anydoc`。** npm 上名為 `anydoc` 的是無關的套件。執行方式仍是 `node_modules/.bin/anydoc <檔案>`。
- **integrant 1.0。** 每個 key 都要有 `ig/init-key` 方法，否則會嘗試 require 同名 namespace 而失敗。
- **Datalevin 中文全文搜尋。** 預設分詞器查不到中文（M0 已實測），M5 須自訂 bigram 分析器。

## 固定版本（M0，2026-10-26）

| 套件 | 版本 |
|---|---|
| org.clojure/clojure | 1.12.6 |
| http-kit/http-kit | 2.8.1 |
| metosin/reitit-ring、reitit-middleware | 0.11.0 |
| metosin/muuntaja | 0.6.12 |
| metosin/malli | 0.20.3 |
| integrant/integrant、integrant/repl | 1.0.1、0.5.1 |
| datalevin/datalevin | 1.2.7 |
| org.commonmark/commonmark、ext-gfm-tables | 0.31.0 |
| nrepl/nrepl、cider/cider-nrepl | 1.8.0、0.63.1 |
| thheller/shadow-cljs（clj 與 npm） | 3.5.7 |
| no.cjohansen/replicant | 2026.07.1 |
| no.cjohansen/nexus | 2026.08.1 |
| clj-kondo/clj-kondo | 2026.10.20 |
| cognitect-labs/test-runner | v0.5.1 (5e91ee0) |
| @tiptap/core、@tiptap/pm | 3.32.0 |
| @firecrawl/anydoc | 0.2.4 |
| clojure-mcp-light（bbin，全域） | v0.2.2 |
