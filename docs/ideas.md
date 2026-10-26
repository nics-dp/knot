# Knot 想法與設計緣由

本檔收錄設計緣由與尚未納入規格的構想，不是需求清單；定案的內容寫進 `docs/SPEC.md`。

> 先前在 Claude Docs 的討論文件尚未匯出進 repo；匯出後放在本檔前面。

---

## 讓 agent 透過 MCP 使用 Knot（構想，2026-10-26）

### 想解決什麼

整理素材與搭卡片骨架很花時間，但這些工作很適合交給 agent：讀完一份 PDF 後列出章節骨架、為每段寫下意圖、從素材中找出依據並連結。若 agent 只能透過 HTTP API 操作，就得替每個 agent 寫一段串接程式，還要讓它理解 `knot.sync/diff` 的修改格式。MCP（Model Context Protocol）讓 agent 直接看到一組有說明、有參數 schema 的工具：Claude Code、Claude Desktop 等用戶端一行設定就能連上，不必寫串接程式。

目標情境：

- 「讀 `local/sources/輔導報告.md`，依第三章的結構建立節與段落卡片，每段寫上意圖」
- 「替 3.1 底下每個段落從素材中找依據並加入」
- 「列出還沒有依據的段落」「把 2.3 的段落關係都標上」
- 之後（M3 完成後）：「匯入這個 xlsx，把表 15 的保留筆數設成事實」

### 原則

1. **agent 是另一位使用者，不是後門。** 所有寫入走與畫面相同的路徑：同樣的合法性檢查（`tree/accepts?`、malli）、同樣寫成修改組。agent 做的事可以在畫面上看到、用 Ctrl／⌘+Z 復原，也會出現在「與前一版比較」中。
2. **工具以「使用者會做的事」為單位，不是資料表的 CRUD。** 例如「在 3.1 底下、某段之後新增段落並寫上意圖」，而不是「新增一個 node 實體並計算 order-key」。編號、分數索引、關係的待確認標記都由 Knot 處理。
3. **agent 的產出和 AI 生成一樣需要人確認。** agent 匯入或切出的素材區塊一律是候選；agent 寫的本文狀態為 AI 生成；鎖定的段落一律拒絕修改。
4. **只在本機。** MCP 端點只綁 localhost，並檢查 `Origin` 標頭（防止網頁透過 DNS rebinding 呼叫本機服務）。

### 架構選擇

| 方案 | 做法 | 優點 | 缺點 |
|---|---|---|---|
| **A. 內建於 Knot 後端（建議）** | 後端多一個 `/mcp` 端點，使用 MCP 的 Streamable HTTP 傳輸；工具直接呼叫 `knot.db`、`knot.tree`、`knot.context` | 與畫面共用同一個資料庫連線與驗證；不需另一個行程 | Knot 必須在執行中；要自己實作 JSON-RPC（訊息不多：initialize、tools/list、tools/call） |
| B. 獨立的 stdio 轉接程式 | 用 babashka 寫一個 stdio MCP server，轉呼叫 Knot 的 HTTP API | 用戶端設定最普遍（stdio） | 多一層轉換；HTTP API 是給前端用的整包修改格式，不適合直接當工具 |
| C. 獨立行程直接開資料庫 | MCP server 自己開 `.knot/db` | 不需 Knot 執行中 | Datalevin（LMDB）由兩個行程同時寫入有風險，也會繞過畫面的同步 |

建議方案 A。Claude Code 的設定會是：

```bash
claude mcp add --transport http knot http://localhost:3000/mcp
```

JSON 處理可用 muuntaja 已帶入的 jsonista，預期不需要新增相依套件（實作時再確認）。

### 工具草案

**讀取**

| 工具 | 說明 |
|---|---|
| `get_outline` | 卡片樹：每張卡片的 id、類型、編號、名稱、意圖摘要、是否已有本文 |
| `get_card` | 單張卡片：意圖、依據、關係（含位置）、本文純文字、下層卡片 |
| `get_generation_context` | 生成這段時 AI 會參考的內容（直接用 `knot.context/generation-context`，與編輯區預覽相同） |
| `search_sources` | 搜尋素材區塊（M3 的中文全文搜尋），可依類型、狀態、來源檔篩選 |
| `get_source_block` | 單一區塊的完整內容或表格 |
| `find_issues` | 現成可查的問題：沒有依據的段落、引用未確認素材的段落、未標註或待確認的關係（M7 交接檢查的前身） |

**卡片**

| 工具 | 說明 |
|---|---|
| `add_heading`、`add_paragraph` | 指定 parent 與「接在哪張之後」，附名稱與意圖；不合法的位置回傳原因 |
| `build_outline` | 一次建立整段巢狀骨架（標題、段落、意圖），寫成一個修改組 |
| `update_card` | 改名稱、意圖；本文另用 `write_prose` |
| `move_card`、`delete_card` | 與畫面上的拖曳、刪除同規則（同層移動、刪除含下層） |
| `set_relation`、`add_remote_relation`、`remove_remote_relation` | 相鄰關係類型、不相鄰關係 |
| `add_citation`、`remove_citation` | 把素材區塊加入段落的依據 |
| `write_prose` | 寫入本文，引用沿用 5.4 的標記（`{{fact:key}}`、`{{ref:label}}`），由 Knot 轉成本文格式並驗證 |

**素材（M3 之後）**

| 工具 | 說明 |
|---|---|
| `import_source` | 匯入檔案（anydoc 轉換後切成候選區塊） |
| `import_markdown` | 匯入 agent 整理好的 Markdown（例如它自己從 PDF 讀出、修正過標題層級的版本） |
| `split_block`、`merge_blocks` | 調整候選區塊 |
| `create_fact` | 在表格儲存格上建立事實 |

確認素材（候選 → 已確認）是否開放給 agent，待決定（見下方問題 2）。

每個寫入工具都接受一個 `summary` 參數，成為修改組的名稱（例如「agent：建立第三章骨架」），讓畫面上的復原提示看得懂是誰做了什麼。修改組的來源記為新的 `:agent`。

### 必須先解決：畫面與 agent 同時修改

現在前端只在開頁時讀一次資料，之後把「自己的狀態與上次儲存的差異」整筆送出。如果 agent 在背景改了某張卡片，而使用者在畫面上又改了同一張卡片，前端送出的整筆資料會把 agent 的修改蓋回去。因此 MCP 的寫入工具上線前，同步機制要先補上兩件事：

1. **通知畫面重新載入。** 後端提供事件串流（Server-Sent Events，例如 `GET /api/events`）。任何修改組寫入後送出通知，前端在使用者沒有輸入時以 `ui/replace-data` 更新畫面（與復原後的做法相同）；正在輸入時先保留，送出後再更新。
2. **衝突檢查。** 前端送出修改時附上卡片的 `:node/version`；後端發現版本已被其他來源改過時拒絕該筆，前端重新載入並提示使用者。

這兩件事對多分頁同時開啟的情況也有幫助。

### 對其他規劃的影響

- **M4 生成可以有第二條路。** 有了 `get_generation_context` 與 `write_prose`，agent 本身就能生成本文：Knot 提供脈絡與驗證，agent 用自己的模型寫。這樣 Knot 不必保存 API 金鑰，也能先試用生成流程。內建的 `knot.llm` 仍有價值（一鍵生成、M5 的修改建議需要在 Knot 內呼叫），兩者可以並存。
- **資料外流。** agent 讀到的素材會送到 agent 所用的模型服務。素材若含不宜外送的內容，要由使用者自行決定是否讓 agent 讀取；可考慮讓素材標記「不提供給 agent」。

### 建議的分階段

1. **同步機制**：事件串流與版本檢查（不論是否做 MCP 都值得做）。
2. **MCP 第一階段**：讀取工具與卡片工具。現在已有卡片與關係，立即可用（例如請 agent 依參考文件搭骨架、寫意圖）。
3. **MCP 第二階段**：素材工具，接在 M3 之後。
4. **MCP 第三階段**：`write_prose`，與 M4 一起規劃。

### 待討論

1. MCP 是否納入範圍？目前 SPEC 第 1 節沒有列；納入的話要排在 M3 之前、之後，還是與 M3 平行？
2. agent 可以直接確認素材嗎？還是只能產生候選、一律由使用者確認？
3. 刪除類工具（刪卡片、移除依據）要不要開放，或只開放新增與修改？
4. 生成本文要走 agent（不需金鑰）、內建 `knot.llm`，還是兩者都要？
