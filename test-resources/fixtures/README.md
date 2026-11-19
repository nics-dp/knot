# 測試用虛構資料

本目錄內容全部虛構，不得放入任何真實政府專案素材（SPEC 第 8 節）。

範例情境：**OO 市智慧路燈示範計畫**結案技術報告。

| 路徑 | 內容 | 用於 |
|---|---|---|
| `templates/closing-report.edn` | 結案報告範本骨架：四層以內的章節樹 | 5.1、M1 `(seed!)` |
| `sources/streetlight-final.md` | 含標題、段落、GFM 表格與 `<!-- page: N -->` 頁碼標記的素材 | 5.2 |
| `agent/` | 模擬 agent 交回的內容（本文、修改建議、整理過的 Markdown），檔名為 `<工具>/<情境>.edn` | M3 起 |

之後依里程碑補上：anydoc 轉換用的 docx、xlsx、pdf（M5），以及埋好各類問題的交接檢查報告（M9）。
