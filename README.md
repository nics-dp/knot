# Knot

報告撰寫工具（本機執行，Agent 透過 MCP 協作）。開發規格見 `docs/SPEC.md`。

## 以 Docker 執行

```sh
docker build -t knot .
cd <報告目錄>
docker run --rm -p 127.0.0.1:3000:3000 -v "$PWD":/report knot
```

用瀏覽器開啟 http://localhost:3000。

- `-p 127.0.0.1:3000:3000`：服務只對本機開放。容器內綁 `0.0.0.0` 才能被轉發，所以務必加上 `127.0.0.1:`，否則同網段的人都連得到。
- 資料庫在 `<報告目錄>/.knot/db`。要讓檔案屬於自己而非 root：加 `--user $(id -u):$(id -g)`。
- 讓 Claude Code 連線：

  ```sh
  claude mcp add --scope user --transport http knot http://localhost:3000/mcp
  ```

## 不用 Docker

`bb start`（= `clojure -M:run [目錄]`），開發見 `CLAUDE.md`。環境變數：`KNOT_HOST`（綁定位址，預設 127.0.0.1）、`KNOT_NO_BROWSER`（有值就不自動開瀏覽器）。
