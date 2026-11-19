# syntax=docker/dockerfile:1

FROM node:22.12.0-bookworm-slim AS node

# ---- 建置：前端 release + uberjar + 自訂 JRE ----
FROM clojure:temurin-25-tools-deps-bookworm AS build
ARG TARGETARCH
COPY --from=node /usr/local/ /usr/local/
WORKDIR /build

# 相依先下載（利用 layer cache）
COPY deps.edn package.json package-lock.json .npmrc ./
RUN npm ci && clojure -P -M:cljs && clojure -P -T:build && clojure -P -M:run

COPY shadow-cljs.edn build.clj ./
COPY src ./src
COPY resources ./resources
RUN npx shadow-cljs release app \
 && clojure -T:build uber :arch "\"${TARGETARCH}\""

# jlink 自訂 runtime：jdeps 找出的模組，加上 jdeps 抓不到、執行期才用到的（缺模組只在執行期出錯，改動後務必實際操作驗證）
RUN apt-get update && apt-get install -y --no-install-recommends binutils && rm -rf /var/lib/apt/lists/* \
 && mods="$(jdeps --ignore-missing-deps --multi-release 25 --print-module-deps target/knot.jar),java.logging,java.xml,java.sql,java.management,java.naming,java.net.http,java.prefs,java.instrument,java.compiler,java.security.sasl,java.security.jgss,jdk.unsupported,jdk.management,jdk.crypto.ec,jdk.zipfs,jdk.net,jdk.naming.dns" \
 && echo "jlink modules: $mods" \
 && jlink --add-modules "$mods" --strip-debug --no-man-pages --no-header-files --compress=zip-6 --output /jre

# ---- 執行（glibc 2.38 以上；Datalevin 原生函式庫需要 libmvec，Debian 12 的 arm64 沒有） ----
FROM ubuntu:24.04
COPY --from=build /jre /opt/jre
COPY --from=build /build/target/knot.jar /app/knot.jar
ENV KNOT_HOST=0.0.0.0 KNOT_NO_BROWSER=1
WORKDIR /report
EXPOSE 3000
VOLUME /report
ENTRYPOINT ["/opt/jre/bin/java", \
  "--add-opens=java.base/java.nio=ALL-UNNAMED", \
  "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED", \
  "--enable-native-access=ALL-UNNAMED", \
  "-jar", "/app/knot.jar", "/report"]
