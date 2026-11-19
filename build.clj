(ns build
  "clojure -T:build uber → target/knot.jar（前端 release 需先建好 resources/public/js）"
  (:require [clojure.tools.build.api :as b]))

(def class-dir "target/classes")
(def jar-file "target/knot.jar")

(defn clean [_] (b/delete {:path "target/classes"}) (b/delete {:path jar-file}))

(def ^:private arch->platform {"amd64" "linux-x86_64" "arm64" "linux-arm64"})
(def ^:private all-platforms ["linux-x86_64" "linux-arm64" "macosx-x86_64" "macosx-arm64" "windows-x86_64"])

(defn- native-excludes
  "只留 arch（\"amd64\" 或 \"arm64\"）的 Datalevin 原生函式庫；arch 為 nil 時全部保留。"
  [arch]
  (when arch
    (let [keep (or (arch->platform (str arch)) (throw (ex-info (str "不支援的 arch：" arch) {})))]
      (for [p all-platforms :when (not= p keep)]
        (str "datalevin/dtlvnative/" p "/.*")))))

(defn uber
  "選項 :arch \"amd64\"|\"arm64\"：只含該平台的原生函式庫（Docker 使用）；省略則全部保留。"
  [{:keys [arch]}]
  (clean nil)
  (let [basis (b/create-basis {:project "deps.edn"})]
    (b/copy-dir {:src-dirs ["src/clj" "src/cljc" "resources"] :target-dir class-dir})
    (b/compile-clj {:basis basis
                    :src-dirs ["src/clj" "src/cljc"]
                    :class-dir class-dir
                    :ns-compile '[knot.main]
                    :java-opts ["--add-opens=java.base/java.nio=ALL-UNNAMED"
                                "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"
                                "--enable-native-access=ALL-UNNAMED"]})
    (b/uber {:class-dir class-dir :uber-file jar-file :basis basis :main 'knot.main
            :exclude (native-excludes arch)})))
