(ns knot.main-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.main :as main]
            [knot.system :as system]))

(deftest env-overrides-test
  (testing "沒設就不覆寫"
    (is (= {} (main/env-overrides {})))
    (is (= {} (main/env-overrides {"KNOT_HOST" ""}))))
  (testing "KNOT_HOST 覆寫 :http/host"
    (is (= {:http/host "0.0.0.0"} (main/env-overrides {"KNOT_HOST" "0.0.0.0"})))))

(deftest no-browser-test
  (is (false? (main/no-browser? {})))
  (is (true? (main/no-browser? {"KNOT_NO_BROWSER" "1"}))))

(deftest system-host-test
  (testing "預設只綁 127.0.0.1"
    (is (= "127.0.0.1" (get-in (system/config) [:knot/config :http/host]))))
  (testing "覆寫會合併進設定"
    (let [cfg (system/config (main/env-overrides {"KNOT_HOST" "0.0.0.0"}))]
      (is (= "0.0.0.0" (get-in cfg [:knot/config :http/host])))
      (is (= 3000 (get-in cfg [:knot/config :http/port]))))))
