(defproject sicp-clojure "0.1.0"
  :description "The is no explanation..."
  :url "http://127.0.0.1:3000"
  :license {
    :name "The MIT License (MIT)"
    :url "http://opensource.org/licenses/MIT"
  }
  :dependencies [
    [org.clojure/tools.trace        "0.7.5"]
    [org.clojure/clojure            "1.5.1"]
    [org.clojure/tools.logging      "0.2.6"]
    [org.clojure/math.combinatorics "0.0.7"]
  ]
  :jvm-opts [
    "-Xms128m" "-Xmx256m" "-server" "-XX:MaxPermSize=64m"
    "-XX:+UseConcMarkSweepGC" "-XX:ParallelGCThreads=4"
    "-XX:+UseParNewGC" "-XX:+AggressiveOpts"
  ]
  :aot :all
  :main sicp-clojure.core)
