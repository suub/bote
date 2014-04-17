(defproject suub/bote "0.1.0-SNAPSHOT"
  :jvm-opts ["-Xmx8g"]
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"conjars" "http://conjars.org/repo"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [org.clojure/tools.reader "0.7.10"]
                 [midje "1.5.1"]
                 [com.taoensso/timbre "2.6.2"]
                 [me.raynes/laser "2.0.0-SNAPSHOT"]
                 [pandect "0.3.0"]
                 [org.clojars.r0man/clj-spark "0.1.0-SNAPSHOT"]
                 [org.apache.spark/spark-core_2.10 "0.9.0-incubating"]
                 [org.apache.spark/spark-streaming_2.10 "0.9.0-incubating"]]
  :profiles {:dev
             {:dependencies [[org.apache.hadoop/hadoop-core "1.1.2"]
                             [cascalog/midje-cascalog "2.0.0"]]
              :plugins      [[lein-midje "3.0.1"]
                             [lein-marginalia "0.7.1"]]}})
