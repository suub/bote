(defproject suub/bote "0.1.0-SNAPSHOT"
  :jvm-opts ["-Xmx4g"]
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
                 [cascalog/cascalog-core "2.0.0"]
                 [cascalog/cascalog-more-taps "2.0.1-SNAPSHOT"]
                 ;[cascalog/cascalog-checkpoint "0.2.0"]
                 [me.raynes/laser "2.0.0-SNAPSHOT"]
                 [pandect "0.3.0"]]
  :profiles {:dev
             {:dependencies [[org.apache.hadoop/hadoop-core "1.1.2"]
                             [cascalog/midje-cascalog "2.0.0"]]
              :plugins      [[lein-midje "3.0.1"]
                             [lein-marginalia "0.7.1"]]}})
