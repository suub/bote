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
                 [org.clojure/data.csv "0.1.2"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [com.taoensso/timbre "2.6.2"]
                 [me.raynes/laser "2.0.0-SNAPSHOT"]
                 [pandect "0.3.0"]
                 [suub/error-codes "0.2.0-SNAPSHOT"]
                 [instaparse "1.3.3"]]
  :resource-paths ["resources"]
  :plugins [[lein-gorilla "0.3.3"]])
