(defproject suub/bote "0.1.0-SNAPSHOT"
  :jvm-opts ["-Xmx64g"]
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"conjars" "http://conjars.org/repo"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.reader "0.8.8"]
                 [org.clojure/data.csv "0.1.2"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [me.raynes/fs "1.4.6"]
                 [com.taoensso/timbre "3.3.1"]
                 [me.raynes/laser "1.1.1"]
                 [pandect "0.3.4"]
                 [de.undeadco/error-codes "0.3.0-SNAPSHOT"]
                 [instaparse "1.3.4"]]
  :resource-paths ["resources"]
  :plugins [[lein-gorilla "0.3.4-SNAPSHOT"]])
