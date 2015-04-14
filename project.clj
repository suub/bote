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
                 [org.clojure/data.zip "0.1.1"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [net.bendlas/data.xml "1.0.0-SNAPSHOT"]
                 [me.raynes/fs "1.4.6"]
                 [com.taoensso/timbre "3.3.1"]
                 [pandect "0.3.4"]
                 [net.mikera/imagez "0.5.0"]
                 [de.undeadco/marmoset "0.1.0-SNAPSHOT"]
                 [suub/error-codes "0.2.2-SNAPSHOT"]]
  :resource-paths ["resources"]
  :plugins [[lein-gorilla "0.3.5-SNAPSHOT"]])
