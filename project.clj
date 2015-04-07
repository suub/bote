(defproject suub/bote "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"conjars" "http://conjars.org/repo"
                 "myGrid Repository" "http://www.mygrid.org.uk/maven/repository"}
  :jvm-opts ["-Xmx6g"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.reader "0.8.8"]
                 [org.clojure/data.csv "0.1.2"]
                 [org.clojure/data.zip "0.1.1"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [net.mikera/imagez "0.5.0"]
                 [net.java.dev.jai-imageio/jai-imageio-core-standalone "1.2-pre-dr-b04-2011-07-04"]
                 [me.raynes/fs "1.4.6"]
                 [com.taoensso/timbre "3.3.1"]
                 [pandect "0.3.4"]
                 [instaparse "1.3.3"]
                 [net.mikera/imagez "0.5.0"]
                 [de.undeadco/marmoset "0.1.0-SNAPSHOT"]
                 [suub/error-codes "0.2.2-SNAPSHOT"]
                 [org.clojure/tools.nrepl "0.2.7"]
                 [suub/laser-experiments "0.1.0-SNAPSHOT"]]
  :resource-paths ["resources"]
  :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]
            [lein-gorilla "0.3.4-SNAPSHOT"]])
