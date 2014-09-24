(ns suub.bote.util
  (:require ;[me.raynes.fs :as fs]
            [clojure.java.io :as io]))

(defn filename-components
  "Separates a filename into the name and type components."
  [filename]
  (when-let [[_ name ext] (re-matches #"(.*)\.(.*)" filename)]
    [name ext]))
