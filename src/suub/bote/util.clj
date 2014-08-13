(ns suub.bote.util
  (:require ;[me.raynes.fs :as fs]
            [clojure.java.io :as io]))

(defn drop-prefix
  "Returns the rest of the seq after the prefix elements have been removed.
   When no match can be found returns nil."
  [prefix coll]
  (loop [pre (seq prefix),
         post (seq coll)]
    (cond (empty? pre) (apply str post)
          (= (first pre) (first post)) (recur (rest pre) (rest post)))))

(defn filename-components
  "Separates a filename into the name and type components."
  [filename]
  (when-let [[_ name ext] (re-matches #"(.*)\.(.*)" filename)]
    [name ext]))

