(ns suub.bote.util
  (:require [midje.sweet :refer :all]
            ;[me.raynes.fs :as fs]
            [clojure.java.io :as io]))

(defn drop-prefix
  "Returns the rest of the seq after the prefix elements have been removed.
   When no match can be found returns nil."
  [prefix coll]
  (loop [pre (seq prefix),
         post (seq coll)]
    (cond (empty? pre) (apply str post)
          (= (first pre) (first post)) (recur (rest pre) (rest post)))))

(tabular
 (fact (drop-prefix ?pre ?post) => ?result)
 ?pre    ?post   ?result
 "hel"   "hello" "lo"
 "hello" "hello" ""
 "ab"    "hello" nil
 "hello" "hel"   nil)

(defn filename-components
  "Separates a filename into the name and type components."
  [filename]
  (when-let [[_ name ext] (re-matches #"(.*)\.(.*)" filename)]
    [name ext]))

(tabular
 (fact (filename-components ?full) => [?name ?type])
 ?full    ?name   ?type
 "hello.txt"   "hello" "txt"
 "foo.bar" "foo" "bar"
 "hel.lo.txt" "hel.lo" "txt")

(fact (filename-components "hello") => nil)
