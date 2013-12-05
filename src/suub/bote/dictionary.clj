(ns suub.bote.dictionary
  (:require [midje.sweet :refer :all]
            [midje.cascalog :refer :all]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [clojure.string :as string]
            [cascalog.api :refer :all]
            [cascalog.more-taps :refer :all]
            [cascalog.logic.ops :as o]))

(defaggregatefn condense-transformations
  ([] {})
  ([state to from prob] (log/debug to) (merge-with merge state {to {from prob}}))
  ([state] [state]))

(fact (<- [?trans]
          ([["u" "u" 1]
            ["u" "n" 0.9]
            ["a" "a" 1]]
             ?to ?from ?prob)
          (condense-transformations ?to ?from ?prob :> ?trans))
      =>
      (produces [[{"u" {"u" 1
                        "n" 0.9}
                   "a" {"a" 1}}]]))

(defn fuw-dictionary [path]
  (let [src (hfs-delimited path :classes [Integer String String])
        total (??<- [?sum]
                    (src ?count _ _)
                    (o/sum ?count :> ?sum))]
    (<- [?word ?unicruft ?prob]
        (src ?count ?word ?unicruft)
        (div ?count total :> ?prob))))

(fact (fuw-dictionary ...path...)
      =>
      (produces [["das" "das"  0.5]
                 ["ſey" "sey" 0.3]
                 ["vermeiden" "vermeiden" 0.2]])
      (provided (hfs-delimited ...path... :classes [Integer String String])
                =>
                [[5 "das" "das"]
                 [3 "ſey" "sey"]
                 [2 "vermeiden" "vermeiden"]]))

(defn- prefix-rest
  "Returns the rest of the seq after the prefix elements have been removed.
   When no match can be found returns nil."
  [prefix coll]
  (loop [pre (seq prefix),
         post (seq coll)]
    (cond (empty? pre) (apply str post)
          (= (first pre) (first post)) (recur (rest pre) (rest post)))))

(tabular
 (fact (prefix-rest ?pre ?post) => ?result)
 ?pre    ?post   ?result
 "hel"   "hello" "lo"
 "hello" "hello" ""
 "ab"    "hello" nil
 "hello" "hel"   nil)

(defn- index* [transformations dictionary]
  (letfn [(index-worker [rests]
            (for [[to _] transformations
                  :let [matching (for [[word prob] rests
                                       :let [rst (prefix-rest to word)]
                                       :when rst]
                                   [rst prob])]
                  :when (not-empty matching)]
              {:token to
               :word-prob (some (fn [[word prob]]
                                  (when (empty? word) prob))
                                matching)
               :next (index-worker matching)}))]
    {:transformations transformations
     :index (index-worker dictionary)}))

(fact (index* {"u" {"n" 1}}
              #{["uu" ...prob-1...] ["nn" ...prob-2...]})
      =>
      {:transformations {"u" {"n" 1}}
       :index [{:token "u"
                :word-prob nil
                :next [{:token "u"
                        :word-prob ...prob-1...
                        :next []}]}]})

(defn index [transformations]
  (bufferfn [words]
            [(index* (map first words))]))

(defn- lookup*
  "Expects a matching function that takes a string and a seq of the matched elements
    and returns a tuple of the match and the rest,
    an index and a query to be matched.
    Retuns the possible replacements and their subst-probability."
  [matcher {:keys [transformations index next]} query]
  (letfn [(lookup-worker [sub-index sub-query]
            (for [{to :token, word-prob :word-prob, next :next} sub-index
                  [subst subst-prob] (transformations to)
                  :let [[from rest :as matched] (matcher subst sub-query)]
                  :when matched
                  [word word-prob
                   subst-prob-sum walk] (or (seq (lookup-worker next rest))
                                            (when (and (empty? rest) subst-prob)
                                              [["" word-prob 1 nil]]))]
              [(str to word)
               word-prob
               (* subst-prob-sum subst-prob)
               (cons {:from from :to to :subst-prob subst-prob} walk)]))]
    (lookup-worker index query)))

(fact (lookup*
       (fn [p q] (when-let [rest (prefix-rest p q)] [p rest]))
       {:transformations {"n" {"n" 1
                               "u" 0.9}
                          "u" {"u" 1
                               "n" 0.9}}
        :index [{:token "n"
                 :word-prob nil
                 :next [{:token "u"
                         :word-prob ...prob...
                         :next []}]}]}
       "uu")
      =>
      [["nu" ...prob... 0.9 [{:from "u" :to "n" :subst-prob 0.9}
                             {:from "u" :to "u" :subst-prob 1}]]])

(defn lookup [matcher]
  (mapcatfn [index word]
            (lookup* matcher index word)))

(defn abbyy-matcher [m q]
  nil)

(fact (abbyy-matcher "he"[{} {} {} {}]))
