(ns suub.bote.dictionary
  (:require [suub.bote.util :as util]
            [midje.sweet :refer :all]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [clojure.string :as string]))

(defn builtin-dict []
  (-> "dictionary.edn"
      clojure.java.io/resource
      slurp
      edn/read-string))

(defn index
  "Takes a transformation map and a collection of words,
  and returns a lazily computed index for using with `lookup`."
  [transformations words]
  (letfn [(index-worker [rests]
            (for [[to _] transformations
                  :let [matching (for [[word prob] rests
                                       :let [rst (util/drop-prefix to word)]
                                       :when rst]
                                   [rst prob])]
                  :when (not-empty matching)]
              {:token to
               :word-prob (some (fn [[word prob]]
                                  (when (empty? word) prob))
                                matching)
               :next (index-worker matching)}))]
    {:transformations transformations
     :index (index-worker words)}))

(fact (index {"u" {"n" 1}}
              #{["uu" ...prob-1...] ["nn" ...prob-2...]})
      =>
      {:transformations {"u" {"n" 1}}
       :index [{:token "u"
                :word-prob nil
                :next [{:token "u"
                        :word-prob ...prob-1...
                        :next []}]}]})

(defn lookup
  "Expects:
    * matching function that takes a collection of elements that
      are expected next by the index and the remainder of the query.
      In case of a match it must return a tuple of the match and the rest,
      otherwise nil.
    * An index to be matched against.
    * A query to be matched.
    Retuns the possible corrections, their probability of a match
    and a collection of their substitutions."
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

(fact (lookup
       (fn [p q] (when-let [rest (util/drop-prefix p q)] [p rest]))
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

