(ns suub.bote.dictionary
  (:require [clojure.core.reducers :as r]
            [suub.bote.util :as util]
            [suub.bote.abbyy :as abbyy]
            [clojure.test :as t]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [clojure.string :as string]
            [instaparse.core :as insta]
            [instaparse.combinators :as instac]))

(defn builtin-dict []
  (-> "dictionary.edn"
      clojure.java.io/resource
      slurp
      edn/read-string))

(defn builtin-substs []
  (-> "substitutions.edn"
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

(t/is (= (index {"u" {"n" 1}}
                {"uu" 'prob "nn" 'prob2})
         {:transformations {"u" {"n" 1}}
          :index [{:token "u"
                   :word-prob nil
                   :next [{:token "u"
                           :word-prob 'prob
                           :next []}]}]}))

(def ni (atom 0))

(defn tokenizer [matches]
  (let [tokenizer (->> matches
                       (map instac/string)
                       (apply instac/alt)
                       (instac/plus)
                       (vector :S)
                       (insta/parser))]
    (fn [[word prob]]
      (swap! ni inc)
      (->> word
           (insta/parses tokenizer)
           (r/map #(insta/transform {:S vector} %))
           (r/map (fn [x] [x prob]))
           (into [])))))

(defn fast-index [transformations words]
  (let [t (tokenizer (keys transformations))
        idx (->> words
                 (r/mapcat t)
                 (r/reduce (fn [a [w p]] (assoc-in a (conj w :t) p)) {}))]
    {:transformations transformations
     :index idx}))

(t/is (= (fast-index {"u" {"n" 1}}
                     {"uu" 'prob "nn" 'prob2})
         {:transformations {"u" {"n" 1}}
          :index {"u" {"u" {:t 'prob}}}}))

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
                  match (or (seq (lookup-worker next rest))
                            (when (and (empty? rest) word-prob)
                              [{:word ""
                                :word-prob word-prob
                                :subst-prob 1
                                :substs nil}]))]
              (-> match
                  (update-in [:word] #(str to %))
                  (update-in [:subst-prob] #(* % subst-prob))
                  (update-in [:substs] #(cons {:from from :to to :prob subst-prob} %)))))]
    (map #(assoc % :prob
                 (* (:word-prob %)
                    (:subst-prob %)))
         (lookup-worker index query))))

(t/is (= (lookup
          (fn [p q] (when-let [rest (util/drop-prefix p q)] [p rest]))
          {:transformations {"n" {"n" 1/2
                                  "u" 1/2}
                             "u" {"u" 1/2
                                  "n" 1/2}}
           :index [{:token "n"
                    :word-prob nil
                    :next [{:token "u"
                            :word-prob 1/3
                            :next []}]}]}
          "uu")
         [{:word "nu"
           :prob 1/12
           :word-prob 1/3
           :subst-prob 1/4
           :substs [{:from "u" :to "n" :prob 1/2}
                    {:from "u" :to "u" :prob 1/2}]}]))

(defn fast-lookup
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
            (for [[to next] sub-index
                  [subst subst-prob] (transformations to)
                  :let [word-prob (:t next)
                        [from rest :as matched] (matcher subst sub-query)]
                  :when matched
                  match (or (seq (lookup-worker next rest))
                            (when (and (empty? rest) word-prob)
                              [{:word ""
                                :word-prob word-prob
                                :subst-prob 1
                                :substs nil}]))]
              (-> match
                  (update-in [:word] #(str to %))
                  (update-in [:subst-prob] #(* % subst-prob))
                  (update-in [:substs] #(cons {:from from :to to :prob subst-prob} %)))))]
    (map #(assoc % :prob
                 (* (:word-prob %)
                    (:subst-prob %)))
         (lookup-worker index query))))

(t/is (= (fast-lookup
          (fn [p q] (when-let [rest (util/drop-prefix p q)] [p rest]))
          {:transformations {"n" {"n" 1/2
                                  "u" 1/2}
                             "u" {"u" 1/2
                                  "n" 1/2}}
           :index {"n" {"u" {:t 1/3}}}}
          "uu")
         [{:word "nu"
           :prob 1/12
           :word-prob 1/3
           :subst-prob 1/4
           :substs [{:from "u" :to "n" :prob 1/2}
                    {:from "u" :to "u" :prob 1/2}]}]))

(def dict (builtin-dict))
(def subst (builtin-substs))
(def idx (future (fast-index subst dict)))
(def l (lookup (fn [p q] (when-let [rest (util/drop-prefix p q)] [p rest]))
               idx
               "Fig."))

(defn difficult-word [idx]
  (fn [w]
    (if-let [l (not-empty
                (sort-by :prob >
                         (lookup (fn [p q] (when-let [rest (util/drop-prefix p q)] [p rest]))
                                 idx
                                 w)))]
      (when (not= w (:word (first l)))
        [w
         (first (filter #(= w (:word %)) l))
         (take 5 l)])
      [w nil])))

(def difficult-words (remove nil? (map (difficult-word idx) (map first dict))))


(defn set-interval [callback ms]
  (let [f (future (while true (do (Thread/sleep ms) (callback))))]
    (fn [] (future-cancel f))))
