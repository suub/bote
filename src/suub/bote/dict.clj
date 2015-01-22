;; gorilla-repl.fileformat = 1

;; **
;;; # Dictionary error correction.
;;; 
;;; This code uses a weighted finite state transducer aproach to give all possible matches for a given word.
;; **

;; @@
(ns suub.bote.dict
  (:require [gorilla-plot.core :as plot]
            [gorilla-repl.table :as table]
            [error-codes.gorilla :as gv]
            [gorilla-repl.html :as gh]
            [clojure.core.reducers :as r]
            [clojure.core.async :as a]
            [clojure.test :as t]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [clojure.data.xml :as xml]
            [clojure.string :as string]
            [clojure.data.priority-map :as pm]
            [clojure.edn :as edn]
            [me.raynes.fs :as fs]
            [suub.bote.abbyy :as abbyy]
            [gorilla-renderable.core :as render]
            [marmoset.util :as marm]))
;; @@

;; @@
(defn read-dict [path]
  (with-open [in (io/reader path)]
    (let [words (->> (line-seq in)
                     (r/map #(string/split % #"\s+"))
                     (r/map (fn [[cnt orig simpl]] [orig (bigint cnt)])))
          word-count (r/fold + (r/map second words))]
      (->> words
           (r/map (fn [[w c]] [w (/ c word-count)]))
           (into {})))))

(defn read-substs [path]
  (let [subst (-> path
                  slurp
                  edn/read-string)]
    (into {}
          (for [[truth n] subst
                [ocr prob] n]
            [[ocr truth] prob]))))
;; @@

;; @@
(defn drop-prefix
  "Returns the rest of the seq after the prefix elements have been removed.
   When no match can be found returns nil."
  [prefix coll]
  (loop [pre (seq prefix),
         post (seq coll)]
    (cond (empty? pre) post
          (= (first pre) (first post)) (recur (rest pre) (rest post)))))
;; @@

;; @@
(defn simple-matcher [p q]
  (when-let [r (drop-prefix p q)]
    [p r]))
;; @@

;; @@
(defn word-prefixes [[w p]]
  (into {}
        (r/map (fn [n] [(subs w 0 n) p])
          (range 0 (inc (count w))))))

(defn merge-prefixes [& m]
  (apply merge-with max m))

(defn prefixes [d]
  (->> d
       (r/map word-prefixes)
       (r/reduce merge-prefixes)))
;; @@

;; **
;;; #Generating possible interpretations.
;; **

;; @@
(defn alternatives
  "Expects:
    * adapter function that makes it possible to treat complex datastructures
      such as parsed XML, as a stream of characters without destroying metainformation,
      like bounding boxes or identifiers.
      In case of a match it must return a tuple of the match and the rest,
      otherwise nil.
    * Possible character group to character group missinterpretations by the OCR.
    * A heuristic to be applied while searching, similar to the distance function in A*,
      it allows for pruning search paths by returning nil and optimised search
      by returning approximations to the final penalty of the words a prefix might lead to.
    * Calculates the penalty given to an alternative.
      This might be the frequency of the word in the dictionary or additional information like
      n-gram scores. The found word will be omitted when nil is returned.
    * A query to be matched.

    Note that the penalty score must be monotonically falling along the search path.
    For example the final penalty may never be higher than the heuristic penalty.

    Retuns the possible corrections, their probability of a match
    and a collection of their substitutions."
  [adapter substs heuristic-penalty final-penalty query]
  (letfn [(iterations [run]
            (for [[[ocr truth] prob] substs
                  :let [[from rest :as matched]
                        (adapter ocr (:rest run))]
                  :when matched]
              (-> run
                  (assoc :rest rest)
                  (update-in [:word] #(str % truth))
                  (update-in [:subst-prob] #(* % prob))
                  (update-in [:substs] #(conj %
                                              {:from from
                                               :to truth
                                               :prob prob})))))
          (worker [unfinished]
            (lazy-seq
             (loop [unfinished unfinished]
               (when (seq unfinished)
                 (let [candidate (-> unfinished peek first)]
                   (if (empty? (:rest candidate))
                     (cons candidate (worker (pop unfinished)))
                     (recur (into (pop unfinished)
                                  (for [{:keys [rest word subst-prob]
                                         :as iteration} (iterations candidate)
                                        :let [word-prob (if (empty? rest)
                                                          (final-penalty word)
                                                          (heuristic-penalty word))]
                                        :when word-prob
                                        :let [prob (* word-prob subst-prob)]]
                                        [(assoc iteration
                                           :word-prob word-prob
                                           :prob prob)
                                          prob])))))))))]
    (->> (worker (pm/priority-map-by >
                  {:rest query
                   :word ""
                   :subst-prob 1
                   :substs []}
                  1))
         (map #(dissoc % :rest)))))
;; @@

;; **
;;; #Setup & loading
;; **

;; **
;;; ## DTA Dictionary
;; **

;; @@
(def simple-subst
        (merge
          (into {}
           (for [c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZüäöß0123456789-,'¬"
                 :let [s (str c)]]
             [[s s] 1]))
          {["u" "n"] 1/4
           ["n" "u"] 1/4
           ["c" "e"] 1/4
           ["e" "c"] 1/4
           ["N" "U"] 1/8
           ["N" "R"] 1/8
           ["ö" "ß"] 1/4
           ["l" "i"] 1/4
           ["rn" "m"] 1/8
           ["iii" "m"] 1/8
           ["m" "en"] 1/8
           ["s" "f"] 1/4
           ["^" "s"] 1
           ["nt" "m"] 1/4}))
;; @@

;; @@
(def dta-dict (-> "resources/dta-freq.d/dta-core-1850+.fuw"
                  read-dict))
;; @@

;; @@
(def dta-prfx (time (prefixes dta-dict)))
;; @@

;; @@
(table/table-view
  (alternatives simple-matcher
                simple-subst
                dta-prfx
                dta-dict  
                "Hans"))
;; @@

;; **
;;; #Grenzboten Dictionary
;; **

;; @@
(def grz-dict (edn/read-string (slurp "gb-dict.edn")))
;; @@

;; @@
(def a (atom 0))
;; @@

;; @@
(def grp (->> "gb-bigr.edn"
              slurp
              edn/read-string
              (group-by ffirst)))
;; @@

;; @@
(count grp)
;; @@

;; @@
(marm/progress-view a (count grp))
;; @@

;; @@
(def grz-bigr (->>
                grp
                seq
                (r/fold
                  merge
                  (fn [acc [k v]]
                    (let [c (apply + (map second v))]
                      (assoc
                        acc
                        k
                        (into {} (for [[[l r] p] v]
                                   [r (/ p c)]))))))))
;; @@

;; @@
(spit "grz-bigrfinal.edn" (pr-str grz-bigr))
;; @@

;; **
;;; ###Bigramm example.
;; **

;; @@
((grz-bigr "der") "Hauf")
;; @@

;; @@

;; @@

;; @@
(table/table-view
  (alternatives simple-matcher
                simple-subst
                dta-prfx
                #(* (dta-dict %) (or ((grz-bigr "der") %) 0)) 
                "Hans"))
;; @@

;; @@
(table/table-view
  (alternatives simple-matcher
                simple-subst
                dta-prfx
                #(when-let [b ((grz-bigr "der") %)] (* (dta-dict %) b))
                "Hans"))
;; @@

;; **
;;; ## Gold-Potsdam Dictionary
;; **

;; @@
(def gold-subst (read-substs "resources/substitutions.edn"))
;; @@

;; @@
(def pots-dict (->> "resources/dict.edn"
                    slurp
                    edn/read-string
                    (map #(update-in % [1] bigint))
                    (into {})))
;; @@

;; @@
(def pots-prfx (time (prefixes pots-dict)))
;; @@

;; **
;;; #Deployment code
;; **

;; @@
(defn correct [p idx]
  (abbyy/change
    (->> p
      abbyy/lines
      abbyy/remove-linewrap
      (map #(first (alternatives idx %)))
      (remove nil?)
      (mapcat :substs))
    p))
;; @@

;; @@
(defn text [p]
  (apply str (map (fn [l]
                    (str (apply str
                                (map :char l))
                         "\n"))
                  (abbyy/lines p))))
;; @@

;; @@
(defn download-xml [vlid]
  (xml/parse-str-raw
    (slurp (str "http://brema.suub.uni-bremen.de/grenzboten/download/fulltext/fr/" vlid))
    {:preserve-whitespace true}))
;; @@

;; @@
(def pages
  (into {}
    (->> "resources/ground-truth"
         io/file
         file-seq
         (filter fs/file?)
         (map #(vector (fs/base-name % true)
                       {:truth (slurp %)
                        :raw (download-xml (fs/base-name % true))})))))
;; @@

;; @@

(apply str (take 1000 (xml/emit-str (:raw (first (vals pages))))))

;; @@

;; @@
(time (doall
  (pmap #(spit (str "corrected/" (first %) ".xml")
               (xml/emit-str-raw (correct (second %) dta)))
        x)))
;; @@

;; **
;;; #Difficult words
;; **

;; @@
(def difficult (filter #(not= % (->> %
                                    (transform dta)
                                    first
                                    :word))
                       (keys (:dict dta))))
;; @@

;; @@
(table/table-view (take 20 (filter second
                                   (map (fn [w] (let [r (take 10 (transform dta w))]
                                          [w
                                           (:word (first r))
                                           (plot/bar-chart
                                             (map :word r)
                                             (map :prob r)
                                             :plot-size 600)]))
                                difficult))))
;; @@

;; @@
(future (doseq [i (range 10000)]
          (swap! a inc)
          (Thread/sleep 1)))
;; @@

;; @@
(def f (future
  (time (dorun
  (pmap #(do
           (swap! a inc)
           (spit (str "/Users/ticking/Desktop/test" (first %) ".xml")
               (with-out-str (xml/emit (correct (xml/parse (second %)) dta)))))
        files)))))
;; @@

;; @@
(def a (atom 0))
;; @@

;; @@
(reset! a 0)
;; @@

;; @@
(progress-view a (count (files "/Users/ticking/Desktop/vls-ro")))
;; @@

;; @@
(def di (->> "/Users/ticking/Desktop/vls-ro/"
             abbyy/files
             (pmap #(->> %
                         (do (swap! a inc))
                         second
                         xml/parse
                         abbyy/lines
                         abbyy/remove-linewrap
                         (mapv abbyy/text)))
             (apply concat)
             frequencies
             future))
;; @@

;; @@
(def bigrams (->> "/Users/ticking/Desktop/vls-ro/"
             abbyy/files
             (pmap #(->> %
                         (do (swap! a inc))
                         second
                         xml/parse
                         abbyy/lines
                         abbyy/remove-linewrap
                         (mapv abbyy/text)))
             (apply concat)
             (partition 2 1)
             frequencies
             future))
;; @@

;; @@
(apply max-key second @di)
;; @@

;; @@
(apply max-key second @bigrams)
;; @@
