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
            [gorilla-renderable.core :as render]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

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
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/read-substs</span>","value":"#'suub.bote.dict/read-substs"}
;; <=

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
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/drop-prefix</span>","value":"#'suub.bote.dict/drop-prefix"}
;; <=

;; @@
(defn simple-matcher [p q]
  (when-let [r (drop-prefix p q)]
    [p r]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/simple-matcher</span>","value":"#'suub.bote.dict/simple-matcher"}
;; <=

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
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/prefixes</span>","value":"#'suub.bote.dict/prefixes"}
;; <=

;; **
;;; #Generating possible interpretations.
;; **

;; @@
(defn transform
  "Expects:
    * matching function that takes a collection of elements that
      are expected next by the transformations and the remainder of the query.
      In case of a match it must return a tuple of the match and the rest,
      otherwise nil.
    * The transformations to be performed.
    * A query to be matched.
    Retuns the possible corrections, their probability of a match
    and a collection of their substitutions."
  [{:keys [matcher dict prefixes substs]} query]
  (letfn [(iterations [run]
            (for [[[ocr truth] prob] substs
                  :let [[from rest :as matched]
                        (matcher ocr (:rest run))]
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
                                        :let [prefix-prob (prefixes word)
                                              dict-prob (dict word)
                                              word-prob (if (empty? rest)
                                                          dict-prob
                                                          prefix-prob)]
                                        :when word-prob
                                        :let [prob (* word-prob subst-prob)]]
                                        [(assoc iteration
                                           :word-prob word-prob
                                           :prob prob)
                                         (- prob)])))))))))]
    (->> (worker (pm/priority-map
                  {:rest query
                   :word ""
                   :subst-prob 1
                   :substs []}
                  1))
         (map #(dissoc % :rest)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/transform</span>","value":"#'suub.bote.dict/transform"}
;; <=

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
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/simple-subst</span>","value":"#'suub.bote.dict/simple-subst"}
;; <=

;; @@
(def dta-dict (-> "resources/dta-freq.d/dta-core-1850+.fuw"
                  read-dict))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta-dict</span>","value":"#'suub.bote.dict/dta-dict"}
;; <=

;; @@
(def dta-prfx (time (prefixes dta-dict)))
;; @@
;; ->
;;; &quot;Elapsed time: 32556.694 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta-prfx</span>","value":"#'suub.bote.dict/dta-prfx"}
;; <=

;; @@
(def dta  {:matcher abbyy/matcher
           :dict dta-dict
           :prefixes dta-prfx
           :substs simple-subst})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta</span>","value":"#'suub.bote.dict/dta"}
;; <=

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

;; @@
(def pots {:matcher abbyy/matcher
           :dict pots-dict
           :prefixes pots-prfx
           :substs simple-subst})
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
      (map #(first (transform idx %)))
      (remove nil?)
      (mapcat :substs))
    p))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/correct</span>","value":"#'suub.bote.dict/correct"}
;; <=

;; @@
(defn text [p]
  (apply str (map (fn [l]
                    (str (apply str
                                (map :char l))
                         "\n"))
                  (abbyy/lines p))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/text</span>","value":"#'suub.bote.dict/text"}
;; <=

;; @@
(defn download-xml [vlid]
  (xml/parse-str
    (slurp (str "http://brema.suub.uni-bremen.de/grenzboten/download/fulltext/fr/" vlid))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/download-xml</span>","value":"#'suub.bote.dict/download-xml"}
;; <=

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
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/pages</span>","value":"#'suub.bote.dict/pages"}
;; <=

;; @@
(keys pages)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(&quot;179460&quot; &quot;179432&quot; &quot;179444&quot; &quot;179419&quot; &quot;179530&quot; &quot;179428&quot; &quot;179511&quot; &quot;179512&quot; &quot;179543&quot; &quot;179394&quot; &quot;179477&quot; &quot;179450&quot; &quot;179453&quot; &quot;179549&quot; &quot;179539&quot; &quot;179582&quot; &quot;179572&quot; &quot;179550&quot; &quot;179580&quot; &quot;179396&quot; &quot;179457&quot; &quot;179470&quot; &quot;179542&quot; &quot;179547&quot; &quot;179537&quot; &quot;179516&quot; &quot;179536&quot; &quot;179573&quot; &quot;179434&quot; &quot;179420&quot; &quot;179439&quot; &quot;179507&quot; &quot;179592&quot; &quot;179556&quot; &quot;179524&quot; &quot;179563&quot; &quot;179493&quot; &quot;179481&quot; &quot;179570&quot; &quot;179433&quot; &quot;179506&quot; &quot;179469&quot; &quot;179417&quot; &quot;179404&quot; &quot;179534&quot; &quot;179464&quot; &quot;179468&quot; &quot;179562&quot; &quot;179504&quot; &quot;179414&quot; &quot;179584&quot; &quot;179400&quot; &quot;179495&quot; &quot;179459&quot; &quot;179487&quot; &quot;179576&quot; &quot;179449&quot; &quot;179488&quot; &quot;179564&quot; &quot;179456&quot; &quot;179588&quot; &quot;179486&quot; &quot;179423&quot; &quot;179575&quot; &quot;179505&quot; &quot;179473&quot; &quot;179476&quot; &quot;179429&quot; &quot;179405&quot; &quot;179393&quot; &quot;179482&quot; &quot;179583&quot; &quot;179492&quot; &quot;179510&quot; &quot;179395&quot; &quot;179586&quot; &quot;179412&quot; &quot;179458&quot; &quot;179545&quot; &quot;179415&quot; &quot;179425&quot; &quot;179465&quot; &quot;179529&quot; &quot;179532&quot; &quot;179484&quot; &quot;179455&quot; &quot;179501&quot; &quot;179528&quot; &quot;179407&quot; &quot;179590&quot; &quot;179503&quot; &quot;179571&quot; &quot;179490&quot; &quot;179421&quot; &quot;179392&quot; &quot;179527&quot; &quot;179533&quot; &quot;179540&quot; &quot;179409&quot; &quot;179427&quot; &quot;179440&quot; &quot;179443&quot; &quot;179559&quot; &quot;179435&quot; &quot;179447&quot; &quot;179485&quot; &quot;179436&quot; &quot;179480&quot; &quot;179437&quot; &quot;179494&quot; &quot;179541&quot; &quot;179479&quot; &quot;179555&quot; &quot;179467&quot; &quot;179567&quot; &quot;179463&quot; &quot;179451&quot; &quot;179413&quot; &quot;179551&quot; &quot;179578&quot; &quot;179402&quot; &quot;179478&quot; &quot;179483&quot; &quot;179595&quot; &quot;179587&quot; &quot;179558&quot; &quot;179579&quot; &quot;179531&quot; &quot;179585&quot; &quot;179448&quot; &quot;179546&quot; &quot;179553&quot; &quot;179454&quot; &quot;179521&quot; &quot;179489&quot; &quot;179499&quot; &quot;179593&quot; &quot;179561&quot; &quot;179557&quot; &quot;179574&quot; &quot;179518&quot; &quot;179508&quot; &quot;179403&quot; &quot;179535&quot; &quot;179471&quot; &quot;179519&quot; &quot;179513&quot; &quot;179591&quot; &quot;179498&quot; &quot;179502&quot; &quot;179431&quot; &quot;179509&quot; &quot;179452&quot; &quot;179446&quot; &quot;179594&quot; &quot;179515&quot; &quot;179398&quot; &quot;179475&quot; &quot;179472&quot; &quot;179554&quot; &quot;179560&quot; &quot;179422&quot; &quot;179548&quot; &quot;179526&quot; &quot;179461&quot; &quot;179410&quot; &quot;179566&quot; &quot;179418&quot; &quot;179441&quot; &quot;179442&quot; &quot;179411&quot; &quot;179589&quot; &quot;179514&quot; &quot;179497&quot; &quot;179568&quot; &quot;179565&quot; &quot;179544&quot; &quot;179525&quot; &quot;179569&quot; &quot;179445&quot; &quot;179581&quot; &quot;179406&quot; &quot;179408&quot; &quot;179462&quot; &quot;179538&quot; &quot;179426&quot; &quot;179424&quot; &quot;179491&quot; &quot;179474&quot; &quot;179401&quot; &quot;179522&quot; &quot;179520&quot; &quot;179500&quot; &quot;179496&quot; &quot;179517&quot; &quot;179430&quot; &quot;179552&quot; &quot;179466&quot; &quot;179577&quot; &quot;179523&quot; &quot;179397&quot; &quot;179438&quot; &quot;179416&quot; &quot;179399&quot;)</span>","value":"(\"179460\" \"179432\" \"179444\" \"179419\" \"179530\" \"179428\" \"179511\" \"179512\" \"179543\" \"179394\" \"179477\" \"179450\" \"179453\" \"179549\" \"179539\" \"179582\" \"179572\" \"179550\" \"179580\" \"179396\" \"179457\" \"179470\" \"179542\" \"179547\" \"179537\" \"179516\" \"179536\" \"179573\" \"179434\" \"179420\" \"179439\" \"179507\" \"179592\" \"179556\" \"179524\" \"179563\" \"179493\" \"179481\" \"179570\" \"179433\" \"179506\" \"179469\" \"179417\" \"179404\" \"179534\" \"179464\" \"179468\" \"179562\" \"179504\" \"179414\" \"179584\" \"179400\" \"179495\" \"179459\" \"179487\" \"179576\" \"179449\" \"179488\" \"179564\" \"179456\" \"179588\" \"179486\" \"179423\" \"179575\" \"179505\" \"179473\" \"179476\" \"179429\" \"179405\" \"179393\" \"179482\" \"179583\" \"179492\" \"179510\" \"179395\" \"179586\" \"179412\" \"179458\" \"179545\" \"179415\" \"179425\" \"179465\" \"179529\" \"179532\" \"179484\" \"179455\" \"179501\" \"179528\" \"179407\" \"179590\" \"179503\" \"179571\" \"179490\" \"179421\" \"179392\" \"179527\" \"179533\" \"179540\" \"179409\" \"179427\" \"179440\" \"179443\" \"179559\" \"179435\" \"179447\" \"179485\" \"179436\" \"179480\" \"179437\" \"179494\" \"179541\" \"179479\" \"179555\" \"179467\" \"179567\" \"179463\" \"179451\" \"179413\" \"179551\" \"179578\" \"179402\" \"179478\" \"179483\" \"179595\" \"179587\" \"179558\" \"179579\" \"179531\" \"179585\" \"179448\" \"179546\" \"179553\" \"179454\" \"179521\" \"179489\" \"179499\" \"179593\" \"179561\" \"179557\" \"179574\" \"179518\" \"179508\" \"179403\" \"179535\" \"179471\" \"179519\" \"179513\" \"179591\" \"179498\" \"179502\" \"179431\" \"179509\" \"179452\" \"179446\" \"179594\" \"179515\" \"179398\" \"179475\" \"179472\" \"179554\" \"179560\" \"179422\" \"179548\" \"179526\" \"179461\" \"179410\" \"179566\" \"179418\" \"179441\" \"179442\" \"179411\" \"179589\" \"179514\" \"179497\" \"179568\" \"179565\" \"179544\" \"179525\" \"179569\" \"179445\" \"179581\" \"179406\" \"179408\" \"179462\" \"179538\" \"179426\" \"179424\" \"179491\" \"179474\" \"179401\" \"179522\" \"179520\" \"179500\" \"179496\" \"179517\" \"179430\" \"179552\" \"179466\" \"179577\" \"179523\" \"179397\" \"179438\" \"179416\" \"179399\")"}
;; <=

;; @@

(apply str (take 1000 (xml/emit-str (:raw (first (vals pages))))))

;; @@

;; @@
(time (doall
  (pmap #(spit (str "corrected/" (first %) ".xml")
               (with-out-str (xml/emit (correct (second %) dta))))
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
(use 'marmoset.util)
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
