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
            [suub.bote.clojure.xml :as xml]
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
                  read-dict
                  (dissoc dta-dict
                          "angezogeuen"
                          "uach"
                          "naeh"
                          "fie"
                          "fic"
                          "Dic"
                          "gebranch"
                          "Irrcreden"
                          "Tanfe")))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta-dict</span>","value":"#'suub.bote.dict/dta-dict"}
;; <=

;; @@
(def dta-prfx (time (prefixes dta-dict)))
;; @@
;; ->
;;; &quot;Elapsed time: 45832.903 msecs&quot;
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
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/gold-subst</span>","value":"#'suub.bote.dict/gold-subst"}
;; <=

;; @@
(def pots-dict (->> "resources/dict.edn"
                    slurp
                    edn/read-string
                    (map #(update-in % [1] bigint))
                    (into {})))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/pots-dict</span>","value":"#'suub.bote.dict/pots-dict"}
;; <=

;; @@
(def pots-prfx (time (prefixes pots-dict)))
;; @@

;; @@
(def pots {:matcher abbyy/matcher
           :dict pots-dict
           :prefixes pots-prfx
           :substs simple-subst})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/pots</span>","value":"#'suub.bote.dict/pots"}
;; <=

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
  (xml/parse
    (str "http://brema.suub.uni-bremen.de/grenzboten/download/fulltext/fr/" vlid)))
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
             (map #(hash-map :truth (slurp %)
                             :raw (download-xml (fs/base-name % true)))))))
;; @@

;; @@
#_(gv/error-view "hallo dies ist ein test"
               "hallu dis ist cin test")
;; @@

;; @@
;(gv/error-view gp cp)
;; @@

;; @@
;(dta-dict "Feierlichkeiten")
;; @@

;; @@
(apply str (take 1000 (with-out-str (xml/emit (correct p dta)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;&lt;?xml version=&#x27;1.0&#x27; encoding=&#x27;UTF-8&#x27;?&gt;\\n&lt;document version=&#x27;1.0&#x27; producer=&#x27;FineReader 8.0&#x27; xmlns=&#x27;http://www.abbyy.com/FineReader_xml/FineReader8-schema-v2.xml&#x27; xmlns:xsi=&#x27;http://www.w3.org/2001/XMLSchema-instance&#x27; xsi:schemaLocation=&#x27;http://www.abbyy.com/FineReader_xml/FineReader8-schema-v2.xml http://www.abbyy.com/FineReader_xml/FineReader8-schema-v2.xml&#x27; mainLanguage=&#x27;OldGerman&#x27; languages=&#x27;OldGerman&#x27;&gt;\\n\\n\\n&lt;page width=&#x27;2101&#x27; height=&#x27;3134&#x27; resolution=&#x27;300&#x27; isSkewCorrect=&#x27;true&#x27; skewAngle=&#x27;0.00675453505667716&#x27;&gt;\\n\\n\\n&lt;block blockType=&#x27;Text&#x27; blockName=&#x27;&#x27; isHidden=&#x27;true&#x27; l=&#x27;829&#x27; t=&#x27;363&#x27; r=&#x27;885&#x27; b=&#x27;410&#x27;&gt;\\n&lt;region&gt;\\n&lt;rect l=&#x27;829&#x27; t=&#x27;363&#x27; r=&#x27;885&#x27; b=&#x27;410&#x27;/&gt;\\n&lt;/region&gt;\\n\\n\\n&lt;text&gt;\\n\\n\\n&lt;par align=&#x27;Justified&#x27;&gt;\\n\\n\\n&lt;line baseline=&#x27;403&#x27; l=&#x27;835&#x27; t=&#x27;370&#x27; r=&#x27;879&#x27; b=&#x27;404&#x27;&gt;\\n&lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;\\n\\n\\n&lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;23&#x27; meanStrokeWidth=&#x27;38&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;854&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;835&#x27; characterHeight=&#x27;23&#x27; word&quot;</span>","value":"\"<?xml version='1.0' encoding='UTF-8'?>\\n<document version='1.0' producer='FineReader 8.0' xmlns='http://www.abbyy.com/FineReader_xml/FineReader8-schema-v2.xml' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://www.abbyy.com/FineReader_xml/FineReader8-schema-v2.xml http://www.abbyy.com/FineReader_xml/FineReader8-schema-v2.xml' mainLanguage='OldGerman' languages='OldGerman'>\\n\\n\\n<page width='2101' height='3134' resolution='300' isSkewCorrect='true' skewAngle='0.00675453505667716'>\\n\\n\\n<block blockType='Text' blockName='' isHidden='true' l='829' t='363' r='885' b='410'>\\n<region>\\n<rect l='829' t='363' r='885' b='410'/>\\n</region>\\n\\n\\n<text>\\n\\n\\n<par align='Justified'>\\n\\n\\n<line baseline='403' l='835' t='370' r='879' b='404'>\\n<formatting lang='OldGerman' ff='Arial' fs='10.'>\\n\\n\\n<charParams suspicious='true' baseLine='0' charConfidence='23' meanStrokeWidth='38' hasUncertainHeight='false' r='854' wordIdentifier='false' serifProbability='255' l='835' characterHeight='23' word\""}
;; <=

;; @@
(time (doall
  (pmap #(spit (str "corrected/" (first %) ".xml")
               (with-out-str (xml/emit (correct (second %) dta))))
        x)))
;; @@
;; ->
;;; &quot;Elapsed time: 19978.448 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)"}
;; <=

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
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/difficult</span>","value":"#'suub.bote.dict/difficult"}
;; <=

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
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def a (atom 0))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/a</span>","value":"#'suub.bote.dict/a"}
;; <=

;; @@
(def filecount
  (count (->> "/Users/ticking/Desktop/xml"
             io/file
             file-seq
             (filter #(and (fs/file? %)
                           (= ".xml" (fs/extension %))))
             (map #(vector (fs/base-name % true) %)))))

(def files
  (->> "/Users/ticking/Desktop/xml"
             io/file
             file-seq
             (filter #(and (fs/file? %)
                           (= ".xml" (fs/extension %))))
             (map #(vector (fs/base-name % true) %))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/files</span>","value":"#'suub.bote.dict/files"}
;; <=

;; @@
(reset! a 0)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}
;; <=

;; @@
(use 'marmoset.util)
;; @@

;; @@
(progress-view 50 100)
;; @@

;; @@
(future (doseq [i (range 10000)]
          (swap! a inc)
          (Thread/sleep 1)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;core$future_call$reify__6320@6d2ebe83: :pending&gt;</span>","value":"#<core$future_call$reify__6320@6d2ebe83: :pending>"}
;; <=

;; @@
(def f (future
  (time (dorun
  (pmap #(do
           (swap! a inc)
           (spit (str "/Users/ticking/Desktop/test" (first %) ".xml")
               (with-out-str (xml/emit (correct (xml/parse (second %)) dta)))))
        files)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/f</span>","value":"#'suub.bote.dict/f"}
;; <=
