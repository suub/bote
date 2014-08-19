;; gorilla-repl.fileformat = 1

;; **
;;; # Dictionary error correction.
;;; 
;;; This code uses a weighted finite state transducer aproach to give all possible matches for a given word.
;; **

;; @@
(ns suub.bote.dictionary
  (:require [gorilla-plot.core :as plot]
            [clojure.core.reducers :as r]
            [suub.bote.util :as util]
            [suub.bote.abbyy :as abbyy]
            [clojure.test :as t]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [clojure.string :as string]
            [clojure.data.priority-map :as pm]
            [instaparse.core :as insta]
            [instaparse.combinators :as instac]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn read-dict [path]
  (with-open [in (io/reader path)]
    (let [words (->> (line-seq in)
                     (r/map #(string/split % #"\s+"))
                     (r/map (fn [[cnt _ simpl]] [simpl (bigint cnt)])))
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/read-substs</span>","value":"#'suub.bote.dictionary/read-substs"}
;; <=

;; @@
(def d (read-dict "resources/dta-freq.d/dta-core-1850+.fuw"))
(def s (read-substs "resources/substitutions.edn"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/s</span>","value":"#'suub.bote.dictionary/s"}
;; <=

;; @@
(defn prefixes [[w p]]
  (into {}
        (r/map (fn [n] [(subs w 0 n) p])
          (range 0 (inc (count w))))))

(defn merge-prefixes [& m]
  (apply merge-with max m))

(def prfx (time (r/reduce merge-prefixes (r/map prefixes d))))©ß
;; @@

;; @@
(defn simple-matcher [p q]
  (when-let [rest (util/drop-prefix p q)] [p rest]))
;; @@

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
  [matcher dict dict-prefixes substs query]
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
                                        :let [prefix-prob (dict-prefixes word)
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/transform</span>","value":"#'suub.bote.dictionary/transform"}
;; <=

;; @@
(time (first (transform simple-matcher d prfx s "oldenburg")))
;; @@
;; ->
;;; &quot;Elapsed time: 321.836 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>226868733196194990692/65150994323949149994027982138470625</span>","value":"226868733196194990692/65150994323949149994027982138470625"}],"value":"[:prob 226868733196194990692/65150994323949149994027982138470625]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:word-prob</span>","value":":word-prob"},{"type":"html","content":"<span class='clj-ratio'>1/35044324</span>","value":"1/35044324"}],"value":"[:word-prob 1/35044324]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:word</span>","value":":word"},{"type":"html","content":"<span class='clj-string'>&quot;Isenburg&quot;</span>","value":"\"Isenburg\""}],"value":"[:word \"Isenburg\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:subst-prob</span>","value":":subst-prob"},{"type":"html","content":"<span class='clj-ratio'>6352324529493459739376/52054873167779643854245369375</span>","value":"6352324529493459739376/52054873167779643854245369375"}],"value":"[:subst-prob 6352324529493459739376/52054873167779643854245369375]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:substs</span>","value":":substs"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;o&quot;</span>","value":"\"o\""}],"value":"[:from \"o\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;I&quot;</span>","value":"\"I\""}],"value":"[:to \"I\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>1/331</span>","value":"1/331"}],"value":"[:prob 1/331]"}],"value":"{:from \"o\", :to \"I\", :prob 1/331}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;ld&quot;</span>","value":"\"ld\""}],"value":"[:from \"ld\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;s&quot;</span>","value":"\"s\""}],"value":"[:to \"s\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>1/23530</span>","value":"1/23530"}],"value":"[:prob 1/23530]"}],"value":"{:from \"ld\", :to \"s\", :prob 1/23530}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;e&quot;</span>","value":"\"e\""}],"value":"[:from \"e\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;e&quot;</span>","value":"\"e\""}],"value":"[:to \"e\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>1474/1475</span>","value":"1474/1475"}],"value":"[:prob 1474/1475]"}],"value":"{:from \"e\", :to \"e\", :prob 1474/1475}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;n&quot;</span>","value":"\"n\""}],"value":"[:from \"n\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;n&quot;</span>","value":"\"n\""}],"value":"[:to \"n\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>42574/43245</span>","value":"42574/43245"}],"value":"[:prob 42574/43245]"}],"value":"{:from \"n\", :to \"n\", :prob 42574/43245}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;b&quot;</span>","value":"\"b\""}],"value":"[:from \"b\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;b&quot;</span>","value":"\"b\""}],"value":"[:to \"b\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>3093/3109</span>","value":"3093/3109"}],"value":"[:prob 3093/3109]"}],"value":"{:from \"b\", :to \"b\", :prob 3093/3109}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;u&quot;</span>","value":"\"u\""}],"value":"[:from \"u\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;u&quot;</span>","value":"\"u\""}],"value":"[:to \"u\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>14752/15161</span>","value":"14752/15161"}],"value":"[:prob 14752/15161]"}],"value":"{:from \"u\", :to \"u\", :prob 14752/15161}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;r&quot;</span>","value":"\"r\""}],"value":"[:from \"r\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;r&quot;</span>","value":"\"r\""}],"value":"[:to \"r\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>28869/28922</span>","value":"28869/28922"}],"value":"[:prob 28869/28922]"}],"value":"{:from \"r\", :to \"r\", :prob 28869/28922}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;g&quot;</span>","value":"\"g\""}],"value":"[:from \"g\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;g&quot;</span>","value":"\"g\""}],"value":"[:to \"g\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>5533/5534</span>","value":"5533/5534"}],"value":"[:prob 5533/5534]"}],"value":"{:from \"g\", :to \"g\", :prob 5533/5534}"}],"value":"[{:from \"o\", :to \"I\", :prob 1/331} {:from \"ld\", :to \"s\", :prob 1/23530} {:from \"e\", :to \"e\", :prob 1474/1475} {:from \"n\", :to \"n\", :prob 42574/43245} {:from \"b\", :to \"b\", :prob 3093/3109} {:from \"u\", :to \"u\", :prob 14752/15161} {:from \"r\", :to \"r\", :prob 28869/28922} {:from \"g\", :to \"g\", :prob 5533/5534}]"}],"value":"[:substs [{:from \"o\", :to \"I\", :prob 1/331} {:from \"ld\", :to \"s\", :prob 1/23530} {:from \"e\", :to \"e\", :prob 1474/1475} {:from \"n\", :to \"n\", :prob 42574/43245} {:from \"b\", :to \"b\", :prob 3093/3109} {:from \"u\", :to \"u\", :prob 14752/15161} {:from \"r\", :to \"r\", :prob 28869/28922} {:from \"g\", :to \"g\", :prob 5533/5534}]]"}],"value":"{:prob 226868733196194990692/65150994323949149994027982138470625, :word-prob 1/35044324, :word \"Isenburg\", :subst-prob 6352324529493459739376/52054873167779643854245369375, :substs [{:from \"o\", :to \"I\", :prob 1/331} {:from \"ld\", :to \"s\", :prob 1/23530} {:from \"e\", :to \"e\", :prob 1474/1475} {:from \"n\", :to \"n\", :prob 42574/43245} {:from \"b\", :to \"b\", :prob 3093/3109} {:from \"u\", :to \"u\", :prob 14752/15161} {:from \"r\", :to \"r\", :prob 28869/28922} {:from \"g\", :to \"g\", :prob 5533/5534}]}"}
;; <=

;; @@
(time (first (transform simple-matcher d prfx s "mann")))
;; @@
;; ->
;;; &quot;Elapsed time: 11.501 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>16400378787/3148697432031676</span>","value":"16400378787/3148697432031676"}],"value":"[:prob 16400378787/3148697432031676]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:word-prob</span>","value":":word-prob"},{"type":"html","content":"<span class='clj-ratio'>93/17522162</span>","value":"93/17522162"}],"value":"[:word-prob 93/17522162]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:word</span>","value":":word"},{"type":"html","content":"<span class='clj-string'>&quot;mann&quot;</span>","value":"\"mann\""}],"value":"[:word \"mann\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:subst-prob</span>","value":":subst-prob"},{"type":"html","content":"<span class='clj-ratio'>176348159/179697998</span>","value":"176348159/179697998"}],"value":"[:subst-prob 176348159/179697998]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:substs</span>","value":":substs"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;m&quot;</span>","value":"\"m\""}],"value":"[:from \"m\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;m&quot;</span>","value":"\"m\""}],"value":"[:to \"m\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>8403/8527</span>","value":"8403/8527"}],"value":"[:prob 8403/8527]"}],"value":"{:from \"m\", :to \"m\", :prob 8403/8527}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;a&quot;</span>","value":"\"a\""}],"value":"[:from \"a\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;a&quot;</span>","value":"\"a\""}],"value":"[:to \"a\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>2171/2173</span>","value":"2171/2173"}],"value":"[:prob 2171/2173]"}],"value":"{:from \"a\", :to \"a\", :prob 2171/2173}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;nn&quot;</span>","value":"\"nn\""}],"value":"[:from \"nn\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;nn&quot;</span>","value":"\"nn\""}],"value":"[:to \"nn\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>1537/1542</span>","value":"1537/1542"}],"value":"[:prob 1537/1542]"}],"value":"{:from \"nn\", :to \"nn\", :prob 1537/1542}"}],"value":"[{:from \"m\", :to \"m\", :prob 8403/8527} {:from \"a\", :to \"a\", :prob 2171/2173} {:from \"nn\", :to \"nn\", :prob 1537/1542}]"}],"value":"[:substs [{:from \"m\", :to \"m\", :prob 8403/8527} {:from \"a\", :to \"a\", :prob 2171/2173} {:from \"nn\", :to \"nn\", :prob 1537/1542}]]"}],"value":"{:prob 16400378787/3148697432031676, :word-prob 93/17522162, :word \"mann\", :subst-prob 176348159/179697998, :substs [{:from \"m\", :to \"m\", :prob 8403/8527} {:from \"a\", :to \"a\", :prob 2171/2173} {:from \"nn\", :to \"nn\", :prob 1537/1542}]}"}
;; <=

;; @@
(apply max-key count (keys d))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;vonVerkauff-Courant-Zeit-Factorie-Unkosten-Rent-Thara-Rabatt-Baratto-Schiffs-Part&quot;</span>","value":"\"vonVerkauff-Courant-Zeit-Factorie-Unkosten-Rent-Thara-Rabatt-Baratto-Schiffs-Part\""}
;; <=

;; @@
(d "Mann")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>8907/35044324</span>","value":"8907/35044324"}
;; <=

;; @@
(def difficult (filter #(not= % (->> %
                                    (transform simple-matcher d prfx s)
                                    first
                                    :word))
                       (keys d)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/difficult</span>","value":"#'suub.bote.dictionary/difficult"}
;; <=

;; @@
(first difficult)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;Gentleman-Farmers&quot;</span>","value":"\"Gentleman-Farmers\""}
;; <=

;; **
;;; #Deployment code
;; **

;; @@
(defn correct-word [word]
  (if (Character/isSpace (first word))
    word
    (or (:word (first (transform simple-matcher d prfx s word)))
        word)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/correct-word</span>","value":"#'suub.bote.dictionary/correct-word"}
;; <=

;; @@
(defn correct-page [page]
  (->> page
       (partition-by #(Character/isSpace %))
       (mapcat correct-word)
       (apply str)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/correct-page</span>","value":"#'suub.bote.dictionary/correct-page"}
;; <=

;; @@
(correct-page "der iiiann grht gerne mit mut nach oldenburg")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;den Milano geht gerne Mit mut nach Isenburg&quot;</span>","value":"\"den Milano geht gerne Mit mut nach Isenburg\""}
;; <=

;; @@
(def files (rest (file-seq (io/file "/Users/ticking/Desktop/ocr-engine-results/abby_verbessert/unverbessert"))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/files</span>","value":"#'suub.bote.dictionary/files"}
;; <=

;; @@
(future
(doall
 (pmap (fn [f]
         (->> f
              slurp
     		  correct-page
       		  (spit (io/file "/Users/ticking/Desktop/ocr-engine-results/abby_verbessert2/ocr-results" (.getName f)))))
       files)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;core$future_call$reify__6267@28f6ec64: :pending&gt;</span>","value":"#<core$future_call$reify__6267@28f6ec64: :pending>"}
;; <=

;; @@
(def f *1)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/f</span>","value":"#'suub.bote.dictionary/f"}
;; <=

;; @@
f
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;core$future_call$reify__6267@28f6ec64: (nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)&gt;</span>","value":"#<core$future_call$reify__6267@28f6ec64: (nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)>"}
;; <=

;; @@
(d "die")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>1/35044324</span>","value":"1/35044324"}
;; <=

;; @@

;; @@
