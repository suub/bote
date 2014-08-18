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
            [instaparse.core :as insta]
            [instaparse.combinators :as instac]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/builtin-substs</span>","value":"#'suub.bote.dictionary/builtin-substs"}
;; <=

;; @@
(def progress (atom 0))

(defn tokenizer [matches]
  (let [tokenizer (->> matches
                       (map instac/string)
                       (apply instac/alt)
                       (instac/plus)
                       (vector :S)
                       (insta/parser))]
    (fn [word]
      (->> word
           (insta/parses tokenizer)
           (r/map #(insta/transform {:S vector} %))))))

(defn subindex [tokenize part]
  (r/reduce (fn [index [word prob]]
              (swap! progress inc)
              (r/reduce #(assoc-in %1 (conj %2 :t) prob)
                        index
                        (tokenize word)))
            {}
            part))


(defn index [transformations dict n]
  (let [tokenize (tokenizer (keys transformations))
        idx (->> dict
                 (partition-all n)
                 (pmap #(subindex tokenize %))
                 vec)]
    {:transformations transformations
     :index idx}))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/index</span>","value":"#'suub.bote.dictionary/index"}
;; <=

;; @@
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
    (-> index
        (mapcat #(lookup-worker % query))
        (map #(assoc % :prob
                     (* (:word-prob %)
                        (:subst-prob %)))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/lookup</span>","value":"#'suub.bote.dictionary/lookup"}
;; <=

;; @@
(def dict (builtin-dict))
(def subst (builtin-substs))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/subst</span>","value":"#'suub.bote.dictionary/subst"}
;; <=

;; @@
(def idx (future (index subst dict 100000)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/idx</span>","value":"#'suub.bote.dictionary/idx"}
;; <=

;; @@
(def d (into {} dict))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/d</span>","value":"#'suub.bote.dictionary/d"}
;; <=

;; @@
(def s (into {}
             (for [[truth n] subst
                   [ocr prob] n]
               [[ocr truth] prob])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/s</span>","value":"#'suub.bote.dictionary/s"}
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
  [matcher substs query]
  (letfn [(worker [sub-query]
            (for [[[ocr truth] prob] substs
                  :let [[from rest :as matched]
                        (matcher ocr sub-query)]
                  :when matched
                  match (if (empty? rest)
                          [{:word ""
                            :subst-prob 1
                            :substs nil}]
                          (worker rest))]
              (-> match
                  (update-in [:word] #(str truth %))
                  (update-in [:subst-prob] #(* % prob))
                  (update-in [:substs] #(cons {:from from
                                               :to truth
                                               :prob prob} %)))))]
    (worker query)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/transform</span>","value":"#'suub.bote.dictionary/transform"}
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
  [matcher dict dict-prefixes substs query]
  (letfn [(iteration [run]
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
          (worker [unfinished finished]
            (if (empty? unfinished)
              finished
              (let [{new-finished true
                     new-unfinished false}
                    (->> unfinished
                         (mapcat iteration)
                         (filter (comp dict-prefixes :word))
                         (group-by (comp empty? :rest)))]
                (recur new-unfinished
                       (into finished new-finished)))))]
    (->> (worker #{{:rest query
                   :word ""
                   :subst-prob 1
                   :substs []}}
                 #{})
         (map #(dissoc % :rest))
         (map #(assoc % :word-prob (dict (:word %))))
         (filter :word-prob)
         (map #(assoc % :prob (* (:word-prob %)
                                 (:subst-prob %)))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/transform</span>","value":"#'suub.bote.dictionary/transform"}
;; <=

;; @@
(defn simple-matcher [p q]
  (when-let [rest (util/drop-prefix p q)] [p rest]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/simple-matcher</span>","value":"#'suub.bote.dictionary/simple-matcher"}
;; <=

;; @@
(time (transform simple-matcher s "H"))
;; @@
;; ->
;;; &quot;Elapsed time: 0.821 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:substs</span>","value":":substs"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;H&quot;</span>","value":"\"H\""}],"value":"[:from \"H\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;H&quot;</span>","value":"\"H\""}],"value":"[:to \"H\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>1057/1073</span>","value":"1057/1073"}],"value":"[:prob 1057/1073]"}],"value":"{:from \"H\", :to \"H\", :prob 1057/1073}"}],"value":"({:from \"H\", :to \"H\", :prob 1057/1073})"}],"value":"[:substs ({:from \"H\", :to \"H\", :prob 1057/1073})]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:word</span>","value":":word"},{"type":"html","content":"<span class='clj-string'>&quot;H&quot;</span>","value":"\"H\""}],"value":"[:word \"H\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:subst-prob</span>","value":":subst-prob"},{"type":"html","content":"<span class='clj-ratio'>1057/1073</span>","value":"1057/1073"}],"value":"[:subst-prob 1057/1073]"}],"value":"{:substs ({:from \"H\", :to \"H\", :prob 1057/1073}), :word \"H\", :subst-prob 1057/1073}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:substs</span>","value":":substs"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;H&quot;</span>","value":"\"H\""}],"value":"[:from \"H\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;h&quot;</span>","value":"\"h\""}],"value":"[:to \"h\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>10/1073</span>","value":"10/1073"}],"value":"[:prob 10/1073]"}],"value":"{:from \"H\", :to \"h\", :prob 10/1073}"}],"value":"({:from \"H\", :to \"h\", :prob 10/1073})"}],"value":"[:substs ({:from \"H\", :to \"h\", :prob 10/1073})]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:word</span>","value":":word"},{"type":"html","content":"<span class='clj-string'>&quot;h&quot;</span>","value":"\"h\""}],"value":"[:word \"h\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:subst-prob</span>","value":":subst-prob"},{"type":"html","content":"<span class='clj-ratio'>10/1073</span>","value":"10/1073"}],"value":"[:subst-prob 10/1073]"}],"value":"{:substs ({:from \"H\", :to \"h\", :prob 10/1073}), :word \"h\", :subst-prob 10/1073}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:substs</span>","value":":substs"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;H&quot;</span>","value":"\"H\""}],"value":"[:from \"H\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;\\f&quot;</span>","value":"\"\\f\""}],"value":"[:to \"\\f\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>1/1073</span>","value":"1/1073"}],"value":"[:prob 1/1073]"}],"value":"{:from \"H\", :to \"\\f\", :prob 1/1073}"}],"value":"({:from \"H\", :to \"\\f\", :prob 1/1073})"}],"value":"[:substs ({:from \"H\", :to \"\\f\", :prob 1/1073})]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:word</span>","value":":word"},{"type":"html","content":"<span class='clj-string'>&quot;\\f&quot;</span>","value":"\"\\f\""}],"value":"[:word \"\\f\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:subst-prob</span>","value":":subst-prob"},{"type":"html","content":"<span class='clj-ratio'>1/1073</span>","value":"1/1073"}],"value":"[:subst-prob 1/1073]"}],"value":"{:substs ({:from \"H\", :to \"\\f\", :prob 1/1073}), :word \"\\f\", :subst-prob 1/1073}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:substs</span>","value":":substs"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;H&quot;</span>","value":"\"H\""}],"value":"[:from \"H\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;R&quot;</span>","value":"\"R\""}],"value":"[:to \"R\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>1/1073</span>","value":"1/1073"}],"value":"[:prob 1/1073]"}],"value":"{:from \"H\", :to \"R\", :prob 1/1073}"}],"value":"({:from \"H\", :to \"R\", :prob 1/1073})"}],"value":"[:substs ({:from \"H\", :to \"R\", :prob 1/1073})]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:word</span>","value":":word"},{"type":"html","content":"<span class='clj-string'>&quot;R&quot;</span>","value":"\"R\""}],"value":"[:word \"R\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:subst-prob</span>","value":":subst-prob"},{"type":"html","content":"<span class='clj-ratio'>1/1073</span>","value":"1/1073"}],"value":"[:subst-prob 1/1073]"}],"value":"{:substs ({:from \"H\", :to \"R\", :prob 1/1073}), :word \"R\", :subst-prob 1/1073}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:substs</span>","value":":substs"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;H&quot;</span>","value":"\"H\""}],"value":"[:from \"H\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;\\nh&quot;</span>","value":"\"\\nh\""}],"value":"[:to \"\\nh\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>1/1073</span>","value":"1/1073"}],"value":"[:prob 1/1073]"}],"value":"{:from \"H\", :to \"\\nh\", :prob 1/1073}"}],"value":"({:from \"H\", :to \"\\nh\", :prob 1/1073})"}],"value":"[:substs ({:from \"H\", :to \"\\nh\", :prob 1/1073})]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:word</span>","value":":word"},{"type":"html","content":"<span class='clj-string'>&quot;\\nh&quot;</span>","value":"\"\\nh\""}],"value":"[:word \"\\nh\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:subst-prob</span>","value":":subst-prob"},{"type":"html","content":"<span class='clj-ratio'>1/1073</span>","value":"1/1073"}],"value":"[:subst-prob 1/1073]"}],"value":"{:substs ({:from \"H\", :to \"\\nh\", :prob 1/1073}), :word \"\\nh\", :subst-prob 1/1073}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:substs</span>","value":":substs"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:from</span>","value":":from"},{"type":"html","content":"<span class='clj-string'>&quot;H&quot;</span>","value":"\"H\""}],"value":"[:from \"H\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:to</span>","value":":to"},{"type":"html","content":"<span class='clj-string'>&quot;\\n\\n&quot;</span>","value":"\"\\n\\n\""}],"value":"[:to \"\\n\\n\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:prob</span>","value":":prob"},{"type":"html","content":"<span class='clj-ratio'>3/1073</span>","value":"3/1073"}],"value":"[:prob 3/1073]"}],"value":"{:from \"H\", :to \"\\n\\n\", :prob 3/1073}"}],"value":"({:from \"H\", :to \"\\n\\n\", :prob 3/1073})"}],"value":"[:substs ({:from \"H\", :to \"\\n\\n\", :prob 3/1073})]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:word</span>","value":":word"},{"type":"html","content":"<span class='clj-string'>&quot;\\n\\n&quot;</span>","value":"\"\\n\\n\""}],"value":"[:word \"\\n\\n\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:subst-prob</span>","value":":subst-prob"},{"type":"html","content":"<span class='clj-ratio'>3/1073</span>","value":"3/1073"}],"value":"[:subst-prob 3/1073]"}],"value":"{:substs ({:from \"H\", :to \"\\n\\n\", :prob 3/1073}), :word \"\\n\\n\", :subst-prob 3/1073}"}],"value":"({:substs ({:from \"H\", :to \"H\", :prob 1057/1073}), :word \"H\", :subst-prob 1057/1073} {:substs ({:from \"H\", :to \"h\", :prob 10/1073}), :word \"h\", :subst-prob 10/1073} {:substs ({:from \"H\", :to \"\\f\", :prob 1/1073}), :word \"\\f\", :subst-prob 1/1073} {:substs ({:from \"H\", :to \"R\", :prob 1/1073}), :word \"R\", :subst-prob 1/1073} {:substs ({:from \"H\", :to \"\\nh\", :prob 1/1073}), :word \"\\nh\", :subst-prob 1/1073} {:substs ({:from \"H\", :to \"\\n\\n\", :prob 3/1073}), :word \"\\n\\n\", :subst-prob 3/1073})"}
;; <=

;; @@
(time (count (transform simple-matcher s "aaaa")))
;; @@

;; @@
  (defn total-memory [obj]
    (let [baos (java.io.ByteArrayOutputStream.)]
      (with-open [oos (java.io.ObjectOutputStream. baos)]
        (.writeObject oos obj))
      (count (.toByteArray baos))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/total-memory</span>","value":"#'suub.bote.dictionary/total-memory"}
;; <=

;; @@
(defn prefixes [s]
  (map #(subs s 0 %)
          (range 0 (inc (count s)))))
(def prfx (time (into #{} (mapcat prefixes (map first d)))))
;; @@
;; ->
;;; &quot;Elapsed time: 148370.09 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/prfx</span>","value":"#'suub.bote.dictionary/prfx"}
;; <=

;; @@
(total-memory prfx)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>288919138</span>","value":"288919138"}
;; <=

;; @@
(time (count (transform simple-matcher d prfx s "hallo")))
;; @@
;; ->
;;; &quot;Elapsed time: 11935.504 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>1318</span>","value":"1318"}
;; <=

;; @@
(defn lazy-index
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/lazy-index</span>","value":"#'suub.bote.dictionary/lazy-index"}
;; <=

;; @@
(defn lazy-lookup
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/lazy-lookup</span>","value":"#'suub.bote.dictionary/lazy-lookup"}
;; <=

;; @@
(def idx (lazy-index subst dict))
(time (count (lazy-lookup simple-matcher idx "hallo")))
(time (count (lazy-lookup simple-matcher idx "hallo")))
;; @@
;; ->
;;; &quot;Elapsed time: 211908.529 msecs&quot;
;;; &quot;Elapsed time: 174.171 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>1318</span>","value":"1318"}
;; <=

;; @@

;; @@
