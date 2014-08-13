;; gorilla-repl.fileformat = 1

;; **
;;; # Dictionary error correction.
;;;
;;; This code uses a weighted finite state transducer aproach to give all possible matches for a given word.
;; **

;; @@
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
(keys (get-in idx [:index 1 "St" "a" "m" "m" "e"]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(&quot;s&quot; :t)</span>","value":"(\"s\" :t)"}
;; <=

;; @@
(keys (nth (:index idx) 1))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(&quot;ii&quot; &quot;nn&quot; &quot;xx&quot; &quot;A&quot; &quot;a&quot; &quot;12&quot; &quot;St&quot; &quot;tu&quot; &quot;B&quot; &quot;b&quot; &quot;tö&quot; &quot;C&quot; &quot;c&quot; &quot;il&quot; &quot;ru&quot; &quot;D&quot; &quot;d&quot; &quot;chi&quot; &quot;ei&quot; &quot;ä&quot; &quot;E&quot; &quot;e&quot; &quot;in&quot; &quot;F&quot; &quot;f&quot; &quot;ci&quot; &quot;fl&quot; &quot;&#x27;&quot; &quot;G&quot; &quot;g&quot; &quot;nu&quot; &quot;H&quot; &quot;h&quot; &quot;I&quot; &quot;i&quot; &quot;en&quot; &quot;ir&quot; &quot;Leb&quot; &quot;*&quot; &quot;J&quot; &quot;ein&quot; &quot;j&quot; &quot;K&quot; &quot;k&quot; &quot;it&quot; &quot;L&quot; &quot;l&quot; &quot;iu&quot; &quot;-&quot; &quot;an&quot; &quot;M&quot; &quot;m&quot; &quot;er&quot; &quot;184&quot; &quot;.&quot; &quot;N&quot; &quot;n&quot; &quot;O&quot; &quot;o&quot; &quot;ra&quot; &quot;0&quot; &quot;P&quot; &quot;p&quot; &quot;eu&quot; &quot;lü&quot; &quot;1&quot; &quot;Q&quot; &quot;q&quot; &quot;2&quot; &quot;R&quot; &quot;r&quot; &quot;cu&quot; &quot;3&quot; &quot;S&quot; &quot;s&quot; &quot;4&quot; &quot;T&quot; &quot;t&quot; &quot;5&quot; &quot;U&quot; &quot;u&quot; &quot;ti&quot; &quot;6&quot; &quot;V&quot; &quot;v&quot; &quot;ö&quot; &quot;Si&quot; &quot;7&quot; &quot;W&quot; &quot;w&quot; &quot;ne&quot; &quot;ri&quot; &quot;8&quot; &quot;x&quot; &quot;y&quot; &quot;n&#x27;&quot; &quot;un&quot; &quot;Z&quot; &quot;z&quot; &quot;ke&quot; &quot;ni&quot; &quot;to&quot; &quot;zu&quot; &quot;&lt;&quot; &quot;ü&quot; &quot;r.&quot; &quot;da&quot; &quot;li&quot; &quot;zw&quot; &quot;&gt;&quot; &quot;Ge&quot; &quot;20&quot; &quot;fe&quot; &quot;10&quot;)</span>","value":"(\"ii\" \"nn\" \"xx\" \"A\" \"a\" \"12\" \"St\" \"tu\" \"B\" \"b\" \"tö\" \"C\" \"c\" \"il\" \"ru\" \"D\" \"d\" \"chi\" \"ei\" \"ä\" \"E\" \"e\" \"in\" \"F\" \"f\" \"ci\" \"fl\" \"'\" \"G\" \"g\" \"nu\" \"H\" \"h\" \"I\" \"i\" \"en\" \"ir\" \"Leb\" \"*\" \"J\" \"ein\" \"j\" \"K\" \"k\" \"it\" \"L\" \"l\" \"iu\" \"-\" \"an\" \"M\" \"m\" \"er\" \"184\" \".\" \"N\" \"n\" \"O\" \"o\" \"ra\" \"0\" \"P\" \"p\" \"eu\" \"lü\" \"1\" \"Q\" \"q\" \"2\" \"R\" \"r\" \"cu\" \"3\" \"S\" \"s\" \"4\" \"T\" \"t\" \"5\" \"U\" \"u\" \"ti\" \"6\" \"V\" \"v\" \"ö\" \"Si\" \"7\" \"W\" \"w\" \"ne\" \"ri\" \"8\" \"x\" \"y\" \"n'\" \"un\" \"Z\" \"z\" \"ke\" \"ni\" \"to\" \"zu\" \"<\" \"ü\" \"r.\" \"da\" \"li\" \"zw\" \">\" \"Ge\" \"20\" \"fe\" \"10\")"}
;; <=

;; @@
(count (remove #(< (last %) 0.01)
               (sort-by last
                        (for [[truth n] subst
                              [ocr prob] n]
                          [truth ocr prob]))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>471</span>","value":"471"}
;; <=

;; @@
(let [f (* (/ 100.0 (count dict)) @progress)
      _ (Thread/sleep 1000)
      s (* (/ 100.0 (count dict)) @progress)]
  (/ 100 (- s f)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>14743.565610863925</span>","value":"14743.565610863925"}
;; <=

;; @@
(defn misrecognitions [subst]
  (let [
        ])
  (for [[truth n] subst
        [ocr prob] n]
    [truth ocr prob]))

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

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dictionary/tokenizer</span>","value":"#'suub.bote.dictionary/tokenizer"}
;; <=

;; @@
(future-cancel idx)g
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; @@

;; @@
