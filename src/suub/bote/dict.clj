;; gorilla-repl.fileformat = 1

;; **
;;; # Dictionary error correction.
;;; 
;;; This code uses a weighted finite state transducer aproach to give all possible matches for a given word.
;; **

;; @@
(ns suub.bote.dict
  (:require ;[gorilla-plot.core :as plot]
            ;[gorilla-repl.table
            [clojure.core.reducers :as r]
            [suub.bote.util :as util]
            [suub.bote.abbyy :as abbyy]
            [clojure.test :as t]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [me.raynes.laser :as l]
            [suub.bote.clojure.xml :as xml]
            [clojure.string :as string]
            [clojure.data.priority-map :as pm]
            [instaparse.core :as insta]
            [instaparse.combinators :as instac]
            [clojure.edn :as edn]
            [me.raynes.fs :as fs]
            [laser-experiments.core :as le]
            [suub.bote.abbyy :as abbyy]
            [error-codes.files :as ec]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn read-dict [path]
  (with-open [in (io/reader path)]
    (let [words (->> (line-seq in)
                     (r/map #(string/split % #"\s+"))
                     (r/remove empty?)
                     ;;use simplified form orig is not good
                     (r/map (fn [[cnt orig simpl]]
                              [simpl (bigint cnt)])))
          word-count (r/fold + (r/map second words))]
      (->> words
           (r/map (fn [[w c]] [w (/ c word-count)]))
           (r/map (fn [[w c]] {w c}))
           (r/fold (partial merge-with +))))))

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
  (when-let [rest (drop-prefix p q)] [p rest]))
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
#_(defonce dta-dict (read-dict "resources/dta-freq.d/dta-core.fuw"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta-dict</span>","value":"#'suub.bote.dict/dta-dict"}
;; <=

;; @@
#_(def dta-dict (dissoc dta-dict
                "angezogeuen"
                "uach"
                "naeh"
                "fie"
                "fic"
                "zn"
                "Dic"
                "gebranch"
                "uud"
                "Irrcreden"
                "Tanfe"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta-dict</span>","value":"#'suub.bote.dict/dta-dict"}
;; <=

;; @@
#_(def gold-subst (read-substs "resources/substitutions.edn"))
#_(def gold-subst (read-string (slurp "resources/new-subsstitution.edn")))

#_(defn create-gold-subst [gold-subst]
  (into {} (for [[a v] gold-subst
                 [b v] v]
             [[a b] v])))

#_(def gold-subst (create-gold-subst gold-subst))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/gold-subst</span>","value":"#'suub.bote.dict/gold-subst"}
;; <=

;; @@
#_(def pots-dict (->> "resources/dict.edn"
                    slurp
                    edn/read-string
                    (map #(update-in % [1] bigint))
                    (into {})))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/simple-subst</span>","value":"#'suub.bote.dict/simple-subst"}
;; <=

;; @@
#_(def simple-subst
        {["u" "n"] 1/2
         ["n" "u"] 1/2
         ["c" "e"] 1/4
         ["e" "c"] 1/4
         ["N" "U"] 1/8
         ["N" "R"] 1/8
         ["rn" "m"] 1/8
         ["iii" "m"] 1/8
         ["in" "m"] 1/8
         ["m" "in"] 1/8
         ["n" "m"] 1/8
         ["m" "n"] 1/8
         ["m" "en"] 1/8
         ["f" "s"] 1/4
         ["s" "f"] 1/4
         ["v" "o"] 1/4
         ["ö" "v"] 1/8
         ["v" "ö"] 1/8
         ["o" "v"] 1/4
         ["n" "r"] 1/4
         ["d" "b"] 1/8
         ["b" "d"] 1/8
         ["ü" "n"] 1/8
         ["n" "ü"] 1/8
         ["zu" "m"] 1/50
         ["n" "il"] 1/50
         ["m" "n?"] 1/50
         ["en" "m"] 1/50
         ["s" "h"] 1/50
         ["b" "h"] 1/8
         ["h" "b"] 1/8
         ["t" "d"] 1/50
         ["d" "t"] 1/50
         ["im" "un"] 1/50
         ["s" "S"] 1/50
         ["S" "s"] 1/50
         ["t" "e"] 1/50
         ["y" "p"] 1/8
         ["p" "y"] 1/8
         ["l" "i"] 1/8
         ["i" "l"] 1/8
         ["ö" "s"] 1/8
         ["s" "ö"] 1/8
         ["k" "l"] 1/8
         ["l" "k"] 1/8
         ["l" "t"] 1/8
         ["t" "l"] 1/8
         ["h" "s"] 1/8
         ["A" "U"] 1/50
         ["U" "A"] 1/50
         ["m," "n"] 1/50
         ["m:" "n"] 1/50
         ["li" "u"] 1/50
         ["it" "n"] 1/50
         ["k" "t"] 1/50
         ["t" "k"] 1/50
         ["ö" "o"] 1/50
         ["o" "ö"] 1/50
         ["ä" "a"] 1/50
         ["a" "ä"] 1/50
         ["ü" "u"] 1/50
         ["u" "ü"] 1/50
         ["Ö" "O"] 1/50
         ["O" "Ö"] 1/50
         ["Ä" "A"] 1/50
         ["A" "Ä"] 1/50
         ["Ü" "U"] 1/50
         ["U" "Ü"] 1/50
         ["n" "lt"] 1/50
         ["I" "J"] 1
         ["J" "I"] 1
         ["a" "a"] 1
         ["b" "b"] 1
         ["c" "c"] 1
         ["d" "d"] 1
         ["e" "e"] 1
         ["f" "f"] 1
         ["g" "g"] 1
         ["h" "h"] 1
         ["i" "i"] 1
         ["j" "j"] 1
         ["k" "k"] 1
         ["l" "l"] 1
         ["m" "m"] 1
         ["n" "n"] 1
         ["o" "o"] 1
         ["p" "p"] 1
         ["q" "q"] 1
         ["r" "r"] 1
         ["s" "s"] 1
         ["t" "t"] 1
         ["u" "u"] 1
         ["v" "v"] 1
         ["w" "w"] 1
         ["x" "x"] 1
         ["y" "y"] 1
         ["z" "z"] 1
         ["A" "A"] 1
         ["B" "B"] 1
         ["C" "C"] 1
         ["D" "D"] 1
         ["E" "E"] 1
         ["F" "F"] 1
         ["G" "G"] 1
         ["H" "H"] 1
         ["I" "I"] 1
         ["J" "J"] 1
         ["K" "K"] 1
         ["L" "L"] 1
         ["M" "M"] 1
         ["N" "N"] 1
         ["O" "O"] 1
         ["P" "P"] 1
         ["Q" "Q"] 1
         ["R" "R"] 1
         ["S" "S"] 1
         ["T" "T"] 1
         ["U" "U"] 1
         ["V" "V"] 1
         ["W" "W"] 1
         ["X" "X"] 1
         ["Y" "Y"] 1
         ["Z" "Z"] 1
         ["0" "0"] 1
         ["1" "1"] 1
         ["2" "2"] 1
         ["3" "3"] 1
         ["4" "4"] 1
         ["5" "5"] 1
         ["6" "6"] 1
         ["7" "7"] 1
         ["8" "8"] 1
         ["9" "9"] 1
         ["ä" "ä"] 1
         ["ö" "ö"] 1
         ["ü" "ü"] 1
         ["ß" "ß"] 1
         ["Ä" "Ä"] 1
         ["Ö" "Ö"] 1
         ["Ü" "Ü"] 1
         ["-" "-"] 1
         ["," ","] 1
         ["'" "'"] 1
         ["¬" "¬"] 1})

(def simple-subst
        {["u" "n"] 1/2
         ["n" "u"] 1/2
         ["c" "e"] 1/4
         ["e" "c"] 1/4
         ["N" "U"] 1/8
         ["N" "R"] 1/8
         ["rn" "m"] 1/8
         ["iii" "m"] 1/8
         ["in" "m"] 1/8
         ["m" "in"] 1/8
         ["n" "m"] 1/8
         ["m" "n"] 1/8
         ["m" "en"] 1/8
         ["f" "s"] 1/4
         ["s" "f"] 1/4
         ["v" "o"] 1/4
         ["o" "v"] 1/4
         ["n" "r"] 1/4
         ["a" "a"] 1
         ["b" "b"] 1
         ["c" "c"] 1
         ["d" "d"] 1
         ["e" "e"] 1
         ["f" "f"] 1
         ["g" "g"] 1
         ["h" "h"] 1
         ["i" "i"] 1
         ["j" "j"] 1
         ["k" "k"] 1
         ["l" "l"] 1
         ["m" "m"] 1
         ["n" "n"] 1
         ["o" "o"] 1
         ["p" "p"] 1
         ["q" "q"] 1
         ["r" "r"] 1
         ["s" "s"] 1
         ["t" "t"] 1
         ["u" "u"] 1
         ["v" "v"] 1
         ["w" "w"] 1
         ["x" "x"] 1
         ["y" "y"] 1
         ["z" "z"] 1
         ["A" "A"] 1
         ["B" "B"] 1
         ["C" "C"] 1
         ["D" "D"] 1
         ["E" "E"] 1
         ["F" "F"] 1
         ["G" "G"] 1
         ["H" "H"] 1
         ["I" "I"] 1
         ["J" "J"] 1
         ["K" "K"] 1
         ["L" "L"] 1
         ["M" "M"] 1
         ["N" "N"] 1
         ["O" "O"] 1
         ["P" "P"] 1
         ["Q" "Q"] 1
         ["R" "R"] 1
         ["S" "S"] 1
         ["T" "T"] 1
         ["U" "U"] 1
         ["V" "V"] 1
         ["W" "W"] 1
         ["X" "X"] 1
         ["Y" "Y"] 1
         ["Z" "Z"] 1
         ["0" "0"] 1
         ["1" "1"] 1
         ["2" "2"] 1
         ["3" "3"] 1
         ["4" "4"] 1
         ["5" "5"] 1
         ["6" "6"] 1
         ["7" "7"] 1
         ["8" "8"] 1
         ["9" "9"] 1
         ["ä" "ä"] 1
         ["ö" "ö"] 1
         ["ü" "ü"] 1
         ["ß" "ß"] 1
         ["Ä" "Ä"] 1
         ["Ö" "Ö"] 1
         ["Ü" "Ü"] 1
         ["-" "-"] 1
         ["," ","] 1
         ["'" "'"] 1
         ["¬" "¬"] 1})


(def simple-subst
        {["u" "n"] 1/2
         ["n" "u"] 1/2
         ["c" "e"] 1/4
         ["e" "c"] 1/4
         ["N" "U"] 1/8
         ["N" "R"] 1/8
         ["rn" "m"] 1/8
         ["iii" "m"] 1/8
         ["in" "m"] 1/8
         ["m" "in"] 1/8
         ["n" "m"] 1/8
         ["m" "n"] 1/8
         ["m" "en"] 1/8
         ["f" "s"] 1/4
         ["s" "f"] 1/4
         ["v" "o"] 1/4
         ["ö" "v"] 1/8
         ["v" "ö"] 1/8
         ["o" "v"] 1/4
         ["n" "r"] 1/4
         ["d" "b"] 1/8
         ["b" "d"] 1/8
         ["ü" "n"] 1/8
         ["n" "ü"] 1/8
         ["zu" "m"] 1/16
         ["n" "il"] 1/16
         ["m" "n?"] 1/16
         ["en" "m"] 1/16
         ["s" "h"] 1/16
         ["b" "h"] 1/8
         ["h" "b"] 1/8
         ["t" "d"] 1/16
         ["d" "t"] 1/16
         ["im" "un"] 1/16
         ["s" "S"] 1/16
         ["S" "s"] 1/16
         ["t" "e"] 1/16
         ["y" "p"] 1/8
         ["p" "y"] 1/8
         ["l" "i"] 1/8
         ["i" "l"] 1/8
         ["ö" "s"] 1/8
         ["s" "ö"] 1/8
         ["k" "l"] 1/8
         ["l" "k"] 1/8
         ["l" "t"] 1/8
         ["t" "l"] 1/8
         ["h" "s"] 1/8
         ["A" "U"] 1/16
         ["U" "A"] 1/16
         ["m," "n"] 1/16
         ["m:" "n"] 1/16
         ["li" "u"] 1/16
         ["it" "n"] 1/16
         ["k" "t"] 1/16
         ["t" "k"] 1/16
         ["ö" "o"] 1/16
         ["o" "ö"] 1/16
         ["ä" "a"] 1/16
         ["a" "ä"] 1/16
         ["ü" "u"] 1/16
         ["u" "ü"] 1/16
         ["Ö" "O"] 1/16
         ["O" "Ö"] 1/16
         ["Ä" "A"] 1/16
         ["A" "Ä"] 1/16
         ["Ü" "U"] 1/16
         ["U" "Ü"] 1/16
         ["n" "lt"] 1/16
         ["I" "J"] 1
         ["J" "I"] 1
         ["a" "a"] 1
         ["b" "b"] 1
         ["c" "c"] 1
         ["d" "d"] 1
         ["e" "e"] 1
         ["f" "f"] 1
         ["g" "g"] 1
         ["h" "h"] 1
         ["i" "i"] 1
         ["j" "j"] 1
         ["k" "k"] 1
         ["l" "l"] 1
         ["m" "m"] 1
         ["n" "n"] 1
         ["o" "o"] 1
         ["p" "p"] 1
         ["q" "q"] 1
         ["r" "r"] 1
         ["s" "s"] 1
         ["t" "t"] 1
         ["u" "u"] 1
         ["v" "v"] 1
         ["w" "w"] 1
         ["x" "x"] 1
         ["y" "y"] 1
         ["z" "z"] 1
         ["A" "A"] 1
         ["B" "B"] 1
         ["C" "C"] 1
         ["D" "D"] 1
         ["E" "E"] 1
         ["F" "F"] 1
         ["G" "G"] 1
         ["H" "H"] 1
         ["I" "I"] 1
         ["J" "J"] 1
         ["K" "K"] 1
         ["L" "L"] 1
         ["M" "M"] 1
         ["N" "N"] 1
         ["O" "O"] 1
         ["P" "P"] 1
         ["Q" "Q"] 1
         ["R" "R"] 1
         ["S" "S"] 1
         ["T" "T"] 1
         ["U" "U"] 1
         ["V" "V"] 1
         ["W" "W"] 1
         ["X" "X"] 1
         ["Y" "Y"] 1
         ["Z" "Z"] 1
         ["0" "0"] 1
         ["1" "1"] 1
         ["2" "2"] 1
         ["3" "3"] 1
         ["4" "4"] 1
         ["5" "5"] 1
         ["6" "6"] 1
         ["7" "7"] 1
         ["8" "8"] 1
         ["9" "9"] 1
         ["ä" "ä"] 1
         ["ö" "ö"] 1
         ["ü" "ü"] 1
         ["ß" "ß"] 1
         ["Ä" "Ä"] 1
         ["Ö" "Ö"] 1
         ["Ü" "Ü"] 1
         ["-" "-"] 1
         ["," ","] 1
         ["'" "'"] 1
         ["¬" "¬"] 1})

(def subst
  (assoc simple-subst
    ["f" "s"] 1/4
    ["s" "f"] 1/4
    ["f" "f"] 3/4
    ["ii" "u"] 1/4))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/simple-subst</span>","value":"#'suub.bote.dict/simple-subst"}
;; <=

;; @@
#_(defonce dta-prfx (time (prefixes dta-dict)))
;;(def pots-prfx (time (prefixes pots-dict)))

;; @@
;; ->
;;; &quot;Elapsed time: 46571.535839 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta-prfx</span>","value":"#'suub.bote.dict/dta-prfx"}
;; <=

;; @@

(def dta2 {:matcher simple-matcher
         ;;  :dict dta-dict
        ;;   :prefixes dta-prfx
            :substs simple-subst})
#_(def pots {:matcher simple-matcher
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
(defn correct-word [idx word]
  (if (and (not ((:dict idx) (apply str word)))
           (Character/isLetterOrDigit (first word)))
    (or (:word (first (transform idx word)))
        word)
    word))

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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/correct-word</span>","value":"#'suub.bote.dict/correct-word"}
;; <=
(defn text [p]
  (apply str (map (fn [l]
                    (str (apply str
                                (map :char l))
                         "\n"))
                  (abbyy/lines p))))
;; @@
;;error-codes aufwärmen - core.matrix ladezeit
(ec/error-codes "a" "b")
;; @@

;; @@
(defn correct-page [idx page]
  (->> page
       (partition-by #(Character/isLetterOrDigit %))
       (mapcat #(correct-word idx %))
       (apply str)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/correct-page</span>","value":"#'suub.bote.dict/correct-page"}
;; <=

(defn download-xml [vlid]
  (xml/parse
    (str "http://brema.suub.uni-bremen.de/grenzboten/download/fulltext/fr/" vlid)))

;; @@
#_(def files (rest (file-seq (io/file "/Users/ticking/Desktop/ocr-engine-results/abby_verbessert/unverbessert"))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/files</span>","value":"#'suub.bote.dict/files"}
;; <=

;; @@
#_(def f
(time
(doall
 (pmap (fn [f]
         (->> f
              slurp
     		  (correct-page dta)
       		  (spit (io/file "/Users/ticking/Desktop/ocr-engine-results/abbyydict2simplesubst/ocr-results" (.getName f)))))
       files))))
;; @@
;; ->
;;; &quot;Elapsed time: 30953.074 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/f</span>","value":"#'suub.bote.dict/f"}
;; <=

;; **
;;; ##Difficult words.
;;; 
;;; Due to the statistical approach, some words from the dictionary might not be transformed to themselves, but instead to other words from the dictionary that are more likely and/or contain more expected characters.
;; **

;; @@
(defn evaluate-algorithm [dta ocr-base-directory corrected-base-directory num-pages]
  (.mkdirs (io/file corrected-base-directory))
  (.mkdir (io/file corrected-base-directory "ground-truth"))
  (doseq [f (take num-pages (ec/get-files-sorted (io/file ocr-base-directory "ground-truth")))]
    (println "f " f)
    (io/copy f (io/file corrected-base-directory "ground-truth" (.getName f))))
  (.mkdir (io/file corrected-base-directory  "edits"))
  (.mkdir (io/file corrected-base-directory "ocr-results"))
  (let [ocr-text (->> (ec/get-files-sorted 
                        (io/file ocr-base-directory "ocr-results"))
                      (take num-pages))]
    (println "starte Nachkorrektur")
    (doall 
      (pmap #(->> % slurp (correct-page dta)
                  (spit (io/file corrected-base-directory "ocr-results" (.getName %))))
            ocr-text))
    	
    (println "starte Auszählung")
    (ec/deploy-error-codes corrected-base-directory)
    (println "starte Auswärtung")
    (let [statistic (ec/gen-statistics-for-base-directories [corrected-base-directory])
          word-statistic (ec/gen-word-statistics-for-base-directories [corrected-base-directory])
          correction-statistic (ec/generate-correction-statistics ocr-base-directory corrected-base-directory)]
      (spit (clojure.java.io/file corrected-base-directory "statistics.edn") (pr-str (second (first statistic))))
      (spit (clojure.java.io/file corrected-base-directory "word-statistics.edn") (pr-str (second (first word-statistic))))
      (spit (clojure.java.io/file corrected-base-directory "correction-statistic.edn") (pr-str correction-statistic))
      (spit (clojure.java.io/file corrected-base-directory "parameters")
            (pr-str (dissoc dta :matcher)))
      {:statistic statistic
       :correction-statistic correction-statistic})))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/difficult</span>","value":"#'suub.bote.dict/difficult"}
;; <=

;; @@
#_(binding [*out* (clojure.java.io/writer "/home/kima/dummyoutput.txt")]
  (evaluate-algorithm 
   dta
   "/home/kima/programming/grenzbote-files/grenzbote/abby" 
   "/home/kima/programming/grenzbote-files/grenzbote/abby-corr-normal-10-pages"
   10))



;;prunen der wörterbuches
;;wann soll geprunt werden
;;checke ob Wort nächste Verbesserung des Wortes

;;operation auf wörterbuch
;; ſ -> s
;; uͤ -> ü

(defn clear-key? [dta key]
  (let [candidate (:word (first (transform dta key)))
        _ (prn "count key " (count key))
        res
        (or (not candidate)
            (and
             (not= candidate key)
             (> (count key) 4)
             (not (and (= (butlast key) (butlast candidate))
                       (= (count key) (count candidate))
                       (every? #{\n \m} [(last key) (last candidate)])))
             (> (/ (get (:dict dta) candidate)
                   (get (:dict dta) key))
                400)
             (< (:distance (ec/edits candidate key)) 3)))]
    (when res (prn "filter " key " for " candidate))
    res))

(defn clear-dict [dta]
  (let [kv-s (seq (:dict dta))]
    (->> (pmap (fn [kv]
                 (if (clear-key? dta (first kv)) [] (list kv)))
               kv-s)
         (apply concat)
         (into #{}))))
(comment
  (defonce cleaned-dict (into {} (seq (read-string (slurp "filtered1dict.edn")))))

  (defonce cleaned-prfx (prefixes cleaned-dict))

  (def dta-cleaned (assoc dta2
                     :dict cleaned-dict
                     :prefixes cleaned-prfx)))


(defn clear-all-consonants [dict]
  (into {}
        (for [[k v] dict
              :when (some #{\a \e \i \o \u} k)]
          [k v])))

(defn clear-all-vocals [dict]
  (into {}
        (for [[k v] dict
              :when (not (every? #{\a \e \i \o \u} k))]
          [k v])))

(defn clear-double-u [dict]
  (into {}
        (for [[k v] dict
              :when (not (some #{[\u \u]} (partition 2 1 k)))]
          [k v])))

(def new-simple-subst
  (merge 
    {["t" "e"] 1/50
     ["k" "t"] 1/50
     ["m" "a"] 1/50
     ["c" "n"] 1/50
     ["h" "d"] 1/50
     ["N" "n"] 1/50
     ["m" "zu"] 1/100
     ["n" "m"] 1/50
     ["m" "u"] 1/50
     ["il" "n"] 1/100
     ["n?" "m"] 1/100
     ["v" "p"] 1/50
     ["w" "o"] 1/50
     ["n:" "m"] 1/100
     ["c" "a"] 1/50
     ["m" "en"] 1/100
     ["n" "ü"] 1/50
     ["ü" "n"] 1/50
     }
    simple-subst))


(defn generate-cheater-dict []
  (let [gt (fs/list-dir "/home/kima/programming/grenzbote-files/grenzbote/abby-cleaned-neu/ground-truth")
        words (apply concat (pmap (comp #(.split % "\\s+") #(.replaceAll (.replace (.replace % "-\n" "") "¬\n" "") "[?!.,:]" "") slurp) gt))
        word-count (count words)]
    (into {} (for [[k v] (frequencies words)]
               [k (/ v word-count)]))))

#_(def cheater-dict (dissoc (generate-cheater-dict)
                          "iu"
                          "nnd"
                          "uud"))

(def alphabet "abcdefghijklmnopqrstuvwxyzüäöß'")
(def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ")

(def bar-map
  {\A 2 \a 2 \B 2 \b 2 \C 2 \c 1 \D 2 \d 2 \E 2 \e 1 \F 2 \f 1 \G 3 \g 2
   \H 2 \h 2 \I 2 \J 2 \i 1 \j 1 \K 2 \k 1 \L 2 \l 1 \M 4 \m 3 \N 3 \n 2
   \O 2 \o 2 \P 3 \p 2 \Q 2 \q 2 \R 3 \r 1 \S 2 \s 2 \T 2 \t 1 \U 2 \u 2
   \V 3 \v 2 \W 4 \w 3 \X 2 \x 1 \Y 3 \y 2 \Z 2 \z 1 \ä 2 \ö 2 \ü 2 \ß 2})

(def by-bar-count
  (group-by bar-map alphabet))
(defn base-substitution [alphabet]
  (into {}
        (for [c (map str alphabet)
              d (map str alphabet)]
          [[c d] (if (= c d) 1 1/1000)])))

(def base-subs
  (apply merge (map base-substitution (vals by-bar-count))))

(defn get-errors [cat-key error-key correction-statistic base-directory]
  (mapcat (fn [fcs]
         (let [gt (slurp (clojure.java.io/file base-directory "ground-truth" (first (:pages fcs))))
               ocr (slurp (clojure.java.io/file base-directory "ocr-results" (first (:pages fcs))))]
           (as-> fcs x
                 (cat-key x)
                 (group-by last x)
                 (get x error-key)
                 (map first x)
                 (ec/error-words gt ocr x))))
          correction-statistic))

#_(def gerrit-cleaned-dict-1800+
  (dissoc (read-dict "resources/output.merged-1800+.fuwv")
          "nnd"))


#_(binding [*out* (clojure.java.io/writer "/home/kima/dummyoutput.txt")]
  (evaluate-algorithm 
   (assoc dta2 :dict cheater-dict :prefixes cheater-prfx :substs new-sim)))
(defn write-to [vlids texts dir]
  (doseq [[vlid text] (map vector vlids texts)]
    (spit (clojure.java.io/file dir (str vlid ".txt")) text)))

(defn download-vlids-to [vlids dir]
  (let [texts (map le/abby-plaintext vlids)]
    (write-to vlids texts dir)))

;;;;;;;;;;;;;;;;;;;;;laser stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-article-url [article]
  (-> (l/select (l/zip article) (l/attr? :xlink:href))
      first
      (get-in [:attrs :xlink:href])))

(defn get-article-doc [article]
  (le/doc-from-url (get-article-url article)))


(defn get-vlids [article-doc]
  (->>
   (l/select article-doc
             (l/descendant-of (l/and (l/element= :mets:structmap)
                                     (l/attr= :type "PHYSICAL"))
                              (l/and (l/element= :mets:div)
                                     (l/attr? :order))))
   (map #(get-in % [:attrs :id]))
   (map #(.substring % 4))
   (map #(Integer/parseInt %))))

(defn article-vlids [article]
  (-> article
      get-article-doc
      get-vlids))

(defn words-on-page
  "page as string"
  [page]
  (->> page
      ((comp #(.split % "[^abcdefghijklmnopqrstuvwxyzäöüßABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ]")
             #(.replaceAll (.replace (.replace % "-\n" "") "¬\n" "") "[?!.,:]" "")))
      (remove empty?)))


(defn page-statistic
  "with optional predicate to filter words by"
  ([dict page] (page-statistic dict page identity))
  ([dict page pred]
   (let [words (filter pred (words-on-page page))
         not-in-dict (remove #(get dict %) words)]
     {:all-words words
      :not-in-dict not-in-dict
      :error-rate (if (= 0 (count words)) 100
                      (double (* 100 (/ (count not-in-dict) (count words)))))})))


(defn site-statistic [dict vlid]
  (page-statistic dict (le/abby-plaintext vlid)))

(def grenzbote-vlids
  (for [jg le/jg-docs
        vol (le/volume-docs jg)
        artcl (le/select-article vol)
        vlid (article-vlids artcl)]
    vlid))

(defn save-vlids []
  (spit "vlids-scraper.edn" (pr-str (into [] grenzbote-vlids))))


(def grenzbote-vlids
  (clojure.set/difference
   (read-string (slurp "vlids-maik+mn.edn"))
   (read-string (slurp "maik-ohne-mn.edn"))))

(defn generate-site-statistic [dict vlids dir]
  (doseq [vlid vlids]
    (spit (io/file dir (str vlid ".edn"))
          (pr-str (site-statistic dict vlid)))))


(defn get-lines [vlid]
  (.split (le/abby-plaintext vlid) "\n"))


(defn generate-line-statistic [dict vlids dir]
  (doseq [vlid vlids
          [line num] (map vector (get-lines vlid) (range))]
    (spit (io/file dir (str vlid "-" num ".edn"))
          (pr-str (page-statistic dict line)))))

(comment
  (def processed
                  (->> (io/file "/home/kima/programming/grenzbote-files/grenzbote/site-statistics/")
                       file-seq
                       rest
                       (map (memfn getName))
                       (map #(.substring % 0 (- (count %) 4)))
                       (map #(Integer/parseInt %))))
  (do (def processed
                  (->> (io/file "/home/kima/programming/grenzbote-files/grenzbote/site-statistics/")
                       file-seq
                       rest
                       (map (memfn getName))
                       (map #(.substring % 0 (- (count %) 4)))
                       (map #(Integer/parseInt %))))
                    
                    (def remaining (clojure.set/difference (into #{} grenzbote-vlids) (into #{} processed)))
                    
                    [(count remaining)
                    (double (* 100 (/ (- (count grenzbote-vlids) (count remaining) ) (count grenzbote-vlids))))])

  (defn ex-watcher [] (try @f (catch Exception e (do (def processed
                  (->> (io/file "/home/kima/programming/grenzbote-files/grenzbote/site-statistics/")
                       file-seq
                       rest
                       (map (memfn getName))
                       (map #(.substring % 0 (- (count %) 4)))
                       (map #(Integer/parseInt %))))
                    
                    (def remaining (clojure.set/difference (into #{} grenzbote-vlids) (into #{} processed)))
                    
                    [(count remaining)
                    (double (* 100 (/ (- (count grenzbote-vlids) (count remaining) ) (count grenzbote-vlids))))]
                    (def idiot (Integer/parseInt (second (re-find #"fulltext/fr/([0-9]+)" (.getMessage e)))))
                    (def wrong-vlids (clojure.set/union wrong-vlids #{idiot}))
                    (spit "wrong-vlids.edn" (pr-str wrong-vlids))
                    (def remaining-filtered (clojure.set/difference remaining wrong-vlids))
                    (def f (future (generate-site-statistic dict remaining-filtered "/home/kima/programming/grenzbote-files/grenzbote/site-statistics/") ))
                    (ex-watcher))))))


(defn generate-entity-statistics
  ([dir entities] (generate-entity-statistics dir entities 0))
  ([dir entities allowed-errors]
   (let [gt (ec/get-files-sorted 
             (io/file dir "ground-truth"))
         ocr (ec/get-files-sorted
              (io/file dir "ocr-results"))
         entities (into #{} entities)
         matching-entities (fn [word]
                             (if (= 0 allowed-errors)
                               [(entities word)]
                               (for [ent entities
                                     :when (<= (:distance (ec/edits word ent)) allowed-errors)]
                                 ent)))
         build-map (fn [files]
                     (merge (into {} (for [ent entities] [ent {}]))
                            (apply merge-with #(merge-with + %1 %2)
                                   (for [f files
                                         word (words-on-page (slurp f))
                                         ent (matching-entities word)]
                                     {ent {word 1}}))))
         entities-in-gt (build-map gt)
         entities-in-ocr (build-map ocr)]
     (merge-with vector entities-in-gt entities-in-ocr))))

(defn generate-single-entity-statistics
  "only ocr"
  ([dir entities] (generate-single-entity-statistics dir entities 0))
  ([dir entities allowed-errors]
   (let [ocr (ec/get-files-sorted
              (io/file dir "ocr-results"))
         entities (into #{} entities)
         matching-entities (fn [word]
                             (if (= 0 allowed-errors)
                               [(entities word)]
                               (for [ent entities
                                     :when (<= (:distance (ec/edits word ent)) allowed-errors)]
                                 ent)))
         build-map (fn [files]
                     (merge (into {} (for [ent entities] [ent {}]))
                            (apply merge-with #(merge-with + %1 %2)
                                   (for [f files
                                         word (words-on-page (slurp f))
                                         ent (matching-entities word)]
                                     {ent {word 1}}))))]
     (build-map ocr))))



;;;;;;;;;;;;;;;; fehlertyp statistik ;;;

(defn error-type-statistic [base-directory]
  (let [gt (map slurp (ec/get-files-sorted (io/file base-directory "ground-truth")))
        ocr (map slurp (ec/get-files-sorted (io/file base-directory "ocr-results")))
        error-codes (map (comp read-string slurp) (ec/get-files-sorted (io/file base-directory "edits")))
        error-types (mapcat (fn [gt ocr edits]
                         (map #(second (ec/augment-error-code gt ocr %)) edits))
                       gt ocr error-codes)]
    (sort-by second (frequencies error-types))))
;; (def error-type-frequencies (error-type-statistic "/home/noelte/clojure/ocr-engine-results/abby-more-text/"))
;; suub.bote.dict> (spit "error-type-frequencies.edn" (pr-str error-type-frequencies))

(defn sorted-error-number-deltas [base-directory-ocr base-directory-corrected]
  (let [ocr-files (ec/get-files-sorted (io/file base-directory-ocr "edits"))
        corr-files (ec/get-files-sorted (io/file base-directory-corrected "edits"))
        ; list-vlid-error-number-ocr (map (fn [x] [(first  (string/split (.getName x) #"\.")) (count (read-string (slurp x)))]) ocr-files)
        ; list-vlid-error-number-corr (map (fn [x] [(first  (string/split (.getName x) #"\.")) (count (read-string (slurp x)))]) corr-files)

        list-vlid-error-deltas (map (fn [ocr corr] 
                                      [(first  (string/split (.getName ocr) #"\."))
                                       (- (count (read-string (slurp ocr))) (count (read-string (slurp corr))))])
                                    ocr-files corr-files)]
    (sort-by second  list-vlid-error-deltas)))
;; suub.bote.dict> (def sorted-correction-quality (sorted-error-number-deltas "/home/noelte/clojure/ocr-engine-results/abby-more-text/" "/home/noelte/clojure/ocr-engine-results/abby-more-text-unser-algorithmus"))
;;suub.bote.dict> (count sorted-correction-quality)
;;362
;; suub.bote.dict> (spit "sorted-correction-quality.edn" (pr-str sorted-correction-quality))

;; 9.4.2014
;; suub.bote.dict> (def sorted-correction-quality-overproof (sorted-error-number-deltas "/home/noelte/clojure/ocr-engine-results/abby-more-text/" "/home/noelte/clojure/ocr-engine-results/australier"))
;; suub.bote.dict> (count sorted-correction-quality-overproof)
;; 200  <-- SELTSAM:
;;          die 3 Verzeichnisse edit ground-truth und ocr-results
;;          haben alle unterschiedlich viele Dateien!?!?!?
;; suub.bote.dict> (spit "sorted-correction-quality-overproof.edn" (pr-str sorted-correction-quality-overproof))



(defn get-current-params []
  (let [dict-unfiltered (read-dict "resources/current-params/dict.fuwv")
        todelete (line-seq (io/reader "resources/current-params/removed-words-from-dict.txt"))
        dict (reduce dissoc dict-unfiltered todelete)
        substs (read-string (slurp "resources/current-params/substs.edn"))]
    {:matcher simple-matcher
     :dict dict
     :prefixes (prefixes dict)
     :substs substs}))

(comment 1 : to run
 (def f (future (def res (binding [*out* (clojure.java.io/writer "status.txt")]
                  (evaluate-algorithm
                 (get-current-params)
                 "/home/noelte/clojure/ocr-engine-results/<start-dir>"
                 "/home/noelte/clojure/ocr-engine-results/<result-dir>"
                 num-pages)))))





 (def f (future
          (do 
            (nummer 1)
            (spit "progess"  "nummer 1 erledigt" :append true)
            (nummer 2)))))

(comment 2 : to run 
zum alles Evaluieren
suub.bote.dict> (def f (future (do 
                          (def res (binding [*out* (clojure.java.io/writer "status.txt")]
                  (evaluate-algorithm
                 (get-current-params)
                 "/home/noelte/clojure/ocr-engine-results/abby-neu-neue-seiten-ohne-schrottseiten"
                 "/home/noelte/clojure/ocr-engine-results/abby-neu-neu-seiten-ohne-schrottseiten-unser-algorithmus-7.4.2015"
                 152)))
                          (def res (binding [*out* (clojure.java.io/writer "status.txt")]
                  (evaluate-algorithm
                 (get-current-params)
                 "/home/noelte/clojure/ocr-engine-results/1870-abby"
                 "/home/noelte/clojure/ocr-engine-results/1870-abby-unser-algorithmus-7.4.2015"
                 10)))
                          (def res (binding [*out* (clojure.java.io/writer "status.txt")]
                  (evaluate-algorithm
                 (get-current-params)
                 "/home/noelte/clojure/ocr-engine-results/1900-abby"
                 "/home/noelte/clojure/ocr-engine-results/1900-abby-unser-algorithmus-7.4.2015"
                 8)))
                          (def res (binding [*out* (clojure.java.io/writer "status.txt")]
                  (evaluate-algorithm
                 (get-current-params)
                 "/home/noelte/clojure/ocr-engine-results/abby-more-text-ohne-schrottseiten"
                 "/home/noelte/clojure/ocr-engine-results/abby-more-text-ohne-schrottseiten-unser-algorithmus-7.4.2015"
                 352))))))
---------------------------------
Mail:

(def res (generate-single-entity-statistics "/home/noelte/clojure/ocr-engine-results/abby-more-text-unser-algorithmus" (read-string (slurp "entities.edn"))))
                      (spit "line-statistics-ocr-verbessert.edn" (pr-str res))

 (def res (generate-single-entity-statistics
 "/home/noelte/clojure/ocr-engine-results/abby-more-text-unser-algorithmus"
 (read-string (slurp "entities.edn"))
 1))

(spit "entity-statistics-ocr-verbessert-fehler-1.edn" (pr-str res))
---------------------------------
(def f (future (do 
                      (def res (generate-single-entity-statistics
                                "/home/noelte/clojure/ocr-engine-results/abby-more-text-unser-algorithmus"
                                (read-string (slurp "entities.edn"))
                                1))

                      (spit "entity-statistics-ocr-verbessert-fehler-1.edn" (pr-str res)))))



)
