;; gorilla-repl.fileformat = 1

;; **
;;; # Dictionary error correction.
;;; 
;;; This code uses a weighted finite state transducer aproach to give all possible matches for a given word.
;; **

;; @@
(ns suub.bote.dict
  (:require ;[gorilla-plot.core :as plot]
            ;[gorilla-repl.table :as table]
            [clojure.core.reducers :as r]
            [suub.bote.util :as util]
            [suub.bote.abbyy :as abbyy]
            [clojure.test :as t]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [suub.bote.clojure.xml :as xml]
            [clojure.string :as string]
            [clojure.data.priority-map :as pm]
            [instaparse.core :as insta]
            [instaparse.combinators :as instac]
            [clojure.edn :as edn]
            [me.raynes.fs :as fs]
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
  (when-let [rest (util/drop-prefix p q)] [p rest]))
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
(defonce dta-dict (read-dict "resources/dta-freq.d/dta-core.fuw"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta-dict</span>","value":"#'suub.bote.dict/dta-dict"}
;; <=

;; @@
(def dta-dict (dissoc dta-dict
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
(defonce dta-prfx (time (prefixes dta-dict)))
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
           :dict dta-dict
           :prefixes dta-prfx
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
  (if (and (not (dta-dict (apply str word)))
           (Character/isLetterOrDigit (first word)))
    (or (:word (first (transform idx word)))
        word)
    word))

defn correct [p idx]
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

defn download-xml [vlid]
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
          correction-statistic (ec/generate-correction-statistics ocr-base-directory corrected-base-directory)]
      (spit (clojure.java.io/file corrected-base-directory "statistics.edn") (pr-str (second (first statistic))))
      (spit (clojure.java.io/file corrected-base-directory "correction-statistic.edn") (pr-str correction-statistic))
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
;; @@
;; =>
;;; {"type":"list-like","open":"<center><table>","close":"</table></center>","separator":"\n","items":[{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Geldadel&quot;</span>","value":"\"Geldadel\""},{"type":"html","content":"<span class='clj-string'>&quot;Entladet&quot;</span>","value":"\"Entladet\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"a844128e-ef9b-4884-be28-71a000d88ee2","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"a844128e-ef9b-4884-be28-71a000d88ee2","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"a844128e-ef9b-4884-be28-71a000d88ee2"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"a844128e-ef9b-4884-be28-71a000d88ee2","values":[{"x":"Entladet","y":2.347246792921289E-26},{"x":"Mitchel","y":1.153748337531145E-26},{"x":"Michiel","y":5.651981202360246E-27},{"x":"Midamus","y":5.202061506481657E-27},{"x":"Etstoel","y":1.875496565622425E-27},{"x":"tribades","y":6.158034207805904E-28},{"x":"Emsiedel","y":5.270781073948562E-29},{"x":"Etliches","y":2.141320151051957E-29},{"x":"Entbindet","y":5.976982678339237E-30},{"x":"Euklides","y":5.226733106155217E-30}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a844128e-ef9b-4884-be28-71a000d88ee2\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a844128e-ef9b-4884-be28-71a000d88ee2\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"a844128e-ef9b-4884-be28-71a000d88ee2\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"a844128e-ef9b-4884-be28-71a000d88ee2\", :values ({:x \"Entladet\", :y 499353881/21274025488326446046942094329262500} {:x \"Mitchel\", :y 39061/3385573675762334787804002359500} {:x \"Michiel\", :y 429671/76021307328582583880012389737000} {:x \"Midamus\", :y 573311/110208423965319670568416643547820} {:x \"Etstoel\", :y 3551/1893365237286650392883251200000} {:x \"tribades\", :y 115235511/187130352172983768066764133083252000} {:x \"Emsiedel\", :y 36571749/693858243909238602872538120715000000} {:x \"Etliches\", :y 39061/1824155065313828428762793379300000} {:x \"Entbindet\", :y 230011/38482795145712354382232493073560000} {:x \"Euklides\", :y 690033/132019941708404544658657733436087500})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Geldadel\" \"Entladet\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a844128e-ef9b-4884-be28-71a000d88ee2\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a844128e-ef9b-4884-be28-71a000d88ee2\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"a844128e-ef9b-4884-be28-71a000d88ee2\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"a844128e-ef9b-4884-be28-71a000d88ee2\", :values ({:x \"Entladet\", :y 499353881/21274025488326446046942094329262500} {:x \"Mitchel\", :y 39061/3385573675762334787804002359500} {:x \"Michiel\", :y 429671/76021307328582583880012389737000} {:x \"Midamus\", :y 573311/110208423965319670568416643547820} {:x \"Etstoel\", :y 3551/1893365237286650392883251200000} {:x \"tribades\", :y 115235511/187130352172983768066764133083252000} {:x \"Emsiedel\", :y 36571749/693858243909238602872538120715000000} {:x \"Etliches\", :y 39061/1824155065313828428762793379300000} {:x \"Entbindet\", :y 230011/38482795145712354382232493073560000} {:x \"Euklides\", :y 690033/132019941708404544658657733436087500})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;angezogeuen&quot;</span>","value":"\"angezogeuen\""},{"type":"html","content":"<span class='clj-string'>&quot;angezogenen&quot;</span>","value":"\"angezogenen\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535","values":[{"x":"angezogenen","y":1.804763011965474E-8},{"x":"angezogeuen","y":1.03816123584215E-8},{"x":"angezogenem","y":1.093047133800089E-11},{"x":"ungezogenen","y":1.749616392793156E-12},{"x":"Zugezogenen","y":8.240807283592811E-15},{"x":"angezogener","y":6.672056268078614E-15},{"x":"ungezogener","y":6.342993223743687E-17},{"x":"angenommen","y":2.139287695804525E-17},{"x":"ungezügelten","y":2.1260159486873E-18},{"x":"angenomenen","y":2.583025465752901E-19}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535\", :values ({:x \"angezogenen\", :y 4818013969845326116723920379525361152/266961032440390977641033575906375380903515625} {:x \"angezogeuen\", :y 24290957649746497222799478277087535104/2339805880927718587128980254547668603236328125} {:x \"angezogenem\", :y 958949802752232651003393696058112/87731788785571120605275881672396148801953125} {:x \"ungezogenen\", :y 58401585127461588362432063559424/33379651315581811413252824616188553474609375} {:x \"Zugezogenen\", :y 348428680001297586415599759104/42280891666403627790120244513838834401171875} {:x \"angezogener\", :y 2978104977491405748457744397696/446354895377557397117237431053543112873828125} {:x \"ungezogener\", :y 21948263307168351900195260416/346023754605468512435160642849249227094140625} {:x \"angenommen\", :y 593328128992617266793212096/27734845114858824858483596194374927385546875} {:x \"ungezügelten\", :y 1938448789413887027430697808/911775281182991454731640200746887569994140625} {:x \"angenomenen\", :y 10718057613306394166241602176/41494200329853478937083536957968148441708984375})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"angezogeuen\" \"angezogenen\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535\", :values ({:x \"angezogenen\", :y 4818013969845326116723920379525361152/266961032440390977641033575906375380903515625} {:x \"angezogeuen\", :y 24290957649746497222799478277087535104/2339805880927718587128980254547668603236328125} {:x \"angezogenem\", :y 958949802752232651003393696058112/87731788785571120605275881672396148801953125} {:x \"ungezogenen\", :y 58401585127461588362432063559424/33379651315581811413252824616188553474609375} {:x \"Zugezogenen\", :y 348428680001297586415599759104/42280891666403627790120244513838834401171875} {:x \"angezogener\", :y 2978104977491405748457744397696/446354895377557397117237431053543112873828125} {:x \"ungezogener\", :y 21948263307168351900195260416/346023754605468512435160642849249227094140625} {:x \"angenommen\", :y 593328128992617266793212096/27734845114858824858483596194374927385546875} {:x \"ungezügelten\", :y 1938448789413887027430697808/911775281182991454731640200746887569994140625} {:x \"angenomenen\", :y 10718057613306394166241602176/41494200329853478937083536957968148441708984375})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Cassir&#x27;&quot;</span>","value":"\"Cassir'\""},{"type":"html","content":"<span class='clj-string'>&quot;nostin&#x27;&quot;</span>","value":"\"nostin'\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"ba79a82d-ecc3-40d2-aff2-2cb550f2be80","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"ba79a82d-ecc3-40d2-aff2-2cb550f2be80","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"ba79a82d-ecc3-40d2-aff2-2cb550f2be80"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"ba79a82d-ecc3-40d2-aff2-2cb550f2be80","values":[{"x":"nostin'","y":3.894053333712672E-25},{"x":"Entfiel","y":6.444844276057581E-29},{"x":"destill","y":5.898766437743017E-30},{"x":"duftet'","y":5.614307423859964E-31},{"x":"Etstoel","y":6.574542951452152E-32},{"x":"einstell","y":5.517729458297543E-33},{"x":"Leiden'","y":4.193375394113717E-34},{"x":"neidet'","y":8.556810673306993E-36}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"ba79a82d-ecc3-40d2-aff2-2cb550f2be80\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"ba79a82d-ecc3-40d2-aff2-2cb550f2be80\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"ba79a82d-ecc3-40d2-aff2-2cb550f2be80\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"ba79a82d-ecc3-40d2-aff2-2cb550f2be80\", :values ({:x \"nostin'\", :y 1969216/5056982612311856254713323832375} {:x \"Entfiel\", :y 3944/61196203213968278878202940539625} {:x \"destill\", :y 61538/10432350670175989233362630873915625} {:x \"duftet'\", :y 17/30279781131600581081213164875500} {:x \"Etstoel\", :y 1061/16138003931750900291220747440640000} {:x \"einstell\", :y 23342/4230363263805618492296967057927421875} {:x \"Leiden'\", :y 4/9538855036958627979383540545453125} {:x \"neidet'\", :y 1/116865972402486838388925389827312500})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Cassir'\" \"nostin'\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"ba79a82d-ecc3-40d2-aff2-2cb550f2be80\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"ba79a82d-ecc3-40d2-aff2-2cb550f2be80\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"ba79a82d-ecc3-40d2-aff2-2cb550f2be80\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"ba79a82d-ecc3-40d2-aff2-2cb550f2be80\", :values ({:x \"nostin'\", :y 1969216/5056982612311856254713323832375} {:x \"Entfiel\", :y 3944/61196203213968278878202940539625} {:x \"destill\", :y 61538/10432350670175989233362630873915625} {:x \"duftet'\", :y 17/30279781131600581081213164875500} {:x \"Etstoel\", :y 1061/16138003931750900291220747440640000} {:x \"einstell\", :y 23342/4230363263805618492296967057927421875} {:x \"Leiden'\", :y 4/9538855036958627979383540545453125} {:x \"neidet'\", :y 1/116865972402486838388925389827312500})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Citadellkirche&quot;</span>","value":"\"Citadellkirche\""},{"type":"html","content":"<span class='clj-string'>&quot;durchnittliche&quot;</span>","value":"\"durchnittliche\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"173de439-f40c-49dc-8c38-7cb09eb4a80b","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"173de439-f40c-49dc-8c38-7cb09eb4a80b","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"173de439-f40c-49dc-8c38-7cb09eb4a80b"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"173de439-f40c-49dc-8c38-7cb09eb4a80b","values":[{"x":"durchnittliche","y":1.766092635368297E-49}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"173de439-f40c-49dc-8c38-7cb09eb4a80b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"173de439-f40c-49dc-8c38-7cb09eb4a80b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"173de439-f40c-49dc-8c38-7cb09eb4a80b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"173de439-f40c-49dc-8c38-7cb09eb4a80b\", :values ({:x \"durchnittliche\", :y 22913230304/129739685479305131061931571963197036577882866321563093223995})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Citadellkirche\" \"durchnittliche\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"173de439-f40c-49dc-8c38-7cb09eb4a80b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"173de439-f40c-49dc-8c38-7cb09eb4a80b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"173de439-f40c-49dc-8c38-7cb09eb4a80b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"173de439-f40c-49dc-8c38-7cb09eb4a80b\", :values ({:x \"durchnittliche\", :y 22913230304/129739685479305131061931571963197036577882866321563093223995})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Generalpausen&quot;</span>","value":"\"Generalpausen\""},{"type":"html","content":"<span class='clj-string'>&quot;Mitrailleusen&quot;</span>","value":"\"Mitrailleusen\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"580b2c45-a60d-462f-a695-4182c88429f4","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"580b2c45-a60d-462f-a695-4182c88429f4","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"580b2c45-a60d-462f-a695-4182c88429f4"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"580b2c45-a60d-462f-a695-4182c88429f4","values":[{"x":"Mitrailleusen","y":1.269230735662661E-30},{"x":"Milchspeisen","y":7.054319995127389E-39},{"x":"Mittelwasser","y":1.823583429755969E-39},{"x":"Mittelwänden","y":1.779538914598228E-39},{"x":"Mittelwinden","y":8.118565431675147E-42},{"x":"Mittelspindel","y":1.542353005774674E-45},{"x":"Kroninsignien","y":2.693694307936421E-46},{"x":"Kroninsignien","y":4.290785636220466E-49}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"580b2c45-a60d-462f-a695-4182c88429f4\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"580b2c45-a60d-462f-a695-4182c88429f4\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"580b2c45-a60d-462f-a695-4182c88429f4\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"580b2c45-a60d-462f-a695-4182c88429f4\", :values ({:x \"Mitrailleusen\", :y 251920327200157263791/198482687285878287595291064121698638543204164328125} {:x \"Milchspeisen\", :y 209257948856/29663801613839485211551560727394524314053097459375} {:x \"Mittelwasser\", :y 1436455009/787710057878857587352917933300624143528400000000} {:x \"Mittelwänden\", :y 1544198513/867752034154666431509576201803229978881020000000} {:x \"Mittelwinden\", :y 1544198513/190205834515443119947237128349822467843011640000000} {:x \"Mittelspindel\", :y 737/477841322473274402360230381446250935956011500000} {:x \"Kroninsignien\", :y 203747/756385011468082986818817497582065555042569370125000} {:x \"Kroninsignien\", :y 17929736/41786603946482374726788399321511384586523708043500421875})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Generalpausen\" \"Mitrailleusen\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"580b2c45-a60d-462f-a695-4182c88429f4\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"580b2c45-a60d-462f-a695-4182c88429f4\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"580b2c45-a60d-462f-a695-4182c88429f4\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"580b2c45-a60d-462f-a695-4182c88429f4\", :values ({:x \"Mitrailleusen\", :y 251920327200157263791/198482687285878287595291064121698638543204164328125} {:x \"Milchspeisen\", :y 209257948856/29663801613839485211551560727394524314053097459375} {:x \"Mittelwasser\", :y 1436455009/787710057878857587352917933300624143528400000000} {:x \"Mittelwänden\", :y 1544198513/867752034154666431509576201803229978881020000000} {:x \"Mittelwinden\", :y 1544198513/190205834515443119947237128349822467843011640000000} {:x \"Mittelspindel\", :y 737/477841322473274402360230381446250935956011500000} {:x \"Kroninsignien\", :y 203747/756385011468082986818817497582065555042569370125000} {:x \"Kroninsignien\", :y 17929736/41786603946482374726788399321511384586523708043500421875})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;quineutrarum&quot;</span>","value":"\"quineutrarum\""},{"type":"html","content":"<span class='clj-string'>&quot;absolutioni&quot;</span>","value":"\"absolutioni\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"e0e8623a-f358-4ac4-a505-022c1bb7dc84","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"e0e8623a-f358-4ac4-a505-022c1bb7dc84","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"e0e8623a-f358-4ac4-a505-022c1bb7dc84"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"e0e8623a-f358-4ac4-a505-022c1bb7dc84","values":[{"x":"absolutioni","y":3.724000657591841E-43}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"e0e8623a-f358-4ac4-a505-022c1bb7dc84\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"e0e8623a-f358-4ac4-a505-022c1bb7dc84\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"e0e8623a-f358-4ac4-a505-022c1bb7dc84\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"e0e8623a-f358-4ac4-a505-022c1bb7dc84\", :values ({:x \"absolutioni\", :y 1888256/5070503938151982144587936007280784553756869558625})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"quineutrarum\" \"absolutioni\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"e0e8623a-f358-4ac4-a505-022c1bb7dc84\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"e0e8623a-f358-4ac4-a505-022c1bb7dc84\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"e0e8623a-f358-4ac4-a505-022c1bb7dc84\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"e0e8623a-f358-4ac4-a505-022c1bb7dc84\", :values ({:x \"absolutioni\", :y 1888256/5070503938151982144587936007280784553756869558625})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Oetaeus&quot;</span>","value":"\"Oetaeus\""},{"type":"html","content":"<span class='clj-string'>&quot;Dolaeus&quot;</span>","value":"\"Dolaeus\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"a39f0030-1cea-4977-9ce1-e8c37e72268b","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"a39f0030-1cea-4977-9ce1-e8c37e72268b","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"a39f0030-1cea-4977-9ce1-e8c37e72268b"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"a39f0030-1cea-4977-9ce1-e8c37e72268b","values":[{"x":"Dolaeus","y":7.287229936805399E-18},{"x":"Ducaeus","y":2.498461819315061E-18},{"x":"Decanus","y":4.379602224839361E-19},{"x":"Dekanus","y":4.048264764561325E-19},{"x":"Duraeus","y":2.445986126458842E-19},{"x":"Dechens","y":6.947853876723156E-20},{"x":"Deittens","y":2.156874800243499E-20},{"x":"Derieus","y":1.413331165309708E-20},{"x":"Ducatus","y":5.561608304539266E-21},{"x":"Deliciis","y":2.464420395751682E-21}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a39f0030-1cea-4977-9ce1-e8c37e72268b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a39f0030-1cea-4977-9ce1-e8c37e72268b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"a39f0030-1cea-4977-9ce1-e8c37e72268b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"a39f0030-1cea-4977-9ce1-e8c37e72268b\", :values ({:x \"Dolaeus\", :y 3852833507776/528710297491315157085497345625} {:x \"Ducaeus\", :y 1864771417763584/746367786510662248672466261705625} {:x \"Decanus\", :y 932385708881792/2128927836399550751523039607378125} {:x \"Dekanus\", :y 42381168585536/104689715348023885283268717095625} {:x \"Duraeus\", :y 169524674342144/693072918559730737690395852470625} {:x \"Dechens\", :y 24545867944928/353287049216191388080016457234375} {:x \"Deittens\", :y 139465158778/6466075766764727757910225903125} {:x \"Derieus\", :y 93517728286048/6616830547676709123339673840640625} {:x \"Ducatus\", :y 316277377504/56867970591503392193505773577675} {:x \"Deliciis\", :y 22613414483/9175956554321000722155119799375})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Oetaeus\" \"Dolaeus\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a39f0030-1cea-4977-9ce1-e8c37e72268b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a39f0030-1cea-4977-9ce1-e8c37e72268b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"a39f0030-1cea-4977-9ce1-e8c37e72268b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"a39f0030-1cea-4977-9ce1-e8c37e72268b\", :values ({:x \"Dolaeus\", :y 3852833507776/528710297491315157085497345625} {:x \"Ducaeus\", :y 1864771417763584/746367786510662248672466261705625} {:x \"Decanus\", :y 932385708881792/2128927836399550751523039607378125} {:x \"Dekanus\", :y 42381168585536/104689715348023885283268717095625} {:x \"Duraeus\", :y 169524674342144/693072918559730737690395852470625} {:x \"Dechens\", :y 24545867944928/353287049216191388080016457234375} {:x \"Deittens\", :y 139465158778/6466075766764727757910225903125} {:x \"Derieus\", :y 93517728286048/6616830547676709123339673840640625} {:x \"Ducatus\", :y 316277377504/56867970591503392193505773577675} {:x \"Deliciis\", :y 22613414483/9175956554321000722155119799375})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Galardon&quot;</span>","value":"\"Galardon\""},{"type":"html","content":"<span class='clj-string'>&quot;Kokarden&quot;</span>","value":"\"Kokarden\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"86821ddf-f418-4ec5-a54d-dc1ad67151eb","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"86821ddf-f418-4ec5-a54d-dc1ad67151eb","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"86821ddf-f418-4ec5-a54d-dc1ad67151eb"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"86821ddf-f418-4ec5-a54d-dc1ad67151eb","values":[{"x":"Kokarden","y":1.032290392744316E-21},{"x":"Eckardin","y":2.595680038407661E-22},{"x":"Eckarden","y":1.228328587666812E-22},{"x":"Kalorien","y":2.719106818003536E-23},{"x":"Kalander","y":2.632464741473946E-23},{"x":"Einlarven","y":2.329638863976121E-23},{"x":"Eilanden","y":1.572516718709987E-23},{"x":"Kalenden","y":2.576678142594957E-24},{"x":"Eckartinn","y":5.159551298220513E-26},{"x":"Eckarten","y":2.102372954520491E-26}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"86821ddf-f418-4ec5-a54d-dc1ad67151eb\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"86821ddf-f418-4ec5-a54d-dc1ad67151eb\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"86821ddf-f418-4ec5-a54d-dc1ad67151eb\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"86821ddf-f418-4ec5-a54d-dc1ad67151eb\", :values ({:x \"Kokarden\", :y 1744819554490792/1690241008493973668909410415021754375} {:x \"Eckardin\", :y 218102444311349/840251653070251088871262495531311525} {:x \"Eckarden\", :y 436204888622698/3551206843205215920014648082832906875} {:x \"Kalorien\", :y 63531151853/2336471352738058467278637497851875} {:x \"Kalander\", :y 64722225412/2458616990849469489129635333971875} {:x \"Einlarven\", :y 63531151853/2727081559094869171177572120300000} {:x \"Eilanden\", :y 158652926341/10089109034793018136764941329125000} {:x \"Kalenden\", :y 362635260208/140737507806385264950625530735703125} {:x \"Eckartinn\", :y 63531151853/1231330946838562727181826036732173600} {:x \"Eckarten\", :y 63531151853/3021878288359648862286890753202277500})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Galardon\" \"Kokarden\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"86821ddf-f418-4ec5-a54d-dc1ad67151eb\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"86821ddf-f418-4ec5-a54d-dc1ad67151eb\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"86821ddf-f418-4ec5-a54d-dc1ad67151eb\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"86821ddf-f418-4ec5-a54d-dc1ad67151eb\", :values ({:x \"Kokarden\", :y 1744819554490792/1690241008493973668909410415021754375} {:x \"Eckardin\", :y 218102444311349/840251653070251088871262495531311525} {:x \"Eckarden\", :y 436204888622698/3551206843205215920014648082832906875} {:x \"Kalorien\", :y 63531151853/2336471352738058467278637497851875} {:x \"Kalander\", :y 64722225412/2458616990849469489129635333971875} {:x \"Einlarven\", :y 63531151853/2727081559094869171177572120300000} {:x \"Eilanden\", :y 158652926341/10089109034793018136764941329125000} {:x \"Kalenden\", :y 362635260208/140737507806385264950625530735703125} {:x \"Eckartinn\", :y 63531151853/1231330946838562727181826036732173600} {:x \"Eckarten\", :y 63531151853/3021878288359648862286890753202277500})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Chanichim&quot;</span>","value":"\"Chanichim\""},{"type":"html","content":"<span class='clj-string'>&quot;Ehelichen&quot;</span>","value":"\"Ehelichen\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b","values":[{"x":"Ehelichen","y":7.927932570040788E-27},{"x":"Ehelichen","y":6.482185137271449E-27},{"x":"Lyonschen","y":2.682474008713683E-29},{"x":"Estrichen","y":3.217863087896571E-32},{"x":"Lycischen","y":2.951186185531016E-32},{"x":"Estrichen","y":2.631049658641364E-32},{"x":"Eberschein","y":1.931234361254036E-33},{"x":"Lünischen","y":1.314994437193841E-34},{"x":"Lucchesini","y":5.221671564159802E-35},{"x":"Luccioloni","y":4.404081234143833E-35}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b\", :values ({:x \"Ehelichen\", :y 700766751913/88392118086518318311072894309191796875} {:x \"Ehelichen\", :y 64808874955072/9997998141465000407226400051812808021875} {:x \"Lyonschen\", :y 617492319839/23019508030018302230907890029796771343750} {:x \"Estrichen\", :y 140518699/4366832744641513647370761060859872000000} {:x \"Lycischen\", :y 3248890864/110087627813133608808472147905156150178125} {:x \"Estrichen\", :y 203055679/7717668054386135607574384592064559788000} {:x \"Eberschein\", :y 32061423/16601518512326543789680341981815707400000} {:x \"Lünischen\", :y 3248890864/24706498918223837466874450967845786052484375} {:x \"Lucchesini\", :y 4096/78442313915602818641737997215557114375} {:x \"Luccioloni\", :y 512/11625580291993283738683789759644515325})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Chanichim\" \"Ehelichen\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b\", :values ({:x \"Ehelichen\", :y 700766751913/88392118086518318311072894309191796875} {:x \"Ehelichen\", :y 64808874955072/9997998141465000407226400051812808021875} {:x \"Lyonschen\", :y 617492319839/23019508030018302230907890029796771343750} {:x \"Estrichen\", :y 140518699/4366832744641513647370761060859872000000} {:x \"Lycischen\", :y 3248890864/110087627813133608808472147905156150178125} {:x \"Estrichen\", :y 203055679/7717668054386135607574384592064559788000} {:x \"Eberschein\", :y 32061423/16601518512326543789680341981815707400000} {:x \"Lünischen\", :y 3248890864/24706498918223837466874450967845786052484375} {:x \"Lucchesini\", :y 4096/78442313915602818641737997215557114375} {:x \"Luccioloni\", :y 512/11625580291993283738683789759644515325})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;DgC&quot;</span>","value":"\"DgC\""},{"type":"html","content":"<span class='clj-string'>&quot;Don&quot;</span>","value":"\"Don\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"bdcf7be9-3252-4626-84cb-2a27607d5f3f","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"bdcf7be9-3252-4626-84cb-2a27607d5f3f","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"bdcf7be9-3252-4626-84cb-2a27607d5f3f"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"bdcf7be9-3252-4626-84cb-2a27607d5f3f","values":[{"x":"Don","y":3.611996681946381E-14},{"x":"Dod","y":6.052687305534358E-15},{"x":"Dan","y":7.097049266158389E-16},{"x":"Dad","y":4.965488772104702E-16},{"x":"Dmn","y":3.014324704929866E-17}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"bdcf7be9-3252-4626-84cb-2a27607d5f3f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"bdcf7be9-3252-4626-84cb-2a27607d5f3f\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"bdcf7be9-3252-4626-84cb-2a27607d5f3f\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"bdcf7be9-3252-4626-84cb-2a27607d5f3f\", :values ({:x \"Don\", :y 799/22120729069148710} {:x \"Dod\", :y 77503/12804725585135393910} {:x \"Dan\", :y 1598/2251639998639875025} {:x \"Dad\", :y 1598/3218212895732039205} {:x \"Dmn\", :y 799/26506766132170567425})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"DgC\" \"Don\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"bdcf7be9-3252-4626-84cb-2a27607d5f3f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"bdcf7be9-3252-4626-84cb-2a27607d5f3f\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"bdcf7be9-3252-4626-84cb-2a27607d5f3f\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"bdcf7be9-3252-4626-84cb-2a27607d5f3f\", :values ({:x \"Don\", :y 799/22120729069148710} {:x \"Dod\", :y 77503/12804725585135393910} {:x \"Dan\", :y 1598/2251639998639875025} {:x \"Dad\", :y 1598/3218212895732039205} {:x \"Dmn\", :y 799/26506766132170567425})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;keicht&quot;</span>","value":"\"keicht\""},{"type":"html","content":"<span class='clj-string'>&quot;leicht&quot;</span>","value":"\"leicht\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"31c47683-705d-4017-a9df-6b3818a48309","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"31c47683-705d-4017-a9df-6b3818a48309","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"31c47683-705d-4017-a9df-6b3818a48309"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"31c47683-705d-4017-a9df-6b3818a48309","values":[{"x":"leicht","y":1.444967960262391E-7},{"x":"leicht","y":1.19398461517392E-7},{"x":"leicht","y":1.181461844320168E-7},{"x":"keicht","y":6.627323436057554E-8},{"x":"keicht","y":5.476192164840371E-8},{"x":"keicht","y":5.41875666796722E-8},{"x":"Weicht","y":3.620591363367604E-10},{"x":"Weicht","y":2.991713660493544E-10},{"x":"Weicht","y":2.960335915626249E-10},{"x":"keucht","y":4.452352397502512E-11}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"31c47683-705d-4017-a9df-6b3818a48309\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"31c47683-705d-4017-a9df-6b3818a48309\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"31c47683-705d-4017-a9df-6b3818a48309\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"31c47683-705d-4017-a9df-6b3818a48309\", :values ({:x \"leicht\", :y 73484131878899456/508551981080296701515625} {:x \"leicht\", :y 474315073572361216/3972539239990715285906805} {:x \"leicht\", :y 6796018648317476864/57522117036526193886278625} {:x \"keicht\", :y 8663998639511072/130731489463340454521875} {:x \"keicht\", :y 55923163913848192/1021205287003991193671511} {:x \"keicht\", :y 801270897778976768/14786987991464168941964475} {:x \"Weicht\", :y 566867223208/1565675786954157590625} {:x \"Weicht\", :y 3658935089888/12230231583340714590477} {:x \"Weicht\", :y 366978296548864/1239650860605901862201775} {:x \"keucht\", :y 1852938951113883776/41617077573492731081337607125})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"keicht\" \"leicht\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"31c47683-705d-4017-a9df-6b3818a48309\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"31c47683-705d-4017-a9df-6b3818a48309\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"31c47683-705d-4017-a9df-6b3818a48309\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"31c47683-705d-4017-a9df-6b3818a48309\", :values ({:x \"leicht\", :y 73484131878899456/508551981080296701515625} {:x \"leicht\", :y 474315073572361216/3972539239990715285906805} {:x \"leicht\", :y 6796018648317476864/57522117036526193886278625} {:x \"keicht\", :y 8663998639511072/130731489463340454521875} {:x \"keicht\", :y 55923163913848192/1021205287003991193671511} {:x \"keicht\", :y 801270897778976768/14786987991464168941964475} {:x \"Weicht\", :y 566867223208/1565675786954157590625} {:x \"Weicht\", :y 3658935089888/12230231583340714590477} {:x \"Weicht\", :y 366978296548864/1239650860605901862201775} {:x \"keucht\", :y 1852938951113883776/41617077573492731081337607125})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Gegegenwart&quot;</span>","value":"\"Gegegenwart\""},{"type":"html","content":"<span class='clj-string'>&quot;Morgenwelt&quot;</span>","value":"\"Morgenwelt\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"a9bf703d-2568-4a42-b378-f195ab55eb05","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"a9bf703d-2568-4a42-b378-f195ab55eb05","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"a9bf703d-2568-4a42-b378-f195ab55eb05"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"a9bf703d-2568-4a42-b378-f195ab55eb05","values":[{"x":"Morgenwelt","y":1.049682533098277E-27},{"x":"Mattenwerk","y":1.166539381285465E-30},{"x":"Marterwort","y":4.987836391439334E-31},{"x":"Montelimart","y":3.291826379866044E-31},{"x":"Knochenmark","y":2.051076073594123E-31},{"x":"Kometenwelt","y":1.807628630789862E-31},{"x":"Mottenwelt","y":1.354468795372178E-31},{"x":"Knochenwerk","y":2.289822125438907E-33},{"x":"Martinwerk","y":4.070586692720931E-34},{"x":"Marmorwerk","y":2.068970315167879E-38}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a9bf703d-2568-4a42-b378-f195ab55eb05\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a9bf703d-2568-4a42-b378-f195ab55eb05\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"a9bf703d-2568-4a42-b378-f195ab55eb05\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"a9bf703d-2568-4a42-b378-f195ab55eb05\", :values ({:x \"Morgenwelt\", :y 2418010221351296/2303563358546338785881002640220766895859375} {:x \"Mattenwerk\", :y 361402093067587/309807023119391709008103932569409472478800000} {:x \"Marterwort\", :y 43215699748/86642175798250871005110428187157666859625} {:x \"Montelimart\", :y 18196525243/55277900907217592674814015902809306120000} {:x \"Knochenmark\", :y 123096244489272769/600154455868474222135818257800466464937289506250} {:x \"Kometenwelt\", :y 161040442177472/890893402740051321041936535152937283048828125} {:x \"Mottenwelt\", :y 44384450227/327688983154492971705003012208939366875000} {:x \"Knochenwerk\", :y 13288476960485122/5803279133717874671748795187116240075638256796875} {:x \"Martinwerk\", :y 867576259321/2131329768439546109454791542321860953955549000} {:x \"Marmorwerk\", :y 99232376/4796220384242108315163048939959635501574318625})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Gegegenwart\" \"Morgenwelt\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a9bf703d-2568-4a42-b378-f195ab55eb05\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a9bf703d-2568-4a42-b378-f195ab55eb05\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"a9bf703d-2568-4a42-b378-f195ab55eb05\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"a9bf703d-2568-4a42-b378-f195ab55eb05\", :values ({:x \"Morgenwelt\", :y 2418010221351296/2303563358546338785881002640220766895859375} {:x \"Mattenwerk\", :y 361402093067587/309807023119391709008103932569409472478800000} {:x \"Marterwort\", :y 43215699748/86642175798250871005110428187157666859625} {:x \"Montelimart\", :y 18196525243/55277900907217592674814015902809306120000} {:x \"Knochenmark\", :y 123096244489272769/600154455868474222135818257800466464937289506250} {:x \"Kometenwelt\", :y 161040442177472/890893402740051321041936535152937283048828125} {:x \"Mottenwelt\", :y 44384450227/327688983154492971705003012208939366875000} {:x \"Knochenwerk\", :y 13288476960485122/5803279133717874671748795187116240075638256796875} {:x \"Martinwerk\", :y 867576259321/2131329768439546109454791542321860953955549000} {:x \"Marmorwerk\", :y 99232376/4796220384242108315163048939959635501574318625})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;sequioris&quot;</span>","value":"\"sequioris\""},{"type":"html","content":"<span class='clj-string'>&quot;suavioris&quot;</span>","value":"\"suavioris\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"30609583-83b0-4fcb-9ce5-4b86e95054e6","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"30609583-83b0-4fcb-9ce5-4b86e95054e6","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"30609583-83b0-4fcb-9ce5-4b86e95054e6"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"30609583-83b0-4fcb-9ce5-4b86e95054e6","values":[{"x":"suavioris","y":1.05288938523803E-19},{"x":"suavioris","y":1.041168704790981E-19},{"x":"feliciores","y":3.136617256977228E-23},{"x":"suaviolis","y":1.400577596349816E-23},{"x":"Scansores","y":3.613831766353837E-25},{"x":"Scabineis","y":1.051077404494468E-26},{"x":"scabinius","y":8.393024031389609E-29},{"x":"ocasiones","y":1.847115980110831E-30},{"x":"Suasiones","y":7.275290986643757E-31},{"x":"Klageorts","y":1.242078036031441E-31}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"30609583-83b0-4fcb-9ce5-4b86e95054e6\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"30609583-83b0-4fcb-9ce5-4b86e95054e6\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"30609583-83b0-4fcb-9ce5-4b86e95054e6\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"30609583-83b0-4fcb-9ce5-4b86e95054e6\", :values ({:x \"suavioris\", :y 7888680721712384/74924116743079953548098927371419985} {:x \"suavioris\", :y 220837162429202243584/2121050713616446360314278318503767337845} {:x \"feliciores\", :y 1101145066268896/35106134285892261654966705517827759375} {:x \"suaviolis\", :y 91795557489016832/6554121508744271185301398704262557464325} {:x \"Scansores\", :y 555648048145784/1537559255854356598157616763257406078125} {:x \"Scabineis\", :y 199198013696/18951792973972966581392342054212554375} {:x \"scabinius\", :y 5729631705424/68266594781515971069227540215593065689725} {:x \"ocasiones\", :y 8731232128/4726953922772141685663317695256689996875} {:x \"Suasiones\", :y 885690341888/1217395075350226532974087931190457006921875} {:x \"Klageorts\", :y 2746490807/22112063230546305256863146672124514321875})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"sequioris\" \"suavioris\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"30609583-83b0-4fcb-9ce5-4b86e95054e6\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"30609583-83b0-4fcb-9ce5-4b86e95054e6\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"30609583-83b0-4fcb-9ce5-4b86e95054e6\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"30609583-83b0-4fcb-9ce5-4b86e95054e6\", :values ({:x \"suavioris\", :y 7888680721712384/74924116743079953548098927371419985} {:x \"suavioris\", :y 220837162429202243584/2121050713616446360314278318503767337845} {:x \"feliciores\", :y 1101145066268896/35106134285892261654966705517827759375} {:x \"suaviolis\", :y 91795557489016832/6554121508744271185301398704262557464325} {:x \"Scansores\", :y 555648048145784/1537559255854356598157616763257406078125} {:x \"Scabineis\", :y 199198013696/18951792973972966581392342054212554375} {:x \"scabinius\", :y 5729631705424/68266594781515971069227540215593065689725} {:x \"ocasiones\", :y 8731232128/4726953922772141685663317695256689996875} {:x \"Suasiones\", :y 885690341888/1217395075350226532974087931190457006921875} {:x \"Klageorts\", :y 2746490807/22112063230546305256863146672124514321875})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Guarenghi&quot;</span>","value":"\"Guarenghi\""},{"type":"html","content":"<span class='clj-string'>&quot;Einmahl&quot;</span>","value":"\"Einmahl\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"6c83ffe4-1f9b-4292-8a5c-295088e8b69f","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"6c83ffe4-1f9b-4292-8a5c-295088e8b69f","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"6c83ffe4-1f9b-4292-8a5c-295088e8b69f"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"6c83ffe4-1f9b-4292-8a5c-295088e8b69f","values":[{"x":"Einmahl","y":1.285327162208256E-27},{"x":"Knotenohr","y":5.701333919662257E-29},{"x":"tuerentur","y":1.27961089504476E-30},{"x":"Enormous","y":2.527069497752067E-32},{"x":"Knurrhahn","y":7.10977802515108E-33},{"x":"Einmals","y":1.497326643422738E-34},{"x":"Engelwahl","y":4.01240758641708E-36},{"x":"Eulennase","y":2.608963309515171E-37},{"x":"Ellernhayn","y":6.017464308144496E-38},{"x":"Eigenwahl","y":1.892742490079259E-38}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"6c83ffe4-1f9b-4292-8a5c-295088e8b69f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"6c83ffe4-1f9b-4292-8a5c-295088e8b69f\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"6c83ffe4-1f9b-4292-8a5c-295088e8b69f\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"6c83ffe4-1f9b-4292-8a5c-295088e8b69f\", :values ({:x \"Einmahl\", :y 2912408/2265888472314191981438806719066825} {:x \"Knotenohr\", :y 1016086289/17821904545808329093950958113616042500} {:x \"tuerentur\", :y 9942493579051/7769935077571541007728014323939780630375000} {:x \"Enormous\", :y 9623/380796808657619252321961592436141760} {:x \"Knurrhahn\", :y 1055777822/148496594164424044170271390453304934119025} {:x \"Einmals\", :y 2/13357138930141531547062612010131875} {:x \"Engelwahl\", :y 54857/13671841361706006695113852108174886223750} {:x \"Eulennase\", :y 11215208/42987220092735392255071509795606713033203125} {:x \"Ellernhayn\", :y 9623/159917857543010219588890278390168864300000} {:x \"Eigenwahl\", :y 4987/263480110270635346549121788001468380513125})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Guarenghi\" \"Einmahl\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"6c83ffe4-1f9b-4292-8a5c-295088e8b69f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"6c83ffe4-1f9b-4292-8a5c-295088e8b69f\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"6c83ffe4-1f9b-4292-8a5c-295088e8b69f\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"6c83ffe4-1f9b-4292-8a5c-295088e8b69f\", :values ({:x \"Einmahl\", :y 2912408/2265888472314191981438806719066825} {:x \"Knotenohr\", :y 1016086289/17821904545808329093950958113616042500} {:x \"tuerentur\", :y 9942493579051/7769935077571541007728014323939780630375000} {:x \"Enormous\", :y 9623/380796808657619252321961592436141760} {:x \"Knurrhahn\", :y 1055777822/148496594164424044170271390453304934119025} {:x \"Einmals\", :y 2/13357138930141531547062612010131875} {:x \"Engelwahl\", :y 54857/13671841361706006695113852108174886223750} {:x \"Eulennase\", :y 11215208/42987220092735392255071509795606713033203125} {:x \"Ellernhayn\", :y 9623/159917857543010219588890278390168864300000} {:x \"Eigenwahl\", :y 4987/263480110270635346549121788001468380513125})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Clutia&quot;</span>","value":"\"Clutia\""},{"type":"html","content":"<span class='clj-string'>&quot;Liukiu&quot;</span>","value":"\"Liukiu\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"c11ac5f1-c700-4afa-825a-54ed7a6e1590","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"c11ac5f1-c700-4afa-825a-54ed7a6e1590","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"c11ac5f1-c700-4afa-825a-54ed7a6e1590"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"c11ac5f1-c700-4afa-825a-54ed7a6e1590","values":[{"x":"Liukiu","y":3.980783747950068E-21},{"x":"diutit","y":3.422291820924223E-21},{"x":"diutit","y":3.379145591118705E-21},{"x":"Elvira","y":3.058106652220628E-21},{"x":"Elilim","y":7.62689377713472E-22},{"x":"Elmina","y":7.289913160243465E-22},{"x":"Liukio","y":6.204812748623228E-22},{"x":"Elston","y":7.271819725212153E-23},{"x":"Einritt","y":6.928708773560678E-23},{"x":"Elucet","y":6.806571233405684E-23}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c11ac5f1-c700-4afa-825a-54ed7a6e1590\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c11ac5f1-c700-4afa-825a-54ed7a6e1590\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"c11ac5f1-c700-4afa-825a-54ed7a6e1590\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"c11ac5f1-c700-4afa-825a-54ed7a6e1590\", :values ({:x \"Liukiu\", :y 21390400/5373414220507490100271143613} {:x \"diutit\", :y 105569/30847457062118706946946625} {:x \"diutit\", :y 111230080/32916628479205595497922154243} {:x \"Elvira\", :y 10855/3549581893135643482795104} {:x \"Elilim\", :y 3074/4030474384232008610441733} {:x \"Elmina\", :y 23881/32758963618713991750314720} {:x \"Liukio\", :y 8556160/13789553926342913879773721031} {:x \"Elston\", :y 53/728840950446605596590375} {:x \"Einritt\", :y 15950/230201622282982239651140109} {:x \"Elucet\", :y 268763/3948581316257286866021907000})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Clutia\" \"Liukiu\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c11ac5f1-c700-4afa-825a-54ed7a6e1590\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c11ac5f1-c700-4afa-825a-54ed7a6e1590\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"c11ac5f1-c700-4afa-825a-54ed7a6e1590\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"c11ac5f1-c700-4afa-825a-54ed7a6e1590\", :values ({:x \"Liukiu\", :y 21390400/5373414220507490100271143613} {:x \"diutit\", :y 105569/30847457062118706946946625} {:x \"diutit\", :y 111230080/32916628479205595497922154243} {:x \"Elvira\", :y 10855/3549581893135643482795104} {:x \"Elilim\", :y 3074/4030474384232008610441733} {:x \"Elmina\", :y 23881/32758963618713991750314720} {:x \"Liukio\", :y 8556160/13789553926342913879773721031} {:x \"Elston\", :y 53/728840950446605596590375} {:x \"Einritt\", :y 15950/230201622282982239651140109} {:x \"Elucet\", :y 268763/3948581316257286866021907000})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Martiniques&quot;</span>","value":"\"Martiniques\""},{"type":"html","content":"<span class='clj-string'>&quot;Martinianus&quot;</span>","value":"\"Martinianus\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693","values":[{"x":"Martinianus","y":2.588448336535015E-17},{"x":"Martinianus","y":2.555814711873022E-17},{"x":"Martinianus","y":2.540184088002195E-17},{"x":"Martinianus","y":2.516417067580635E-17},{"x":"Martinianus","y":2.50815895042066E-17},{"x":"Maximianus","y":1.403623202651915E-25},{"x":"Wortinhaltes","y":1.780009074458347E-26},{"x":"Wortinhaltes","y":1.763354568108578E-26},{"x":"Wortinhaltes","y":1.757567773540371E-26},{"x":"Weltinhaltes","y":1.493150702755582E-31}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693\", :values ({:x \"Martinianus\", :y 2516912538923134/97236344392036775599015217221875} {:x \"Martinianus\", :y 285872728675939268864/11185189886728455625200454533216466695} {:x \"Martinianus\", :y 1187848299779879421376/46762291968929653429060822079233914375} {:x \"Martinianus\", :y 102091067547628327168/4057001077559134752191351305268345547} {:x \"Martinianus\", :y 134916660575117324331524096/5379111262166601056124712775666157908339787} {:x \"Maximianus\", :y 105697361110/753032301762340913554216896959600343} {:x \"Wortinhaltes\", :y 476378963204189/26762726664702548864694868722757358203125} {:x \"Wortinhaltes\", :y 3149459205404/178606121670821755388186653553082613125} {:x \"Wortinhaltes\", :y 594587514318227584/33830132941076589525589919480669920931401875} {:x \"Weltinhaltes\", :y 2178185013092/14587844408954835648854885066908450634765625})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Martiniques\" \"Martinianus\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693\", :values ({:x \"Martinianus\", :y 2516912538923134/97236344392036775599015217221875} {:x \"Martinianus\", :y 285872728675939268864/11185189886728455625200454533216466695} {:x \"Martinianus\", :y 1187848299779879421376/46762291968929653429060822079233914375} {:x \"Martinianus\", :y 102091067547628327168/4057001077559134752191351305268345547} {:x \"Martinianus\", :y 134916660575117324331524096/5379111262166601056124712775666157908339787} {:x \"Maximianus\", :y 105697361110/753032301762340913554216896959600343} {:x \"Wortinhaltes\", :y 476378963204189/26762726664702548864694868722757358203125} {:x \"Wortinhaltes\", :y 3149459205404/178606121670821755388186653553082613125} {:x \"Wortinhaltes\", :y 594587514318227584/33830132941076589525589919480669920931401875} {:x \"Weltinhaltes\", :y 2178185013092/14587844408954835648854885066908450634765625})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Grenzdorfe&quot;</span>","value":"\"Grenzdorfe\""},{"type":"html","content":"<span class='clj-string'>&quot;Kreuzböcke&quot;</span>","value":"\"Kreuzböcke\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"3f357338-aaf8-4dad-a23e-e1f703ddbfca","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"3f357338-aaf8-4dad-a23e-e1f703ddbfca","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"3f357338-aaf8-4dad-a23e-e1f703ddbfca"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"3f357338-aaf8-4dad-a23e-e1f703ddbfca","values":[{"x":"Kreuzböcke","y":3.719066614450263E-27},{"x":"Kreuzhütte","y":3.534349893877153E-29},{"x":"Elennböcke","y":1.339009824013125E-34},{"x":"Ermunterte","y":1.158276987726304E-34},{"x":"Ermunterst","y":4.57861821548031E-36},{"x":"Exercices","y":3.074290481991594E-39},{"x":"Kleinodes","y":7.142352573010212E-40},{"x":"Etruriens","y":9.696131610655254E-42},{"x":"Erminiens","y":4.611183671165881E-42},{"x":"Kiloniens","y":2.075903197101711E-44}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"3f357338-aaf8-4dad-a23e-e1f703ddbfca\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"3f357338-aaf8-4dad-a23e-e1f703ddbfca\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"3f357338-aaf8-4dad-a23e-e1f703ddbfca\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"3f357338-aaf8-4dad-a23e-e1f703ddbfca\", :values ({:x \"Kreuzböcke\", :y 2838486800435924/763225587142514121563515939476884114915625} {:x \"Kreuzhütte\", :y 5587572441803/158093358314150357607951378708781035600000} {:x \"Elennböcke\", :y 1651776929/12335808889358896797038523060339270159375000} {:x \"Ermunterte\", :y 8667466672271/74830690448967836554357073139032317701696000000} {:x \"Ermunterst\", :y 1681747264769/367304541593577719765115396289216257735782400000} {:x \"Exercices\", :y 737/239730111489840427588736488187534868000000} {:x \"Kleinodes\", :y 536/750453011834583449221046700787820688046875} {:x \"Etruriens\", :y 127/13098007029982702237006065617746545234560000} {:x \"Erminiens\", :y 9623/2086882823638847287763718475761316298367000000} {:x \"Kiloniens\", :y 4/192687212273897568253029035819654255622609375})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Grenzdorfe\" \"Kreuzböcke\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"3f357338-aaf8-4dad-a23e-e1f703ddbfca\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"3f357338-aaf8-4dad-a23e-e1f703ddbfca\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"3f357338-aaf8-4dad-a23e-e1f703ddbfca\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"3f357338-aaf8-4dad-a23e-e1f703ddbfca\", :values ({:x \"Kreuzböcke\", :y 2838486800435924/763225587142514121563515939476884114915625} {:x \"Kreuzhütte\", :y 5587572441803/158093358314150357607951378708781035600000} {:x \"Elennböcke\", :y 1651776929/12335808889358896797038523060339270159375000} {:x \"Ermunterte\", :y 8667466672271/74830690448967836554357073139032317701696000000} {:x \"Ermunterst\", :y 1681747264769/367304541593577719765115396289216257735782400000} {:x \"Exercices\", :y 737/239730111489840427588736488187534868000000} {:x \"Kleinodes\", :y 536/750453011834583449221046700787820688046875} {:x \"Etruriens\", :y 127/13098007029982702237006065617746545234560000} {:x \"Erminiens\", :y 9623/2086882823638847287763718475761316298367000000} {:x \"Kiloniens\", :y 4/192687212273897568253029035819654255622609375})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Calviniani&quot;</span>","value":"\"Calviniani\""},{"type":"html","content":"<span class='clj-string'>&quot;Entwirrens&quot;</span>","value":"\"Entwirrens\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"0f78550a-447e-4a5a-a458-60a465450348","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"0f78550a-447e-4a5a-a458-60a465450348","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"0f78550a-447e-4a5a-a458-60a465450348"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"0f78550a-447e-4a5a-a458-60a465450348","values":[{"x":"Entwirrens","y":2.828822064385377E-39},{"x":"Entwichene","y":1.292037223143679E-39}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0f78550a-447e-4a5a-a458-60a465450348\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0f78550a-447e-4a5a-a458-60a465450348\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"0f78550a-447e-4a5a-a458-60a465450348\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"0f78550a-447e-4a5a-a458-60a465450348\", :values ({:x \"Entwirrens\", :y 176378/62350333808755100729757015113739392853200625} {:x \"Entwichene\", :y 12164/9414589442248074886439371235605008633571875})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Calviniani\" \"Entwirrens\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0f78550a-447e-4a5a-a458-60a465450348\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0f78550a-447e-4a5a-a458-60a465450348\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"0f78550a-447e-4a5a-a458-60a465450348\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"0f78550a-447e-4a5a-a458-60a465450348\", :values ({:x \"Entwirrens\", :y 176378/62350333808755100729757015113739392853200625} {:x \"Entwichene\", :y 12164/9414589442248074886439371235605008633571875})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Feinperiode&quot;</span>","value":"\"Feinperiode\""},{"type":"html","content":"<span class='clj-string'>&quot;Imperoche&quot;</span>","value":"\"Imperoche\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"74e6b508-00f0-4da8-b366-cfd4215cd1a5","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"74e6b508-00f0-4da8-b366-cfd4215cd1a5","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"74e6b508-00f0-4da8-b366-cfd4215cd1a5"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"74e6b508-00f0-4da8-b366-cfd4215cd1a5","values":[{"x":"Imperoche","y":7.171170211232453E-28},{"x":"Impulsive","y":1.283545334151516E-34},{"x":"Impulsion","y":7.593211352804835E-39},{"x":"Impressor","y":8.914245306474316E-40},{"x":"Impresion","y":4.276078289583947E-40}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"74e6b508-00f0-4da8-b366-cfd4215cd1a5\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"74e6b508-00f0-4da8-b366-cfd4215cd1a5\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"74e6b508-00f0-4da8-b366-cfd4215cd1a5\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"74e6b508-00f0-4da8-b366-cfd4215cd1a5\", :values ({:x \"Imperoche\", :y 475174117/662617261901995121100699928881884520} {:x \"Impulsive\", :y 2948/22967634422891398316217625813885211565} {:x \"Impulsion\", :y 8/1053572675419458884106326432220859764603} {:x \"Impressor\", :y 1/1121799956832813796309034981872412653395} {:x \"Impresion\", :y 2/4677182840341764459659205745241319374805})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Feinperiode\" \"Imperoche\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"74e6b508-00f0-4da8-b366-cfd4215cd1a5\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"74e6b508-00f0-4da8-b366-cfd4215cd1a5\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"74e6b508-00f0-4da8-b366-cfd4215cd1a5\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"74e6b508-00f0-4da8-b366-cfd4215cd1a5\", :values ({:x \"Imperoche\", :y 475174117/662617261901995121100699928881884520} {:x \"Impulsive\", :y 2948/22967634422891398316217625813885211565} {:x \"Impulsion\", :y 8/1053572675419458884106326432220859764603} {:x \"Impressor\", :y 1/1121799956832813796309034981872412653395} {:x \"Impresion\", :y 2/4677182840341764459659205745241319374805})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Schau¬&quot;</span>","value":"\"Schau¬\""},{"type":"html","content":"<span class='clj-string'>&quot;Schau-&quot;</span>","value":"\"Schau-\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc","values":[{"x":"Schau-","y":4.473376463320802E-8},{"x":"Schau'","y":3.551626783389638E-10},{"x":"Scham-","y":5.157164378310343E-11},{"x":"Schall-","y":2.904429726630807E-11},{"x":"Schalt-","y":8.899688467144073E-12},{"x":"Schaun","y":6.553817026580948E-12},{"x":"Schall'","y":5.051161845443494E-14},{"x":"Schon-","y":1.648101835730251E-14},{"x":"Schann","y":3.426557592767522E-15},{"x":"Schinn-","y":2.522211921503071E-15}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc\", :values ({:x \"Schau-\", :y 591931891445105732352/13232329009163836808429119325} {:x \"Schau'\", :y 41243860886643376/116126674907211500859193195} {:x \"Scham-\", :y 76761892930021728/1488451546219115315156983055} {:x \"Schall-\", :y 6978353902729248/240265888988275495678224325} {:x \"Schalt-\", :y 1744588475682312/196028038747984812820604195} {:x \"Schaun\", :y 82487721773286752/12586210667574840487859172225} {:x \"Schall'\", :y 11183259459502/221399745280189198096100475} {:x \"Schon-\", :y 1071450008096/65011153125817347886739925} {:x \"Schann\", :y 123015854054522/35900711055951057113479975125} {:x \"Schinn-\", :y 133931251012/53100712858492032243367725})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Schau¬\" \"Schau-\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc\", :values ({:x \"Schau-\", :y 591931891445105732352/13232329009163836808429119325} {:x \"Schau'\", :y 41243860886643376/116126674907211500859193195} {:x \"Scham-\", :y 76761892930021728/1488451546219115315156983055} {:x \"Schall-\", :y 6978353902729248/240265888988275495678224325} {:x \"Schalt-\", :y 1744588475682312/196028038747984812820604195} {:x \"Schaun\", :y 82487721773286752/12586210667574840487859172225} {:x \"Schall'\", :y 11183259459502/221399745280189198096100475} {:x \"Schon-\", :y 1071450008096/65011153125817347886739925} {:x \"Schann\", :y 123015854054522/35900711055951057113479975125} {:x \"Schinn-\", :y 133931251012/53100712858492032243367725})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"}],"value":"#gorilla_repl.table.TableView{:contents ([\"Geldadel\" \"Entladet\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a844128e-ef9b-4884-be28-71a000d88ee2\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a844128e-ef9b-4884-be28-71a000d88ee2\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"a844128e-ef9b-4884-be28-71a000d88ee2\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"a844128e-ef9b-4884-be28-71a000d88ee2\", :values ({:x \"Entladet\", :y 499353881/21274025488326446046942094329262500} {:x \"Mitchel\", :y 39061/3385573675762334787804002359500} {:x \"Michiel\", :y 429671/76021307328582583880012389737000} {:x \"Midamus\", :y 573311/110208423965319670568416643547820} {:x \"Etstoel\", :y 3551/1893365237286650392883251200000} {:x \"tribades\", :y 115235511/187130352172983768066764133083252000} {:x \"Emsiedel\", :y 36571749/693858243909238602872538120715000000} {:x \"Etliches\", :y 39061/1824155065313828428762793379300000} {:x \"Entbindet\", :y 230011/38482795145712354382232493073560000} {:x \"Euklides\", :y 690033/132019941708404544658657733436087500})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"angezogeuen\" \"angezogenen\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"c6d52b27-5d70-4db0-9f3d-e7a70ca1d535\", :values ({:x \"angezogenen\", :y 4818013969845326116723920379525361152/266961032440390977641033575906375380903515625} {:x \"angezogeuen\", :y 24290957649746497222799478277087535104/2339805880927718587128980254547668603236328125} {:x \"angezogenem\", :y 958949802752232651003393696058112/87731788785571120605275881672396148801953125} {:x \"ungezogenen\", :y 58401585127461588362432063559424/33379651315581811413252824616188553474609375} {:x \"Zugezogenen\", :y 348428680001297586415599759104/42280891666403627790120244513838834401171875} {:x \"angezogener\", :y 2978104977491405748457744397696/446354895377557397117237431053543112873828125} {:x \"ungezogener\", :y 21948263307168351900195260416/346023754605468512435160642849249227094140625} {:x \"angenommen\", :y 593328128992617266793212096/27734845114858824858483596194374927385546875} {:x \"ungezügelten\", :y 1938448789413887027430697808/911775281182991454731640200746887569994140625} {:x \"angenomenen\", :y 10718057613306394166241602176/41494200329853478937083536957968148441708984375})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Cassir'\" \"nostin'\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"ba79a82d-ecc3-40d2-aff2-2cb550f2be80\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"ba79a82d-ecc3-40d2-aff2-2cb550f2be80\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"ba79a82d-ecc3-40d2-aff2-2cb550f2be80\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"ba79a82d-ecc3-40d2-aff2-2cb550f2be80\", :values ({:x \"nostin'\", :y 1969216/5056982612311856254713323832375} {:x \"Entfiel\", :y 3944/61196203213968278878202940539625} {:x \"destill\", :y 61538/10432350670175989233362630873915625} {:x \"duftet'\", :y 17/30279781131600581081213164875500} {:x \"Etstoel\", :y 1061/16138003931750900291220747440640000} {:x \"einstell\", :y 23342/4230363263805618492296967057927421875} {:x \"Leiden'\", :y 4/9538855036958627979383540545453125} {:x \"neidet'\", :y 1/116865972402486838388925389827312500})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Citadellkirche\" \"durchnittliche\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"173de439-f40c-49dc-8c38-7cb09eb4a80b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"173de439-f40c-49dc-8c38-7cb09eb4a80b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"173de439-f40c-49dc-8c38-7cb09eb4a80b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"173de439-f40c-49dc-8c38-7cb09eb4a80b\", :values ({:x \"durchnittliche\", :y 22913230304/129739685479305131061931571963197036577882866321563093223995})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Generalpausen\" \"Mitrailleusen\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"580b2c45-a60d-462f-a695-4182c88429f4\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"580b2c45-a60d-462f-a695-4182c88429f4\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"580b2c45-a60d-462f-a695-4182c88429f4\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"580b2c45-a60d-462f-a695-4182c88429f4\", :values ({:x \"Mitrailleusen\", :y 251920327200157263791/198482687285878287595291064121698638543204164328125} {:x \"Milchspeisen\", :y 209257948856/29663801613839485211551560727394524314053097459375} {:x \"Mittelwasser\", :y 1436455009/787710057878857587352917933300624143528400000000} {:x \"Mittelwänden\", :y 1544198513/867752034154666431509576201803229978881020000000} {:x \"Mittelwinden\", :y 1544198513/190205834515443119947237128349822467843011640000000} {:x \"Mittelspindel\", :y 737/477841322473274402360230381446250935956011500000} {:x \"Kroninsignien\", :y 203747/756385011468082986818817497582065555042569370125000} {:x \"Kroninsignien\", :y 17929736/41786603946482374726788399321511384586523708043500421875})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"quineutrarum\" \"absolutioni\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"e0e8623a-f358-4ac4-a505-022c1bb7dc84\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"e0e8623a-f358-4ac4-a505-022c1bb7dc84\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"e0e8623a-f358-4ac4-a505-022c1bb7dc84\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"e0e8623a-f358-4ac4-a505-022c1bb7dc84\", :values ({:x \"absolutioni\", :y 1888256/5070503938151982144587936007280784553756869558625})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Oetaeus\" \"Dolaeus\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a39f0030-1cea-4977-9ce1-e8c37e72268b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a39f0030-1cea-4977-9ce1-e8c37e72268b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"a39f0030-1cea-4977-9ce1-e8c37e72268b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"a39f0030-1cea-4977-9ce1-e8c37e72268b\", :values ({:x \"Dolaeus\", :y 3852833507776/528710297491315157085497345625} {:x \"Ducaeus\", :y 1864771417763584/746367786510662248672466261705625} {:x \"Decanus\", :y 932385708881792/2128927836399550751523039607378125} {:x \"Dekanus\", :y 42381168585536/104689715348023885283268717095625} {:x \"Duraeus\", :y 169524674342144/693072918559730737690395852470625} {:x \"Dechens\", :y 24545867944928/353287049216191388080016457234375} {:x \"Deittens\", :y 139465158778/6466075766764727757910225903125} {:x \"Derieus\", :y 93517728286048/6616830547676709123339673840640625} {:x \"Ducatus\", :y 316277377504/56867970591503392193505773577675} {:x \"Deliciis\", :y 22613414483/9175956554321000722155119799375})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Galardon\" \"Kokarden\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"86821ddf-f418-4ec5-a54d-dc1ad67151eb\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"86821ddf-f418-4ec5-a54d-dc1ad67151eb\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"86821ddf-f418-4ec5-a54d-dc1ad67151eb\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"86821ddf-f418-4ec5-a54d-dc1ad67151eb\", :values ({:x \"Kokarden\", :y 1744819554490792/1690241008493973668909410415021754375} {:x \"Eckardin\", :y 218102444311349/840251653070251088871262495531311525} {:x \"Eckarden\", :y 436204888622698/3551206843205215920014648082832906875} {:x \"Kalorien\", :y 63531151853/2336471352738058467278637497851875} {:x \"Kalander\", :y 64722225412/2458616990849469489129635333971875} {:x \"Einlarven\", :y 63531151853/2727081559094869171177572120300000} {:x \"Eilanden\", :y 158652926341/10089109034793018136764941329125000} {:x \"Kalenden\", :y 362635260208/140737507806385264950625530735703125} {:x \"Eckartinn\", :y 63531151853/1231330946838562727181826036732173600} {:x \"Eckarten\", :y 63531151853/3021878288359648862286890753202277500})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Chanichim\" \"Ehelichen\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"ec46d7aa-78a2-4ab1-96e6-bd6b8fb9f73b\", :values ({:x \"Ehelichen\", :y 700766751913/88392118086518318311072894309191796875} {:x \"Ehelichen\", :y 64808874955072/9997998141465000407226400051812808021875} {:x \"Lyonschen\", :y 617492319839/23019508030018302230907890029796771343750} {:x \"Estrichen\", :y 140518699/4366832744641513647370761060859872000000} {:x \"Lycischen\", :y 3248890864/110087627813133608808472147905156150178125} {:x \"Estrichen\", :y 203055679/7717668054386135607574384592064559788000} {:x \"Eberschein\", :y 32061423/16601518512326543789680341981815707400000} {:x \"Lünischen\", :y 3248890864/24706498918223837466874450967845786052484375} {:x \"Lucchesini\", :y 4096/78442313915602818641737997215557114375} {:x \"Luccioloni\", :y 512/11625580291993283738683789759644515325})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"DgC\" \"Don\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"bdcf7be9-3252-4626-84cb-2a27607d5f3f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"bdcf7be9-3252-4626-84cb-2a27607d5f3f\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"bdcf7be9-3252-4626-84cb-2a27607d5f3f\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"bdcf7be9-3252-4626-84cb-2a27607d5f3f\", :values ({:x \"Don\", :y 799/22120729069148710} {:x \"Dod\", :y 77503/12804725585135393910} {:x \"Dan\", :y 1598/2251639998639875025} {:x \"Dad\", :y 1598/3218212895732039205} {:x \"Dmn\", :y 799/26506766132170567425})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"keicht\" \"leicht\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"31c47683-705d-4017-a9df-6b3818a48309\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"31c47683-705d-4017-a9df-6b3818a48309\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"31c47683-705d-4017-a9df-6b3818a48309\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"31c47683-705d-4017-a9df-6b3818a48309\", :values ({:x \"leicht\", :y 73484131878899456/508551981080296701515625} {:x \"leicht\", :y 474315073572361216/3972539239990715285906805} {:x \"leicht\", :y 6796018648317476864/57522117036526193886278625} {:x \"keicht\", :y 8663998639511072/130731489463340454521875} {:x \"keicht\", :y 55923163913848192/1021205287003991193671511} {:x \"keicht\", :y 801270897778976768/14786987991464168941964475} {:x \"Weicht\", :y 566867223208/1565675786954157590625} {:x \"Weicht\", :y 3658935089888/12230231583340714590477} {:x \"Weicht\", :y 366978296548864/1239650860605901862201775} {:x \"keucht\", :y 1852938951113883776/41617077573492731081337607125})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Gegegenwart\" \"Morgenwelt\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a9bf703d-2568-4a42-b378-f195ab55eb05\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a9bf703d-2568-4a42-b378-f195ab55eb05\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"a9bf703d-2568-4a42-b378-f195ab55eb05\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"a9bf703d-2568-4a42-b378-f195ab55eb05\", :values ({:x \"Morgenwelt\", :y 2418010221351296/2303563358546338785881002640220766895859375} {:x \"Mattenwerk\", :y 361402093067587/309807023119391709008103932569409472478800000} {:x \"Marterwort\", :y 43215699748/86642175798250871005110428187157666859625} {:x \"Montelimart\", :y 18196525243/55277900907217592674814015902809306120000} {:x \"Knochenmark\", :y 123096244489272769/600154455868474222135818257800466464937289506250} {:x \"Kometenwelt\", :y 161040442177472/890893402740051321041936535152937283048828125} {:x \"Mottenwelt\", :y 44384450227/327688983154492971705003012208939366875000} {:x \"Knochenwerk\", :y 13288476960485122/5803279133717874671748795187116240075638256796875} {:x \"Martinwerk\", :y 867576259321/2131329768439546109454791542321860953955549000} {:x \"Marmorwerk\", :y 99232376/4796220384242108315163048939959635501574318625})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"sequioris\" \"suavioris\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"30609583-83b0-4fcb-9ce5-4b86e95054e6\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"30609583-83b0-4fcb-9ce5-4b86e95054e6\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"30609583-83b0-4fcb-9ce5-4b86e95054e6\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"30609583-83b0-4fcb-9ce5-4b86e95054e6\", :values ({:x \"suavioris\", :y 7888680721712384/74924116743079953548098927371419985} {:x \"suavioris\", :y 220837162429202243584/2121050713616446360314278318503767337845} {:x \"feliciores\", :y 1101145066268896/35106134285892261654966705517827759375} {:x \"suaviolis\", :y 91795557489016832/6554121508744271185301398704262557464325} {:x \"Scansores\", :y 555648048145784/1537559255854356598157616763257406078125} {:x \"Scabineis\", :y 199198013696/18951792973972966581392342054212554375} {:x \"scabinius\", :y 5729631705424/68266594781515971069227540215593065689725} {:x \"ocasiones\", :y 8731232128/4726953922772141685663317695256689996875} {:x \"Suasiones\", :y 885690341888/1217395075350226532974087931190457006921875} {:x \"Klageorts\", :y 2746490807/22112063230546305256863146672124514321875})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Guarenghi\" \"Einmahl\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"6c83ffe4-1f9b-4292-8a5c-295088e8b69f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"6c83ffe4-1f9b-4292-8a5c-295088e8b69f\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"6c83ffe4-1f9b-4292-8a5c-295088e8b69f\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"6c83ffe4-1f9b-4292-8a5c-295088e8b69f\", :values ({:x \"Einmahl\", :y 2912408/2265888472314191981438806719066825} {:x \"Knotenohr\", :y 1016086289/17821904545808329093950958113616042500} {:x \"tuerentur\", :y 9942493579051/7769935077571541007728014323939780630375000} {:x \"Enormous\", :y 9623/380796808657619252321961592436141760} {:x \"Knurrhahn\", :y 1055777822/148496594164424044170271390453304934119025} {:x \"Einmals\", :y 2/13357138930141531547062612010131875} {:x \"Engelwahl\", :y 54857/13671841361706006695113852108174886223750} {:x \"Eulennase\", :y 11215208/42987220092735392255071509795606713033203125} {:x \"Ellernhayn\", :y 9623/159917857543010219588890278390168864300000} {:x \"Eigenwahl\", :y 4987/263480110270635346549121788001468380513125})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Clutia\" \"Liukiu\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c11ac5f1-c700-4afa-825a-54ed7a6e1590\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c11ac5f1-c700-4afa-825a-54ed7a6e1590\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"c11ac5f1-c700-4afa-825a-54ed7a6e1590\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"c11ac5f1-c700-4afa-825a-54ed7a6e1590\", :values ({:x \"Liukiu\", :y 21390400/5373414220507490100271143613} {:x \"diutit\", :y 105569/30847457062118706946946625} {:x \"diutit\", :y 111230080/32916628479205595497922154243} {:x \"Elvira\", :y 10855/3549581893135643482795104} {:x \"Elilim\", :y 3074/4030474384232008610441733} {:x \"Elmina\", :y 23881/32758963618713991750314720} {:x \"Liukio\", :y 8556160/13789553926342913879773721031} {:x \"Elston\", :y 53/728840950446605596590375} {:x \"Einritt\", :y 15950/230201622282982239651140109} {:x \"Elucet\", :y 268763/3948581316257286866021907000})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Martiniques\" \"Martinianus\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"d33420cc-8d52-4c57-b3bd-0ee3e7f1a693\", :values ({:x \"Martinianus\", :y 2516912538923134/97236344392036775599015217221875} {:x \"Martinianus\", :y 285872728675939268864/11185189886728455625200454533216466695} {:x \"Martinianus\", :y 1187848299779879421376/46762291968929653429060822079233914375} {:x \"Martinianus\", :y 102091067547628327168/4057001077559134752191351305268345547} {:x \"Martinianus\", :y 134916660575117324331524096/5379111262166601056124712775666157908339787} {:x \"Maximianus\", :y 105697361110/753032301762340913554216896959600343} {:x \"Wortinhaltes\", :y 476378963204189/26762726664702548864694868722757358203125} {:x \"Wortinhaltes\", :y 3149459205404/178606121670821755388186653553082613125} {:x \"Wortinhaltes\", :y 594587514318227584/33830132941076589525589919480669920931401875} {:x \"Weltinhaltes\", :y 2178185013092/14587844408954835648854885066908450634765625})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Grenzdorfe\" \"Kreuzböcke\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"3f357338-aaf8-4dad-a23e-e1f703ddbfca\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"3f357338-aaf8-4dad-a23e-e1f703ddbfca\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"3f357338-aaf8-4dad-a23e-e1f703ddbfca\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"3f357338-aaf8-4dad-a23e-e1f703ddbfca\", :values ({:x \"Kreuzböcke\", :y 2838486800435924/763225587142514121563515939476884114915625} {:x \"Kreuzhütte\", :y 5587572441803/158093358314150357607951378708781035600000} {:x \"Elennböcke\", :y 1651776929/12335808889358896797038523060339270159375000} {:x \"Ermunterte\", :y 8667466672271/74830690448967836554357073139032317701696000000} {:x \"Ermunterst\", :y 1681747264769/367304541593577719765115396289216257735782400000} {:x \"Exercices\", :y 737/239730111489840427588736488187534868000000} {:x \"Kleinodes\", :y 536/750453011834583449221046700787820688046875} {:x \"Etruriens\", :y 127/13098007029982702237006065617746545234560000} {:x \"Erminiens\", :y 9623/2086882823638847287763718475761316298367000000} {:x \"Kiloniens\", :y 4/192687212273897568253029035819654255622609375})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Calviniani\" \"Entwirrens\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0f78550a-447e-4a5a-a458-60a465450348\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0f78550a-447e-4a5a-a458-60a465450348\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"0f78550a-447e-4a5a-a458-60a465450348\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"0f78550a-447e-4a5a-a458-60a465450348\", :values ({:x \"Entwirrens\", :y 176378/62350333808755100729757015113739392853200625} {:x \"Entwichene\", :y 12164/9414589442248074886439371235605008633571875})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Feinperiode\" \"Imperoche\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"74e6b508-00f0-4da8-b366-cfd4215cd1a5\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"74e6b508-00f0-4da8-b366-cfd4215cd1a5\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"74e6b508-00f0-4da8-b366-cfd4215cd1a5\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"74e6b508-00f0-4da8-b366-cfd4215cd1a5\", :values ({:x \"Imperoche\", :y 475174117/662617261901995121100699928881884520} {:x \"Impulsive\", :y 2948/22967634422891398316217625813885211565} {:x \"Impulsion\", :y 8/1053572675419458884106326432220859764603} {:x \"Impressor\", :y 1/1121799956832813796309034981872412653395} {:x \"Impresion\", :y 2/4677182840341764459659205745241319374805})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Schau¬\" \"Schau-\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"4ab04f9b-1eb7-4dbd-bd3b-ffac495c70bc\", :values ({:x \"Schau-\", :y 591931891445105732352/13232329009163836808429119325} {:x \"Schau'\", :y 41243860886643376/116126674907211500859193195} {:x \"Scham-\", :y 76761892930021728/1488451546219115315156983055} {:x \"Schall-\", :y 6978353902729248/240265888988275495678224325} {:x \"Schalt-\", :y 1744588475682312/196028038747984812820604195} {:x \"Schaun\", :y 82487721773286752/12586210667574840487859172225} {:x \"Schall'\", :y 11183259459502/221399745280189198096100475} {:x \"Schon-\", :y 1071450008096/65011153125817347886739925} {:x \"Schann\", :y 123015854054522/35900711055951057113479975125} {:x \"Schinn-\", :y 133931251012/53100712858492032243367725})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]), :opts nil}"}
;; <=
