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
            [clojure.string :as string]
            [clojure.data.priority-map :as pm]
            [instaparse.core :as insta]
            [instaparse.combinators :as instac]
            [clojure.edn :as edn]
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
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/read-substs</span>","value":"#'suub.bote.dict/read-substs"}
;; <=

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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta</span>","value":"#'suub.bote.dict/dta"}
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/correct-word</span>","value":"#'suub.bote.dict/correct-word"}
;; <=

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
;;; &quot;Elapsed time: 152.940353 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/f</span>","value":"#'suub.bote.dict/f"}
;; <=

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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/evaluate-algorithm</span>","value":"#'suub.bote.dict/evaluate-algorithm"}
;; <=

;; @@
#_(binding [*out* (clojure.java.io/writer "/home/kima/dummyoutput.txt")]
  (evaluate-algorithm 
   dta
   "/home/kima/programming/grenzbote-files/grenzbote/abby" 
   "/home/kima/programming/grenzbote-files/grenzbote/abby-corr-normal-10-pages"
   10))
;; @@
;; <=

(defn download-xml [vlid]
  (suub.bote.abbyy/parse
   (str "http://brema.suub.uni-bremen.de/grenzboten/download/fulltext/fr/" vlid)))
