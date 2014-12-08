(ns suub.bote.postcorrection
  (:require [clojure.java.io :as io :refer [file]]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [me.raynes.fs :as fs]
            [error-codes.core :as ec]
            [suub.bote.abbyy :as abbyy]
            [suub.bote.dict :as dict]))

(defonce dict (dissoc (dict/read-dict "resources/dta-freq.d/dta-core.fuw")
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

(defonce dta-core-prfx (time (dict/prefixes dict)))




;; @@
(def simple-subst
        {["u" "n"] 1/4
         ["n" "u"] 1/4
         ["c" "e"] 1/4
         ["e" "c"] 1/4
         ["N" "U"] 1/8
         ["N" "R"] 1/8         
         ["a" "a"] 1
         ["b" "b"] 1
         ["c" "c"] 3/4
         ["d" "d"] 1
         ["e" "e"] 3/4
         ["f" "f"] 1
         ["g" "g"] 1
         ["h" "h"] 1
         ["i" "i"] 1
         ["j" "j"] 1
         ["k" "k"] 1
         ["l" "l"] 1
         ["m" "m"] 7/8
         ["n" "n"] 3/4
         ["o" "o"] 1
         ["p" "p"] 1
         ["q" "q"] 1
         ["r" "r"] 1
         ["s" "s"] 1
         ["t" "t"] 1
         ["u" "u"] 3/4
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
         ["N" "N"] 3/4
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
         ["ü" "ü"] 1
         ["ä" "ä"] 1
         ["ö" "ö"] 1
         ["ß" "ß"] 1
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
         ["-" "-"] 1
         ["," ","] 1
         ["'" "'"] 1
         ["¬" "¬"] 1
         ["rn" "m"] 1/8
         ["iii" "m"] 1/8
         ["m" "en"] 1/8})

(def subst
  (assoc simple-subst
    ["f" "s"] 1/4
    ["s" "f"] 1/4
    ["f" "f"] 3/4
    ["ii" "u"] 1/4))

(def dta {:matcher dict/simple-matcher
          :dict dict
          :prefixes dta-core-prfx
          :subst simple-subst})




;; @@
(defn correct-word [idx word]
  (if (Character/isLetterOrDigit (first word))
    (or (:word (first (dict/transform idx word)))
        word)
    word))

(ec/error-codes "a" "b")

(defn correct-page [idx page]
  (->> page
       (partition-by #(Character/isLetterOrDigit %))
       (mapcat #(correct-word idx %))
       (apply str)))

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

