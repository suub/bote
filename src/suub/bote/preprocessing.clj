(ns suub.bote.preprocessing
  (:require [taoensso.timbre :refer [spy debug info error]]
            [clojure.java.io :as io]
            [clojure.core.reducers :as r]
            [clojure.set :as set]
            [error-codes.core :as ecodes]))

(defn- count-to-frequency
  "Takes a collection of tuples of the form [word count],
  and calculates the overall probability of occurence for each word."
  [dict]
  (let [sum (->> dict
                 (r/map second)
                 (r/fold +))]
    (r/map (fn [[w c]] [w (/ c sum)])
           dict)))

(defn read-dict
  "Takes space speperated dict entries and returns"
  [lines]
  (->> lines
      (r/map (fn [line]
               (if-let [[_ cnt _ simpl] (re-matches #"(.+?)\s+(.+?)\s+(.+?)" line)]
                 [simpl (bigint cnt)]
                 (error (str "Could not parse line:" \" line \")))))
      (r/remove nil?)
      count-to-frequency
      (into [])))

(def header
";This archive contains frequency counts from the Deutsches Textarchiv (\"DTA\",
;http://www.deutschestextarchiv.de), and the digitalised Polytechnischen Journal
;(\"Dingler\", http://dingler.culture.hu-berlin.de).
;The words are in contemporary German orthography as determined by unicruft
;(http://odo.dwds.de/~moocow/software/unicruft/).
")

(defn convert-dict! [in out]
  (with-open [in (io/reader in)]
    (let [lines (vec (line-seq in))]
      (spit out
            (str header
                 (pr-str (read-dict lines)))))))

(defn build-error [a b error-code]
  (cond
   (= 2 (count error-code))
   (let [[code [l r]] error-code]
     [(str (nth a l)) (str (nth b r))])
   (= 7 (get-in error-code [0 1]))
   (let [[code [ls rs] [le re]] error-code]
     [(subs a ls le) (subs b rs (inc re))])
   (= 7 (get-in error-code [0 0]))
   (let [[code [ls rs] [le re]] error-code]
     [(subs a ls (inc le)) (subs b rs re)])))

(defn visualize-errors
  "the codes are visualized by tuples [a b] where a ist the text in the ground truth a
   that was recognized as b by the ocr-engine. For example
   (visualize-errors 'Mammut' 'Rnarniiiiit' (ecodes/error-codes 'Mammut' 'Rnarniiiiit'))
   ;=> (['M' 'Rn']['m' 'rn']['m' 'iii']['u' 'ii'])"
  [a b error-codes]
  (->> error-codes
       (remove #(some #{8} (first %)))
       (map (partial build-error a b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Build substitution map with the visualized error codes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We have evaluated our ocr results on 200 pages of ground truth text.
;; With the error-codes library, we have extracted all the errors from
;; these 200 pages Now in a postprocessing step, we want to optimize the
;; results of the ocr-engine by going over the ocr text with
;; a) a list of allowed words with their probability
;; b) probabilities that characters xyz in the ocr-results are really xyz
;; in the ground truth.
;; from this two probabilities we can deduce the most probable word from
;; the ocr-results and hope to get less errors after this postprocessing
;; step.  Therefore, it is important to have accurate character
;; recognition probabilities, and this is it that we want to extract now

;; say we have the following ocr-results and ground-truth
(def ocr-results
  "Diescr Teset rnacht wenig Siun")

(def ground-truth
  "Dieser Text macht wenig Sinn")

;; Es wurden die folgenden Ersetzungen von ground-truth nach
;; ocr-results gemacht

(def ec (ecodes/error-codes ground-truth ocr-results))

ec
;=> ([[1 1] [4 4]] [[1 1] [9 9]] [[8 1] [10 10]] [[1 7] [12 13] [13
;14]] [[1 1] [26 28]]) 


;;visualized, these n-to-m substitutions have been made

(def visualized (visualize-errors ground-truth ocr-results ec))

visualized
;=> (["e" "c"] ["x" "s"] ["m" "rn"] ["n" "u"])

;;If we now start from the ocr-results and preprocess the results
;;we would like to have the probabilities of for example
;;that "rnacht" is  "macht" or "nacht".
;;Therefore we now build up a statistics with what probability the
;;character (sequence) in the ocr-results was correctly identified or
;;with wich probabilities it was recognized as something different.
;;As example, look at the letter c
;;number of c's in te ocr-results is

(count (filter #{\c} ocr-results))
;;=> 2

;;we need a function here to calculate these in the general case
;;because we are looking to m-to-n substitutions, we have to count
;;the occurences of a substring in a string.
(defn count-occurences
  "counts how often cs occures in s"
  [cs s]
  (loop [count 0 s s]
    (let [pos (.indexOf s cs)]
      (if (not= -1 pos)
        (recur (inc count) (.substring s (inc pos)))
        count))))

(count-occurences "rn" "rnarniiiiit")
;=> 2


;;how many times has it been falsely recognized ?
;;that is exactly the number of visualized errors that
;;contain c in their second element (remember, its gt to ocr)

(defn count-false
  "counts in how many errors cs occurs in the second eleement"
  [cs errors]
  (count (filter (comp #(not= -1 (.indexOf % cs)) second) errors)))

(count-false "c" visualized)
;=> 1

;;so we can estimate the probability, that a cs in the ocr-results is
;;really a cs with the probability of 1 - count-false/count-occurences
;;(= 1/2 in the case of c)

(defn identity-prob
  "the probability that the cs is really cs"
  [cs errors count-occ]
  (- 1 (/ (count-false cs errors) count-occ)))

(identity-prob "c" visualized 2)
;=> 1/2

;;now we can calculate for all character sequences in te ocr-text
;;the probability, that it is correct.
;;It remains to calculate the probabilties for the substitutions, that
;;occured in errors.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;WARNING: Note that the sum of the probabilities for the
;;substitutions from cs to something plus the identity-probability of
;;cs is not neccesary equal to 1. The reason is that cs could be a
;;part of a bigger ccs that also occurs in the errors. Then the number
;;of occurrences of cs in the errors is bigger than the count of all
;;entries [xyz cs] in errors, what is what the substitution
;;probabilities count!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;like described in the warning above, the substitution probability
;;that cs is sc in the ground truth text is the number of entries
;;[sc cs] in errors.
;;to build the map with the substitution probabilities we can filter
;;all errors that contain cs as their second element and group this
;;by the first argument.

(defn substitution-probabiltites
  "for each substitution [sc cs] that occurs in errors
   calculate the probability (count/count-occurrences)
   returns map from sc to prob"
  ([cs errors count-occ]
     (->> errors
          (filter (comp #{cs} second))
          (group-by first)
          (map (fn [[sc group]]
                 [sc (/ (count group) count-occ)]))
          (into {}))))

(substitution-probabiltites "c" visualized 2)
;=> {"e" 1/2}


;;with all this parts in place, we can build the whole substitution
;;list for the errors in this example.
;;To do this, we examine all unique elements in the right hand side
;;of visualization errors. This will be the keys of the substitution
;;map.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;WARNING noe quite, we need more entries in the substitution map
;;if for example n is not the right hand side of some error but
;;there exist an entry [_ "rn"] in the errors, the probability of
;;n is not one. To be precise, we need all keys we have now plus
;;all possible subsequences of each key.......
;;error-subsequences extracts all keys from the errors

(defn- all-subsequences [coll]
  (let [c (count coll)]
    (apply concat
           (for [i (range 1 (inc c))]
             (partition i 1 coll)))))

(defn error-subsequences
  "all the subsequences that don't have a identity-prob of 1"
  [errors]
  (->> errors
       (map second)
       (mapcat all-subsequences)
       (map #(apply str %))
       (into #{})))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;to build the substitution map, for each identified key
;;its occurrence count ist calculated an with it the
;;substitution probabilities as well as the identity probability.
;;These are converted to a map of the form
;;{key {key identity-prob subs1 prob1 subs2 prob2 ....}}
(defn build-substitution-map
  [errors ocr-text]
  (let [keys (error-subsequences errors)]
    (->>
     (for [k keys
           :let [count-occ (count-occurences k ocr-text)]
           :when (> count-occ 0)]
       (let [sub-prob (substitution-probabiltites k errors count-occ)
             identity-prob (identity-prob k errors count-occ)]
         [k (assoc sub-prob k identity-prob)]))
     (into {}))))

(build-substitution-map visualized ocr-results)
;=>{"c" {"c" 1/2, "e" 1/2}, "n" {"n" 2/3}, "r" {"r" 1/2}, "s" {"s" 1/2, "x" 1/2}, "u" {"u" 0, "n" 1}, "rn" {"rn" 0, "m" 1}}
;=>{"c" {"c" 1/2, "e" 1/2}, "n" {"n" 2/3}, "r" {"r" 1/2}, "s" {"s" 1/2,
;"x" 1/2}, "u" {"u" 0, "n" 1}, "rn" {"rn" 0, "m" 1}}

;;now try it on something bigger
;;the evaluated error-codes for the 200 ground truth pages are stored
;;in the repository of the ocr-visiaulizer.
;;we can now build the whole substitution map

(defn get-files-sorted [dir]
  (->> (file-seq (io/file dir))
       rest
       (sort-by #(.getName  %))))

(defn create-substitution [base-directory]
  (let [gts (get-files-sorted (io/file base-directory "ground-truth"))
        ocr-res (get-files-sorted (io/file base-directory "ocr-results"))
        error-codes (get-files-sorted (io/file base-directory "edits"))
        ocr-text (apply str (map slurp ocr-res))
        errors (for [[gt ocr codes] (map (fn [gf of cf]
                                           [(slurp gf) (slurp of)
                                            (read-string (slurp cf))])
                                         gts ocr-res error-codes)]
                 (visualize-errors gt ocr codes))
        errors (apply concat errors)]
    (prn "start-building")
    (build-substitution-map errors ocr-text)))

;;the current results are spitted to substitution-map.edn in the
;;resources folder
