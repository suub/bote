(ns suub.bote.preprocessing
  (:require [taoensso.timbre :refer [spy debug info error]]
            [clojure.java.io :as io]
            [clojure.core.reducers :as r]
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
   (= 2 (count error-code)) (do (println error-code)(let [[code [l r]] error-code] {(str (nth a l))
                                                                                    (str (nth b r))}))
   (= 7 (get-in error-code [0 1])) (let [[code [ls rs] [le re]] error-code] {(subs a ls le)
                                                                             (subs b rs (inc re))})
   (= 7 (get-in error-code [0 0])) (let [[code [ls rs] [le re]] error-code] {(subs a ls (inc le))
                                                                             (subs b rs re)})))

(defn build-errors [a b]
  (->> (ecodes/error-codes a b)
       (remove #(some #{8} (first %)))
       (map (partial build-error a b))))


(build-errors "iii" "m")

(apply merge-with vector [{"u" "n"} {"u" "ii"}])
