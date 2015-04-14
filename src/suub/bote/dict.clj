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
           ; [error-codes.gorilla :as gv]
            [gorilla-repl.html :as gh]
            [clojure.core.reducers :as r]
            [clojure.core.async :as a]
            [clojure.test :as t]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [clojure.data.xml :as xml]
            [clojure.string :as string]
            [clojure.data.priority-map :as pm]
            [clojure.edn :as edn]
            [me.raynes.fs :as fs]
            [suub.bote.abbyy :as abbyy]
            [gorilla-renderable.core :as render]
            [marmoset.util :as marm]
            [error-codes.files :as ec]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ##Generating alternative interpretations.
;;; 
;;; Our main approach in improving the fulltext is by correcting single words produced by the OCR based on its known weaknesses in distinguishing character groups, and actual word probabilities based on existing dictionaries and word-n-grams.
;;; 
;;; ###A generative model.
;; **

;; @@
(defn alternatives
  "Expects:
    * adapter function that makes it possible to treat complex datastructures
      such as parsed XML, as a stream of characters without destroying metainformation,
      like bounding boxes or identifiers.
      Will take a string being matched and the word(fragment) being being processed
      and try to overlay the string as a prefix over the word(fragment).
      In case of a match it must return a tuple of the match and the rest,
      otherwise nil.
      Example:
        (keyword-adapter \"he\" [:h :e :l :l :o]) => [[:h :e] [:l :l :o]]
    * Possible character group to character group missinterpretations by the OCR.
    * A heuristic to be applied while searching, similar to the distance function in A*,
      it allows for pruning search paths by returning nil and optimised search
      by returning approximations to the final penalty of the words a prefix might lead to.
    * Calculates the penalty given to an alternative.
      This might be the frequency of the word in the dictionary or additional information like
      n-gram scores. The found word will be omitted when nil is returned.
    * A query to be matched.

    Note that the penalty score must be monotonically falling along the search path.
    For example the final penalty may never be higher than the heuristic penalty.

    Retuns the possible corrections, their probability of a match
    and a collection of their substitutions."
  [adapter substs heuristic-penalty final-penalty query]
  (letfn [(iterations [run]
            (for [[[ocr truth] prob] substs
                  :let [[from rest :as matched]
                        (adapter ocr (:rest run))]
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
                                        :let [word-prob (if (empty? rest)
                                                          (final-penalty word)
                                                          (heuristic-penalty word))]
                                        :when word-prob
                                        :let [prob (* word-prob subst-prob)]]
                                        [(assoc iteration
                                           :word-prob word-prob
                                           :prob prob)
                                          prob])))))))))]
    (->> (worker (pm/priority-map-by >
                  {:rest query
                   :word ""
                   :subst-prob 1
                   :substs []}
                  1))
         (map #(dissoc % :rest)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/alternatives</span>","value":"#'suub.bote.dict/alternatives"}
;; <=

;; **
;;; ###Parameters for the generative model.
;;; ####Adapters
;; **

;; @@
(defn string-adapter
  "Adapter function for plain string.
  Returns the rest of the stringafter the prefix elements have been removed
  as well as the rest.
  When no match can be found returns nil."
  [p q]
  (when-let [r (loop [pre (seq p),
                      post (seq q)]
                 (cond (empty? pre) post
                       (= (first pre) (first post)) (recur (rest pre) (rest post))))]
    [p r]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/string-adapter</span>","value":"#'suub.bote.dict/string-adapter"}
;; <=

;; **
;;; #### Error model.
;;; Our error model consist of a simple mapping of groups of characters in the scanned documents and the groups of characters that they could be misrecognized as by the OCR engine to the probability at which these missrecognitions ocur.
;;; This mans that a "u" might be misrecognized as a "n" or a combination of "rn" might be mischaracterized as a "m".
;; **

;; @@
(def example-errors
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/example-errors</span>","value":"#'suub.bote.dict/example-errors"}
;; <=

;; **
;;; #### Dictionaries and N-Grams
;;; In addition to the probability that a word was correctly recognized from the error model,
;;; different word probabilities without sentence context as with a dictionary or with sentence context
;;; as with N-Grams is used.
;; **

;; @@
(defn index-bigrams
  "Expects a collection of bigramms of the form
  {[\"left\" \"right\"] count}
  and returns a double index for the relative probability of the form
  {\"left\" {\"right\" probability}}."
  [bigrams]
  (->> bigrams
       (group-by ffirst)
       seq
       (r/fold
         merge
         (fn [acc [k v]]
           (let [c (apply + (map second v))]
             (assoc
               acc
               k
               (into {} (for [[[l r] p] v]
                          [r (/ p c)]))))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/index-bigrams</span>","value":"#'suub.bote.dict/index-bigrams"}
;; <=

;; **
;;; ##### DTA
;;; A dictionary generated from the corpus of the DTA (Deutsches Text Archiv) in Berlin.
;; **

;; @@
(defn read-DTA-dictionary
  "Reads a DTA dicitonary from the given path.
  Selects the word form via the column argument,
  either :original or :simplified."
  [path column]
  (with-open [in (io/reader path)]
    (let [words (->> (line-seq in)
                     (r/map #(string/split % #"\s+"))
                     (r/map (fn [[cnt orig simpl]] {(case column
                                                      :original orig
                                                      :simplified simpl)
                                                    (bigint cnt)}))
                     (r/fold (partial merge-with +)))
          word-count (r/fold + (r/map second words))]
      (->> words
           (r/map (fn [[w c]] [w (/ c word-count)]))
           (into {})))))
;; @@

;; @@
(def DTA-dictionary (read-DTA-dictionary "resources/dta-freq.d/dta-core-1850+.fuw"
                                         :simplified))
;; @@

;; **
;;; ####Grenzbote
;;; A dictionary and bigrams generated from the raw grenzboten material itself.
;; **

;; @@
(defn read-grenzbote-ngram [path n]
  (->> path
       abbyy/files
       (pmap #(->> %
                   second
                   xml/parse
                   abbyy/lines
                   abbyy/remove-linewrap
                   (mapv abbyy/text)))
       (apply concat)
       (partition n 1)
       frequencies))
;; @@

;; @@
(def grenzbote-dictionary (edn/read-string (slurp "resources/gb-dict.edn")))
(def grenzbote-bigrams    (edn/read-string (slurp "resources/gb-bigr.edn")))
;; @@

;; **
;;; ##### Potsdam Dictionary
;; **

;; @@
(def potsdam-dictionary (->> "resources/potsdam-dict.edn"
                              slurp
                              edn/read-string
                              (map #(update-in % [1] bigint))
                              (into {})))
;; @@

;; **
;;; ####Heuristics
;;; Alternatives are generated lazily in ascending probability order.
;;; This is done to improve the runtime of the general correction case,
;;; where one is only interested in the most probable alternative.
;;; 
;;; To do this the generative model employs a heuristic function to prioritize the search space similar to the @@A*@@ algoritm. This function will map partial runs of the search space in the form of the prefix analyzed so far to the best possible outcome when following this search path.
;;; 
;;; For words from a dictionary this means the highest probability of a word with this prefix.
;; **

;; @@
(defn word-prefixes
  "Generate all prefixes for a given word."
  [[w p]]
  (into {}
        (r/map (fn [n] [(subs w 0 n) p])
          (range 0 (inc (count w))))))

(defn merge-prefixes [& m]
  (apply merge-with max m))

(defn heuristic-for-dictionary
  "Will create a mapping from every prefix of every word
  to the highest probability that can be "
  [dictionary]
  (->> dictionary
       (r/map word-prefixes)
       (r/reduce merge-prefixes)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/heuristic-for-dictionary</span>","value":"#'suub.bote.dict/heuristic-for-dictionary"}
;; <=

;; **
;;; ##### DTA
;; **

;; @@
(def DTA-heuristic (heuristic-for-dictionary DTA-dictionary))
;; @@

;; **
;;; ##### Potsdam
;; **

;; @@
(def potsdam-heuristic (time (prefixes pots-dict)))
;; @@

;; **
;;; ##Evaluation
;; **

;; @@
(defn penalty-for-dictionary-and-bigrams [dictionary bigrams default-penalty]
  (fn [last-word word]
    (let [d (dictionary word)
          b (or (get (bigrams last-word) word) default-penalty)]
      (when d (* d b)))))

(defn correct-page [substs heuristic-penalty dictionary bigrams default-bigram-penalty page-xml] 
  (let [final-penalty (penalty-for-dictionary-and-bigrams
                        dictionary
                        bigrams
                        default-bigram-penalty)]  
    (as-> page-xml %
          (abbyy/lines %)
          (abbyy/remove-linewrap %)
          (reduce (fn [[acc lst] val]
                    (let [alternative
                          (and (not (dictionary (abbyy/text val)))
                               (Character/isLetterOrDigit (first (abbyy/text val)))
                               (first (alternatives abbyy/abbyy-adapter
                                                    substs
                                                    heuristic-penalty
                                                    #(final-penalty lst %)
                                                    val)))]
                      [(conj acc alternative) (abbyy/text alternative)]))
                  [[] nil]
                  (rest %))
          (first %)
          (mapcat :substs %)
          (abbyy/change % page-xml))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/correct-page</span>","value":"#'suub.bote.dict/correct-page"}
;; <=

;; @@
(def f (future
  (time (dorun
  (pmap #(do
           (spit (str "/Users/ticking/Desktop/test" (first %) ".xml")
               (with-out-str (xml/emit-str-raw (correct (xml/parse-str-raw (second %) {:preserve-whitespace true}) dta)))))
        files)))))
;; @@

;; @@
(defn evaluate-algorithm [substs heuristic-penalty dictionary bigrams default-bigram-penalty
                          ocr-base-directory corrected-base-directory num-pages]
  (.mkdirs (io/file corrected-base-directory))
  (.mkdir (io/file corrected-base-directory "ground-truth"))
  (doseq [f (take num-pages (ec/get-files-sorted (io/file ocr-base-directory "ground-truth")))]
    (println "f " f)
    (io/copy f (io/file corrected-base-directory "ground-truth" (.getName f))))
  (.mkdir (io/file corrected-base-directory  "edits"))
  (.mkdir (io/file corrected-base-directory "ocr-results"))
  (let [ocr-text (->> (ec/get-files-sorted 
                        (io/file ocr-base-directory "ocr-xml-results"))
                      (take num-pages))]
    (println "starte Nachkorrektur")
    (doall 
      (pmap #(as-> % x
                  (do (println x) x)
                  (slurp x)
                  (xml/parse-str-raw x :preserve-whitespace true)
                  (correct-page substs heuristic-penalty dictionary bigrams default-bigram-penalty x)
                  (abbyy/page-text x)
                  (spit (io/file corrected-base-directory "ocr-results" (.getName %)) x))
            ocr-text))
    	
    (println "starte Auszählung")
    (ec/deploy-error-codes corrected-base-directory)
    (println "starte Auswärtung")
    (let [statistic (ec/gen-statistics-for-base-directories [corrected-base-directory])
          correction-statistic (ec/generate-correction-statistics ocr-base-directory corrected-base-directory)]
      (spit (clojure.java.io/file corrected-base-directory "statistics.edn") (pr-str (second (first statistic))))
      (spit (clojure.java.io/file corrected-base-directory "correction-statistic.edn") (pr-str correction-statistic))
      (spit (clojure.java.io/file corrected-base-directory "parameters") (pr-str [substs heuristic-penalty dictionary bigrams default-bigram-penalty]))
      {:statistic statistic
       :correction-statistic correction-statistic})))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/evaluate-algorithm</span>","value":"#'suub.bote.dict/evaluate-algorithm"}
;; <=

;; @@
(map #(spit (str "/Users/ticking/Desktop/gt/" % ".xml") (slurp (str "http://brema.suub.uni-bremen.de/grenzboten/download/fulltext/fr/" %))) files)
;; @@

;; @@
(binding [*out* (clojure.java.io/writer "output.txt")]
  (evaluate-algorithm gerrit-substitutions
                      gerrit-heuristic
                      gerrit-dictionary
                      grenzbote-bigrams
                      1/10000
                      "/Users/ticking/Desktop/ocr-engine-results/abby-more-text"
                      "/Users/ticking/Desktop/bigram-correction-results" 200))
;; @@

;; @@
(binding [*out* (clojure.java.io/writer "output.txt")]
  (evaluate-algorithm gerrit-substitutions
                      gerrit-heuristic
                      gerrit-dictionary
                      {}
                      1
                      "/Users/ticking/Desktop/ocr-engine-results/abby-more-text"
                      "/Users/ticking/Desktop/bigram-correction-results2" 200))
;; @@

;; @@
(->> "/Users/ticking/Desktop/ocr-engine-results/abby-more-text/ground-truth"
     io/file
     file-seq
     (filter #(and (fs/file? %)
                   (= ".txt" (fs/extension %))
                   (re-matches #"\d+" (fs/base-name % true))))
     (map #(fs/base-name % true))
     (pmap #(spit (str "/Users/ticking/Desktop/gt/" % ".txt")
                  (slurp (str "http://brema.suub.uni-bremen.de/grenzboten/download/fulltext/fr/" %))))
     dorun)
;; @@

;; **
;;; ##Correction
;; **

;; @@
(def params (read-string (slurp "/Users/ticking/Desktop/ocr-engine-results/abby-cleaned-gerrit-1800/parameters")))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/params</span>","value":"#'suub.bote.dict/params"}
;; <=

;; @@
(def gerrit-substitutions (:substs params))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/gerrit-substitutions</span>","value":"#'suub.bote.dict/gerrit-substitutions"}
;; <=

;; @@
(def gerrit-dictionary (:dict params))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/gerrit-dictionary</span>","value":"#'suub.bote.dict/gerrit-dictionary"}
;; <=

;; @@
(def gerrit-heuristic (heuristic-for-dictionary gerrit-dictionary))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/gerrit-heuristic</span>","value":"#'suub.bote.dict/gerrit-heuristic"}
;; <=

;; @@
(defn correct-page [substs heuristic-penalty final-penalty page-xml] 
  (as-> page-xml %
        (abbyy/lines %)
        (abbyy/remove-linewrap %)
        (map (fn [v]
                 (and (not (final-penalty (abbyy/text v)))
                      (Character/isLetterOrDigit (first (abbyy/text v)))
                      (first (alternatives abbyy/abbyy-adapter
                                           substs
                                           heuristic-penalty
                                           final-penalty
                                           v))))
             %)
        (mapcat :substs %)
        (abbyy/change % page-xml)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/correct-page</span>","value":"#'suub.bote.dict/correct-page"}
;; <=

;; @@
(def c (atom 0))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/c</span>","value":"#'suub.bote.dict/c"}
;; <=

;; @@
(def f
(->> "/Volumes/5TB/data/"
     io/file
     file-seq
     (filter #(and (fs/file? %)
                   (= ".xml" (fs/extension %))
                   (re-matches #"\d+" (fs/base-name % true))))
     (pmap (fn [v]
             (swap! c inc)
             (as-> v x
                  (slurp x)
                  (xml/parse-str-raw x :preserve-whitespace true)
                  (correct-page gerrit-substitutions gerrit-heuristic gerrit-dictionary x)
                  (xml/emit-str-raw x)
                  (spit (str "/Volumes/4TB/processed/" (fs/base-name v true) ".xml") x))))
     dorun
     future))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/f</span>","value":"#'suub.bote.dict/f"}
;; <=

;; @@
c
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-atom'>#&lt;Atom@57e3677: 186740&gt;</span>","value":"#<Atom@57e3677: 186740>"}
;; <=

;; @@
(future-done? f)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; @@
(def f
(->> "/Volumes/5TB/data/"
     io/file
     file-seq
     (filter #(and (fs/file? %)
                   (= ".xml" (fs/extension %))
                   (re-matches #"\d+" (fs/base-name % true))))
     (pmap (fn [v]
             (swap! c inc)
             (as-> v x
                  (slurp x)
                  (xml/parse-str-raw x :preserve-whitespace true)
                  (xml/emit-str-raw x)
                  (spit (str "/Volumes/4TB/raw/" (fs/base-name v true) ".xml") x))))
     dorun
     future))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/f</span>","value":"#'suub.bote.dict/f"}
;; <=

;; @@
c
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-atom'>#&lt;Atom@57e3677: 121995&gt;</span>","value":"#<Atom@57e3677: 121995>"}
;; <=

;; @@
(future-done? f)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; @@

;; @@
