;; gorilla-repl.fileformat = 1

;; **
;;; # Dictionary error correction.
;;; 
;;; This code uses a weighted finite state transducer aproach to give all possible matches for a given word.
;; **

;; @@
(ns suub.bote.dict
<<<<<<< HEAD
  (:require ;[gorilla-plot.core :as plot]
            ;[gorilla-repl.table :as table]
=======
  (:require [gorilla-plot.core :as plot]
            [gorilla-repl.table :as table]
            [error-codes.gorilla :as gv]
            [gorilla-repl.html :as gh]
>>>>>>> dev
            [clojure.core.reducers :as r]
            [clojure.core.async :as a]
            [clojure.test :as t]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [suub.bote.clojure.xml :as xml]
            [clojure.string :as string]
            [clojure.data.priority-map :as pm]
<<<<<<< HEAD
            [instaparse.core :as insta]
            [instaparse.combinators :as instac]
            [clojure.edn :as edn]
            [error-codes.files :as ec]))
=======
            [clojure.edn :as edn]
            [me.raynes.fs :as fs]
            [suub.bote.abbyy :as abbyy]
            [gorilla-renderable.core :as render]))
>>>>>>> dev
;; @@

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
  (when-let [r (drop-prefix p q)]
    [p r]))
;; @@

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

;; **
;;; #Setup & loading
;; **

;; **
;;; ## DTA Dictionary
;; **

;; @@
(def simple-subst
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

;; @@
(def dta-dict (-> "resources/dta-freq.d/dta-core-1850+.fuw"
                  read-dict))
;; @@

;; @@
<<<<<<< HEAD
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
=======
(def dta-prfx (time (prefixes dta-dict)))
;; @@

;; @@
(def dta  {:matcher abbyy/matcher
           :dict dta-dict
           :prefixes dta-prfx
           :substs simple-subst})
>>>>>>> dev
;; @@

;; **
;;; ## Gold-Potsdam Dictionary
;; **

;; @@
#_(def gold-subst (read-substs "resources/substitutions.edn"))
#_(def gold-subst (read-string (slurp "resources/new-subsstitution.edn")))

#_(defn create-gold-subst [gold-subst]
  (into {} (for [[a v] gold-subst
                 [b v] v]
             [[a b] v])))

#_(def gold-subst (create-gold-subst gold-subst))
;; @@

;; @@
#_(def pots-dict (->> "resources/dict.edn"
                    slurp
                    edn/read-string
                    (map #(update-in % [1] bigint))
                    (into {})))
;; @@

;; @@
<<<<<<< HEAD
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
(def dta-prfx (time (prefixes dta-dict)))
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
=======
(def pots-prfx (time (prefixes pots-dict)))
;; @@

;; @@
(def pots {:matcher abbyy/matcher
>>>>>>> dev
           :dict pots-dict
           :prefixes pots-prfx
           :substs simple-subst})
;; @@
<<<<<<< HEAD
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta</span>","value":"#'suub.bote.dict/dta"}
;; <=
=======
>>>>>>> dev

;; **
;;; #Deployment code
;; **

;; @@
<<<<<<< HEAD
(defn correct-word [idx word]
  (if (and (not (dta-dict (apply str word)))
           (Character/isLetterOrDigit (first word)))
    (or (:word (first (transform idx word)))
        word)
    word))
=======
(defn correct [p idx]
  (abbyy/change
    (->> p
      abbyy/lines
      abbyy/remove-linewrap
      (map #(first (transform idx %)))
      (remove nil?)
      (mapcat :substs))
    p))
>>>>>>> dev
;; @@

;; @@
<<<<<<< HEAD
;;error-codes aufwärmen - core.matrix ladezeit
(ec/error-codes "a" "b")
;; @@

;; @@
(defn correct-page [idx page]
  (->> page
       (partition-by #(Character/isLetterOrDigit %))
       (mapcat #(correct-word idx %))
       (apply str)))
=======
(defn text [p]
  (apply str (map (fn [l]
                    (str (apply str
                                (map :char l))
                         "\n"))
                  (abbyy/lines p))))
>>>>>>> dev
;; @@

;; @@
<<<<<<< HEAD
#_(def files (rest (file-seq (io/file "/Users/ticking/Desktop/ocr-engine-results/abby_verbessert/unverbessert"))))
=======
(defn download-xml [vlid]
  (xml/parse
    (str "http://brema.suub.uni-bremen.de/grenzboten/download/fulltext/fr/" vlid)))
>>>>>>> dev
;; @@

;; @@
<<<<<<< HEAD
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

=======
(def pages
  (into {}
    (->> "resources/ground-truth"
             io/file
             file-seq
             (filter fs/file?)
             (map #(hash-map :truth (slurp %)
                             :raw (download-xml (fs/base-name % true)))))))
;; @@

;; @@
#_(gv/error-view "hallo dies ist ein test"
               "hallu dis ist cin test")
;; @@

;; @@
;(gv/error-view gp cp)
;; @@

;; @@
;(dta-dict "Feierlichkeiten")
;; @@

;; @@
(apply str (take 1000 (with-out-str (xml/emit (correct p dta)))))
;; @@

;; @@
(time (doall
  (pmap #(spit (str "corrected/" (first %) ".xml")
               (with-out-str (xml/emit (correct (second %) dta))))
        x)))
;; @@

;; **
;;; #Difficult words
;; **

>>>>>>> dev
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
<<<<<<< HEAD
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/evaluate-algorithm</span>","value":"#'suub.bote.dict/evaluate-algorithm"}
;; <=
=======
>>>>>>> dev

;; @@
#_(binding [*out* (clojure.java.io/writer "/home/kima/dummyoutput.txt")]
  (evaluate-algorithm 
   dta
   "/home/kima/programming/grenzbote-files/grenzbote/abby" 
   "/home/kima/programming/grenzbote-files/grenzbote/abby-corr-normal-10-pages"
   10))
;; @@
<<<<<<< HEAD
;; <=

;; **
;;; ##Difficult words.
;;; 
;;; Due to the statistical approach, some words from the dictionary might not be transformed to themselves, but instead to other words from the dictionary that are more likely and/or contain more expected characters.
;; **
;; (comment
;;   ;; @@
;;   (def difficult (filter #(not= % (->> %
;;                                        (transform dta)
;;                                        first
;;                                        :word))
;;                          (keys (:dict dta))))
;;   ;; @@
;;   ;; =>
;; ;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/difficult</span>","value":"#'suub.bote.dict/difficult"}
;;   ;; <=

;;   ;; @@
;;   (table/table-view (take 20 (filter second
;;                                      (map (fn [w] (let [r (take 10 (transform dta w))]
;;                                                     [w
;;                                                      (:word (first r))
;;                                                      (plot/bar-chart
;;                                                       (map :word r)
;;                                                       (map :prob r)
;;                                                       :plot-size 600)]))
;;                                           difficult))))
;;   ;; @@
;;   ;; =>
;; ;;; {"type":"list-like","open":"<center><table>","close":"</table></center>","separator":"\n","items":[{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;merek&quot;</span>","value":"\"merek\""},{"type":"html","content":"<span class='clj-string'>&quot;merck&quot;</span>","value":"\"merck\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"20c615e9-8682-4204-b849-61fd0a36b19a","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"20c615e9-8682-4204-b849-61fd0a36b19a","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"20c615e9-8682-4204-b849-61fd0a36b19a"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"20c615e9-8682-4204-b849-61fd0a36b19a","values":[{"x":"merck","y":1.24082953980114E-7},{"x":"merek","y":5.474247969710913E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"20c615e9-8682-4204-b849-61fd0a36b19a\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"20c615e9-8682-4204-b849-61fd0a36b19a\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"20c615e9-8682-4204-b849-61fd0a36b19a\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"20c615e9-8682-4204-b849-61fd0a36b19a\", :values ({:x \"merck\", :y 17/137005120} {:x \"merek\", :y 3/548020480})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"merek\" \"merck\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"20c615e9-8682-4204-b849-61fd0a36b19a\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"20c615e9-8682-4204-b849-61fd0a36b19a\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"20c615e9-8682-4204-b849-61fd0a36b19a\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"20c615e9-8682-4204-b849-61fd0a36b19a\", :values ({:x \"merck\", :y 17/137005120} {:x \"merek\", :y 3/548020480})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;kounten&quot;</span>","value":"\"kounten\""},{"type":"html","content":"<span class='clj-string'>&quot;konnten&quot;</span>","value":"\"konnten\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"f99d7767-5329-484c-95c7-90b2c510201d","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"f99d7767-5329-484c-95c7-90b2c510201d","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"f99d7767-5329-484c-95c7-90b2c510201d"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"f99d7767-5329-484c-95c7-90b2c510201d","values":[{"x":"konnten","y":9.657746471706417E-6},{"x":"kounten","y":3.519159409099873E-9},{"x":"konuten","y":7.820354242444161E-10}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"f99d7767-5329-484c-95c7-90b2c510201d\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"f99d7767-5329-484c-95c7-90b2c510201d\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"f99d7767-5329-484c-95c7-90b2c510201d\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"f99d7767-5329-484c-95c7-90b2c510201d\", :values ({:x \"konnten\", :y 74097/7672286720} {:x \"kounten\", :y 27/7672286720} {:x \"konuten\", :y 3/3836143360})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"kounten\" \"konnten\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"f99d7767-5329-484c-95c7-90b2c510201d\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"f99d7767-5329-484c-95c7-90b2c510201d\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"f99d7767-5329-484c-95c7-90b2c510201d\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"f99d7767-5329-484c-95c7-90b2c510201d\", :values ({:x \"konnten\", :y 74097/7672286720} {:x \"kounten\", :y 27/7672286720} {:x \"konuten\", :y 3/3836143360})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Busbeqn&quot;</span>","value":"\"Busbeqn\""},{"type":"html","content":"<span class='clj-string'>&quot;Busbequ&quot;</span>","value":"\"Busbequ\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"f859d4c0-cccf-47ed-ac7e-40f3e10752d7","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"f859d4c0-cccf-47ed-ac7e-40f3e10752d7","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"f859d4c0-cccf-47ed-ac7e-40f3e10752d7"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"f859d4c0-cccf-47ed-ac7e-40f3e10752d7","values":[{"x":"Busbequ","y":6.256283393955329E-9},{"x":"Busbeqn","y":4.692212545466497E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"f859d4c0-cccf-47ed-ac7e-40f3e10752d7\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"f859d4c0-cccf-47ed-ac7e-40f3e10752d7\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"f859d4c0-cccf-47ed-ac7e-40f3e10752d7\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"f859d4c0-cccf-47ed-ac7e-40f3e10752d7\", :values ({:x \"Busbequ\", :y 3/479517920} {:x \"Busbeqn\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Busbeqn\" \"Busbequ\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"f859d4c0-cccf-47ed-ac7e-40f3e10752d7\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"f859d4c0-cccf-47ed-ac7e-40f3e10752d7\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"f859d4c0-cccf-47ed-ac7e-40f3e10752d7\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"f859d4c0-cccf-47ed-ac7e-40f3e10752d7\", :values ({:x \"Busbequ\", :y 3/479517920} {:x \"Busbeqn\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;ehc&quot;</span>","value":"\"ehc\""},{"type":"html","content":"<span class='clj-string'>&quot;ehe&quot;</span>","value":"\"ehe\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"c2257117-ce0b-4725-b093-fac68214e072","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"c2257117-ce0b-4725-b093-fac68214e072","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"c2257117-ce0b-4725-b093-fac68214e072"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"c2257117-ce0b-4725-b093-fac68214e072","values":[{"x":"ehe","y":1.943827250501921E-5},{"x":"che","y":2.397546825083548E-6},{"x":"ehc","y":6.256283393955329E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c2257117-ce0b-4725-b093-fac68214e072\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c2257117-ce0b-4725-b093-fac68214e072\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"c2257117-ce0b-4725-b093-fac68214e072\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"c2257117-ce0b-4725-b093-fac68214e072\", :values ({:x \"ehe\", :y 9321/479517920} {:x \"che\", :y 3449/1438553760} {:x \"ehc\", :y 3/479517920})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"ehc\" \"ehe\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c2257117-ce0b-4725-b093-fac68214e072\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c2257117-ce0b-4725-b093-fac68214e072\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"c2257117-ce0b-4725-b093-fac68214e072\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"c2257117-ce0b-4725-b093-fac68214e072\", :values ({:x \"ehe\", :y 9321/479517920} {:x \"che\", :y 3449/1438553760} {:x \"ehc\", :y 3/479517920})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;codem&quot;</span>","value":"\"codem\""},{"type":"html","content":"<span class='clj-string'>&quot;eodem&quot;</span>","value":"\"eodem\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca","values":[{"x":"eodem","y":4.671358267486646E-7},{"x":"codem","y":4.37939837576873E-8}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca\", :values ({:x \"eodem\", :y 1/2140705} {:x \"codem\", :y 3/68502560})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"codem\" \"eodem\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca\", :values ({:x \"eodem\", :y 1/2140705} {:x \"codem\", :y 3/68502560})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Rheingan&quot;</span>","value":"\"Rheingan\""},{"type":"html","content":"<span class='clj-string'>&quot;Rheingau&quot;</span>","value":"\"Rheingau\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"666d7580-6aab-437e-b97e-76acd974305a","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"666d7580-6aab-437e-b97e-76acd974305a","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"666d7580-6aab-437e-b97e-76acd974305a"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"666d7580-6aab-437e-b97e-76acd974305a","values":[{"x":"Rheingau","y":1.157412427881736E-7},{"x":"Rheingan","y":9.384425090932993E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"666d7580-6aab-437e-b97e-76acd974305a\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"666d7580-6aab-437e-b97e-76acd974305a\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"666d7580-6aab-437e-b97e-76acd974305a\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"666d7580-6aab-437e-b97e-76acd974305a\", :values ({:x \"Rheingau\", :y 111/959035840} {:x \"Rheingan\", :y 9/959035840})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Rheingan\" \"Rheingau\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"666d7580-6aab-437e-b97e-76acd974305a\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"666d7580-6aab-437e-b97e-76acd974305a\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"666d7580-6aab-437e-b97e-76acd974305a\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"666d7580-6aab-437e-b97e-76acd974305a\", :values ({:x \"Rheingau\", :y 111/959035840} {:x \"Rheingan\", :y 9/959035840})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;vergenden&quot;</span>","value":"\"vergenden\""},{"type":"html","content":"<span class='clj-string'>&quot;vergeuden&quot;</span>","value":"\"vergeuden\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"9e90f240-a681-4bec-9b52-c521c9c1c744","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"9e90f240-a681-4bec-9b52-c521c9c1c744","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"9e90f240-a681-4bec-9b52-c521c9c1c744"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"9e90f240-a681-4bec-9b52-c521c9c1c744","values":[{"x":"vergeuden","y":3.695117379554866E-8},{"x":"vergenden","y":2.639369556824904E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"9e90f240-a681-4bec-9b52-c521c9c1c744\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"9e90f240-a681-4bec-9b52-c521c9c1c744\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"9e90f240-a681-4bec-9b52-c521c9c1c744\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"9e90f240-a681-4bec-9b52-c521c9c1c744\", :values ({:x \"vergeuden\", :y 81/2192081920} {:x \"vergenden\", :y 81/30689146880})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"vergenden\" \"vergeuden\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"9e90f240-a681-4bec-9b52-c521c9c1c744\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"9e90f240-a681-4bec-9b52-c521c9c1c744\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"9e90f240-a681-4bec-9b52-c521c9c1c744\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"9e90f240-a681-4bec-9b52-c521c9c1c744\", :values ({:x \"vergeuden\", :y 81/2192081920} {:x \"vergenden\", :y 81/30689146880})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;eigeuer&quot;</span>","value":"\"eigeuer\""},{"type":"html","content":"<span class='clj-string'>&quot;eigener&quot;</span>","value":"\"eigener\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"63964078-dbe0-4a14-8839-47668ef3fa31","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"63964078-dbe0-4a14-8839-47668ef3fa31","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"63964078-dbe0-4a14-8839-47668ef3fa31"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"63964078-dbe0-4a14-8839-47668ef3fa31","values":[{"x":"eigener","y":2.483353489688143E-6},{"x":"eigeuer","y":3.519159409099873E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"63964078-dbe0-4a14-8839-47668ef3fa31\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"63964078-dbe0-4a14-8839-47668ef3fa31\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"63964078-dbe0-4a14-8839-47668ef3fa31\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"63964078-dbe0-4a14-8839-47668ef3fa31\", :values ({:x \"eigener\", :y 19053/7672286720} {:x \"eigeuer\", :y 27/7672286720})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"eigeuer\" \"eigener\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"63964078-dbe0-4a14-8839-47668ef3fa31\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"63964078-dbe0-4a14-8839-47668ef3fa31\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"63964078-dbe0-4a14-8839-47668ef3fa31\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"63964078-dbe0-4a14-8839-47668ef3fa31\", :values ({:x \"eigener\", :y 19053/7672286720} {:x \"eigeuer\", :y 27/7672286720})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Gebranch&quot;</span>","value":"\"Gebranch\""},{"type":"html","content":"<span class='clj-string'>&quot;Gebrauch&quot;</span>","value":"\"Gebrauch\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"add88147-886a-4fed-98ff-1e41f29bd678","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"add88147-886a-4fed-98ff-1e41f29bd678","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"add88147-886a-4fed-98ff-1e41f29bd678"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"add88147-886a-4fed-98ff-1e41f29bd678","values":[{"x":"Gebrauch","y":1.419237887918766E-5},{"x":"Gebranch","y":1.876885018186599E-8}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"add88147-886a-4fed-98ff-1e41f29bd678\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"add88147-886a-4fed-98ff-1e41f29bd678\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"add88147-886a-4fed-98ff-1e41f29bd678\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"add88147-886a-4fed-98ff-1e41f29bd678\", :values ({:x \"Gebrauch\", :y 13611/959035840} {:x \"Gebranch\", :y 9/479517920})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Gebranch\" \"Gebrauch\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"add88147-886a-4fed-98ff-1e41f29bd678\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"add88147-886a-4fed-98ff-1e41f29bd678\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"add88147-886a-4fed-98ff-1e41f29bd678\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"add88147-886a-4fed-98ff-1e41f29bd678\", :values ({:x \"Gebrauch\", :y 13611/959035840} {:x \"Gebranch\", :y 9/479517920})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Neese&quot;</span>","value":"\"Neese\""},{"type":"html","content":"<span class='clj-string'>&quot;Reese&quot;</span>","value":"\"Reese\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"5879eee8-bec7-4635-b1d4-f935b5b7ca6f","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"5879eee8-bec7-4635-b1d4-f935b5b7ca6f","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"5879eee8-bec7-4635-b1d4-f935b5b7ca6f"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"5879eee8-bec7-4635-b1d4-f935b5b7ca6f","values":[{"x":"Reese","y":5.865265681833121E-9},{"x":"Neese","y":3.519159409099873E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"5879eee8-bec7-4635-b1d4-f935b5b7ca6f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"5879eee8-bec7-4635-b1d4-f935b5b7ca6f\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"5879eee8-bec7-4635-b1d4-f935b5b7ca6f\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"5879eee8-bec7-4635-b1d4-f935b5b7ca6f\", :values ({:x \"Reese\", :y 9/1534457344} {:x \"Neese\", :y 27/7672286720})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Neese\" \"Reese\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"5879eee8-bec7-4635-b1d4-f935b5b7ca6f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"5879eee8-bec7-4635-b1d4-f935b5b7ca6f\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"5879eee8-bec7-4635-b1d4-f935b5b7ca6f\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"5879eee8-bec7-4635-b1d4-f935b5b7ca6f\", :values ({:x \"Reese\", :y 9/1534457344} {:x \"Neese\", :y 27/7672286720})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;verliche&quot;</span>","value":"\"verliche\""},{"type":"html","content":"<span class='clj-string'>&quot;verliehe&quot;</span>","value":"\"verliehe\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b","values":[{"x":"verliehe","y":1.251256678791066E-8},{"x":"verliche","y":4.692212545466497E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b\", :values ({:x \"verliehe\", :y 3/239758960} {:x \"verliche\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"verliche\" \"verliehe\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b\", :values ({:x \"verliehe\", :y 3/239758960} {:x \"verliche\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Neichen&quot;</span>","value":"\"Neichen\""},{"type":"html","content":"<span class='clj-string'>&quot;Reichen&quot;</span>","value":"\"Reichen\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c","values":[{"x":"Reichen","y":4.852041035296449E-7},{"x":"Neichen","y":5.278739113649809E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c\", :values ({:x \"Reichen\", :y 29781/61378293760} {:x \"Neichen\", :y 81/15344573440})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Neichen\" \"Reichen\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c\", :values ({:x \"Reichen\", :y 29781/61378293760} {:x \"Neichen\", :y 81/15344573440})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;indiens&quot;</span>","value":"\"indiens\""},{"type":"html","content":"<span class='clj-string'>&quot;indicus&quot;</span>","value":"\"indicus\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"3d4754b2-32c2-400a-9cb3-52df92162e11","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"3d4754b2-32c2-400a-9cb3-52df92162e11","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"3d4754b2-32c2-400a-9cb3-52df92162e11"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"3d4754b2-32c2-400a-9cb3-52df92162e11","values":[{"x":"indicus","y":1.042713898992555E-8},{"x":"indiens","y":4.692212545466497E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"3d4754b2-32c2-400a-9cb3-52df92162e11\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"3d4754b2-32c2-400a-9cb3-52df92162e11\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"3d4754b2-32c2-400a-9cb3-52df92162e11\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"3d4754b2-32c2-400a-9cb3-52df92162e11\", :values ({:x \"indicus\", :y 1/95903584} {:x \"indiens\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"indiens\" \"indicus\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"3d4754b2-32c2-400a-9cb3-52df92162e11\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"3d4754b2-32c2-400a-9cb3-52df92162e11\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"3d4754b2-32c2-400a-9cb3-52df92162e11\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"3d4754b2-32c2-400a-9cb3-52df92162e11\", :values ({:x \"indicus\", :y 1/95903584} {:x \"indiens\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Braud&quot;</span>","value":"\"Braud\""},{"type":"html","content":"<span class='clj-string'>&quot;Brand&quot;</span>","value":"\"Brand\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"51f76982-b639-411a-9c38-21ba6af86fdd","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"51f76982-b639-411a-9c38-21ba6af86fdd","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"51f76982-b639-411a-9c38-21ba6af86fdd"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"51f76982-b639-411a-9c38-21ba6af86fdd","values":[{"x":"Brand","y":4.713066823446348E-6},{"x":"Braud","y":4.170855595970219E-8}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"51f76982-b639-411a-9c38-21ba6af86fdd\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"51f76982-b639-411a-9c38-21ba6af86fdd\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"51f76982-b639-411a-9c38-21ba6af86fdd\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"51f76982-b639-411a-9c38-21ba6af86fdd\", :values ({:x \"Brand\", :y 113/23975896} {:x \"Braud\", :y 1/23975896})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Braud\" \"Brand\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"51f76982-b639-411a-9c38-21ba6af86fdd\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"51f76982-b639-411a-9c38-21ba6af86fdd\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"51f76982-b639-411a-9c38-21ba6af86fdd\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"51f76982-b639-411a-9c38-21ba6af86fdd\", :values ({:x \"Brand\", :y 113/23975896} {:x \"Braud\", :y 1/23975896})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;junc&quot;</span>","value":"\"junc\""},{"type":"html","content":"<span class='clj-string'>&quot;jnne&quot;</span>","value":"\"jnne\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6","values":[{"x":"jnne","y":8.863068141436716E-9},{"x":"junc","y":4.692212545466497E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6\", :values ({:x \"jnne\", :y 17/1918071680} {:x \"junc\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"junc\" \"jnne\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6\", :values ({:x \"jnne\", :y 17/1918071680} {:x \"junc\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Minuc&quot;</span>","value":"\"Minuc\""},{"type":"html","content":"<span class='clj-string'>&quot;Minne&quot;</span>","value":"\"Minne\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"cb444da5-8731-4789-9416-75c85967a421","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"cb444da5-8731-4789-9416-75c85967a421","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"cb444da5-8731-4789-9416-75c85967a421"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"cb444da5-8731-4789-9416-75c85967a421","values":[{"x":"Minne","y":6.621233258602723E-8},{"x":"Minuc","y":2.346106272733248E-8}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"cb444da5-8731-4789-9416-75c85967a421\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"cb444da5-8731-4789-9416-75c85967a421\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"cb444da5-8731-4789-9416-75c85967a421\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"cb444da5-8731-4789-9416-75c85967a421\", :values ({:x \"Minne\", :y 127/1918071680} {:x \"Minuc\", :y 9/383614336})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Minuc\" \"Minne\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"cb444da5-8731-4789-9416-75c85967a421\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"cb444da5-8731-4789-9416-75c85967a421\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"cb444da5-8731-4789-9416-75c85967a421\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"cb444da5-8731-4789-9416-75c85967a421\", :values ({:x \"Minne\", :y 127/1918071680} {:x \"Minuc\", :y 9/383614336})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Heuker&quot;</span>","value":"\"Heuker\""},{"type":"html","content":"<span class='clj-string'>&quot;Henker&quot;</span>","value":"\"Henker\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"a2589b09-de17-4b67-8b4a-3bd2307fa024","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"a2589b09-de17-4b67-8b4a-3bd2307fa024","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"a2589b09-de17-4b67-8b4a-3bd2307fa024"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"a2589b09-de17-4b67-8b4a-3bd2307fa024","values":[{"x":"Henker","y":7.491899364261507E-7},{"x":"Heuker","y":4.692212545466497E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a2589b09-de17-4b67-8b4a-3bd2307fa024\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a2589b09-de17-4b67-8b4a-3bd2307fa024\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"a2589b09-de17-4b67-8b4a-3bd2307fa024\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"a2589b09-de17-4b67-8b4a-3bd2307fa024\", :values ({:x \"Henker\", :y 1437/1918071680} {:x \"Heuker\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Heuker\" \"Henker\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a2589b09-de17-4b67-8b4a-3bd2307fa024\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a2589b09-de17-4b67-8b4a-3bd2307fa024\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"a2589b09-de17-4b67-8b4a-3bd2307fa024\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"a2589b09-de17-4b67-8b4a-3bd2307fa024\", :values ({:x \"Henker\", :y 1437/1918071680} {:x \"Heuker\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;cc&quot;</span>","value":"\"cc\""},{"type":"html","content":"<span class='clj-string'>&quot;ce&quot;</span>","value":"\"ce\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"8a206188-ddf5-4fb2-a97e-380ad948d78c","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"8a206188-ddf5-4fb2-a97e-380ad948d78c","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"8a206188-ddf5-4fb2-a97e-380ad948d78c"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"8a206188-ddf5-4fb2-a97e-380ad948d78c","values":[{"x":"ce","y":1.816407612045031E-6},{"x":"cc","y":5.69321788849935E-7},{"x":"ee","y":3.406198736709013E-8},{"x":"ec","y":4.170855595970219E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"8a206188-ddf5-4fb2-a97e-380ad948d78c\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"8a206188-ddf5-4fb2-a97e-380ad948d78c\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"8a206188-ddf5-4fb2-a97e-380ad948d78c\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"8a206188-ddf5-4fb2-a97e-380ad948d78c\", :values ({:x \"ce\", :y 871/479517920} {:x \"cc\", :y 39/68502560} {:x \"ee\", :y 1/29358240} {:x \"ec\", :y 1/239758960})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"cc\" \"ce\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"8a206188-ddf5-4fb2-a97e-380ad948d78c\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"8a206188-ddf5-4fb2-a97e-380ad948d78c\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"8a206188-ddf5-4fb2-a97e-380ad948d78c\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"8a206188-ddf5-4fb2-a97e-380ad948d78c\", :values ({:x \"ce\", :y 871/479517920} {:x \"cc\", :y 39/68502560} {:x \"ee\", :y 1/29358240} {:x \"ec\", :y 1/239758960})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;coufundiren&quot;</span>","value":"\"coufundiren\""},{"type":"html","content":"<span class='clj-string'>&quot;confundiren&quot;</span>","value":"\"confundiren\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"6a139aef-1e74-43d1-8ab0-a2296fb87b12","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"6a139aef-1e74-43d1-8ab0-a2296fb87b12","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"6a139aef-1e74-43d1-8ab0-a2296fb87b12"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"6a139aef-1e74-43d1-8ab0-a2296fb87b12","values":[{"x":"confundiren","y":1.583621734094943E-8},{"x":"coufundiren","y":1.979527167618678E-9},{"x":"confundireu","y":2.19947463068742E-10}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"6a139aef-1e74-43d1-8ab0-a2296fb87b12\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"6a139aef-1e74-43d1-8ab0-a2296fb87b12\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"6a139aef-1e74-43d1-8ab0-a2296fb87b12\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"6a139aef-1e74-43d1-8ab0-a2296fb87b12\", :values ({:x \"confundiren\", :y 243/15344573440} {:x \"coufundiren\", :y 243/122756587520} {:x \"confundireu\", :y 27/122756587520})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"coufundiren\" \"confundiren\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"6a139aef-1e74-43d1-8ab0-a2296fb87b12\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"6a139aef-1e74-43d1-8ab0-a2296fb87b12\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"6a139aef-1e74-43d1-8ab0-a2296fb87b12\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"6a139aef-1e74-43d1-8ab0-a2296fb87b12\", :values ({:x \"confundiren\", :y 243/15344573440} {:x \"coufundiren\", :y 243/122756587520} {:x \"confundireu\", :y 27/122756587520})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-string'>&quot;Nenen&quot;</span>","value":"\"Nenen\""},{"type":"html","content":"<span class='clj-string'>&quot;Neuen&quot;</span>","value":"\"Neuen\""},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"0174596c-e15d-4c53-bd0a-89903e75714d","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"0174596c-e15d-4c53-bd0a-89903e75714d","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"0174596c-e15d-4c53-bd0a-89903e75714d"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"0174596c-e15d-4c53-bd0a-89903e75714d","values":[{"x":"Neuen","y":7.425426353200731E-7},{"x":"Nenen","y":2.639369556824904E-9},{"x":"Reuen","y":1.319684778412452E-9},{"x":"Neueu","y":5.865265681833121E-10},{"x":"Renen","y":4.398949261374841E-10}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0174596c-e15d-4c53-bd0a-89903e75714d\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0174596c-e15d-4c53-bd0a-89903e75714d\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"0174596c-e15d-4c53-bd0a-89903e75714d\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"0174596c-e15d-4c53-bd0a-89903e75714d\", :values ({:x \"Neuen\", :y 5697/7672286720} {:x \"Nenen\", :y 81/30689146880} {:x \"Reuen\", :y 81/61378293760} {:x \"Neueu\", :y 9/15344573440} {:x \"Renen\", :y 27/61378293760})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[\"Nenen\" \"Neuen\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0174596c-e15d-4c53-bd0a-89903e75714d\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0174596c-e15d-4c53-bd0a-89903e75714d\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"0174596c-e15d-4c53-bd0a-89903e75714d\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"0174596c-e15d-4c53-bd0a-89903e75714d\", :values ({:x \"Neuen\", :y 5697/7672286720} {:x \"Nenen\", :y 81/30689146880} {:x \"Reuen\", :y 81/61378293760} {:x \"Neueu\", :y 9/15344573440} {:x \"Renen\", :y 27/61378293760})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"}],"value":"#gorilla_repl.table.TableView{:contents ([\"merek\" \"merck\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"20c615e9-8682-4204-b849-61fd0a36b19a\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"20c615e9-8682-4204-b849-61fd0a36b19a\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"20c615e9-8682-4204-b849-61fd0a36b19a\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"20c615e9-8682-4204-b849-61fd0a36b19a\", :values ({:x \"merck\", :y 17/137005120} {:x \"merek\", :y 3/548020480})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"kounten\" \"konnten\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"f99d7767-5329-484c-95c7-90b2c510201d\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"f99d7767-5329-484c-95c7-90b2c510201d\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"f99d7767-5329-484c-95c7-90b2c510201d\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"f99d7767-5329-484c-95c7-90b2c510201d\", :values ({:x \"konnten\", :y 74097/7672286720} {:x \"kounten\", :y 27/7672286720} {:x \"konuten\", :y 3/3836143360})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Busbeqn\" \"Busbequ\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"f859d4c0-cccf-47ed-ac7e-40f3e10752d7\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"f859d4c0-cccf-47ed-ac7e-40f3e10752d7\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"f859d4c0-cccf-47ed-ac7e-40f3e10752d7\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"f859d4c0-cccf-47ed-ac7e-40f3e10752d7\", :values ({:x \"Busbequ\", :y 3/479517920} {:x \"Busbeqn\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"ehc\" \"ehe\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c2257117-ce0b-4725-b093-fac68214e072\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c2257117-ce0b-4725-b093-fac68214e072\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"c2257117-ce0b-4725-b093-fac68214e072\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"c2257117-ce0b-4725-b093-fac68214e072\", :values ({:x \"ehe\", :y 9321/479517920} {:x \"che\", :y 3449/1438553760} {:x \"ehc\", :y 3/479517920})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"codem\" \"eodem\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"3e4f4826-f6f8-4ec2-a82f-3245e5db4bca\", :values ({:x \"eodem\", :y 1/2140705} {:x \"codem\", :y 3/68502560})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Rheingan\" \"Rheingau\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"666d7580-6aab-437e-b97e-76acd974305a\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"666d7580-6aab-437e-b97e-76acd974305a\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"666d7580-6aab-437e-b97e-76acd974305a\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"666d7580-6aab-437e-b97e-76acd974305a\", :values ({:x \"Rheingau\", :y 111/959035840} {:x \"Rheingan\", :y 9/959035840})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"vergenden\" \"vergeuden\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"9e90f240-a681-4bec-9b52-c521c9c1c744\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"9e90f240-a681-4bec-9b52-c521c9c1c744\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"9e90f240-a681-4bec-9b52-c521c9c1c744\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"9e90f240-a681-4bec-9b52-c521c9c1c744\", :values ({:x \"vergeuden\", :y 81/2192081920} {:x \"vergenden\", :y 81/30689146880})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"eigeuer\" \"eigener\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"63964078-dbe0-4a14-8839-47668ef3fa31\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"63964078-dbe0-4a14-8839-47668ef3fa31\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"63964078-dbe0-4a14-8839-47668ef3fa31\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"63964078-dbe0-4a14-8839-47668ef3fa31\", :values ({:x \"eigener\", :y 19053/7672286720} {:x \"eigeuer\", :y 27/7672286720})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Gebranch\" \"Gebrauch\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"add88147-886a-4fed-98ff-1e41f29bd678\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"add88147-886a-4fed-98ff-1e41f29bd678\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"add88147-886a-4fed-98ff-1e41f29bd678\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"add88147-886a-4fed-98ff-1e41f29bd678\", :values ({:x \"Gebrauch\", :y 13611/959035840} {:x \"Gebranch\", :y 9/479517920})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Neese\" \"Reese\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"5879eee8-bec7-4635-b1d4-f935b5b7ca6f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"5879eee8-bec7-4635-b1d4-f935b5b7ca6f\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"5879eee8-bec7-4635-b1d4-f935b5b7ca6f\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"5879eee8-bec7-4635-b1d4-f935b5b7ca6f\", :values ({:x \"Reese\", :y 9/1534457344} {:x \"Neese\", :y 27/7672286720})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"verliche\" \"verliehe\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"911bbe12-8a9c-43d1-b81d-a8e29ea6bf2b\", :values ({:x \"verliehe\", :y 3/239758960} {:x \"verliche\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Neichen\" \"Reichen\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"5d4d47b1-3f6e-4308-bfd8-3d11e6a9a69c\", :values ({:x \"Reichen\", :y 29781/61378293760} {:x \"Neichen\", :y 81/15344573440})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"indiens\" \"indicus\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"3d4754b2-32c2-400a-9cb3-52df92162e11\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"3d4754b2-32c2-400a-9cb3-52df92162e11\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"3d4754b2-32c2-400a-9cb3-52df92162e11\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"3d4754b2-32c2-400a-9cb3-52df92162e11\", :values ({:x \"indicus\", :y 1/95903584} {:x \"indiens\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Braud\" \"Brand\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"51f76982-b639-411a-9c38-21ba6af86fdd\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"51f76982-b639-411a-9c38-21ba6af86fdd\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"51f76982-b639-411a-9c38-21ba6af86fdd\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"51f76982-b639-411a-9c38-21ba6af86fdd\", :values ({:x \"Brand\", :y 113/23975896} {:x \"Braud\", :y 1/23975896})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"junc\" \"jnne\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"bd52166d-6ab5-4173-85a6-8b5dd5c6eca6\", :values ({:x \"jnne\", :y 17/1918071680} {:x \"junc\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Minuc\" \"Minne\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"cb444da5-8731-4789-9416-75c85967a421\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"cb444da5-8731-4789-9416-75c85967a421\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"cb444da5-8731-4789-9416-75c85967a421\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"cb444da5-8731-4789-9416-75c85967a421\", :values ({:x \"Minne\", :y 127/1918071680} {:x \"Minuc\", :y 9/383614336})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Heuker\" \"Henker\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a2589b09-de17-4b67-8b4a-3bd2307fa024\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a2589b09-de17-4b67-8b4a-3bd2307fa024\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"a2589b09-de17-4b67-8b4a-3bd2307fa024\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"a2589b09-de17-4b67-8b4a-3bd2307fa024\", :values ({:x \"Henker\", :y 1437/1918071680} {:x \"Heuker\", :y 9/1918071680})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"cc\" \"ce\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"8a206188-ddf5-4fb2-a97e-380ad948d78c\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"8a206188-ddf5-4fb2-a97e-380ad948d78c\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"8a206188-ddf5-4fb2-a97e-380ad948d78c\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"8a206188-ddf5-4fb2-a97e-380ad948d78c\", :values ({:x \"ce\", :y 871/479517920} {:x \"cc\", :y 39/68502560} {:x \"ee\", :y 1/29358240} {:x \"ec\", :y 1/239758960})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"coufundiren\" \"confundiren\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"6a139aef-1e74-43d1-8ab0-a2296fb87b12\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"6a139aef-1e74-43d1-8ab0-a2296fb87b12\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"6a139aef-1e74-43d1-8ab0-a2296fb87b12\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"6a139aef-1e74-43d1-8ab0-a2296fb87b12\", :values ({:x \"confundiren\", :y 243/15344573440} {:x \"coufundiren\", :y 243/122756587520} {:x \"confundireu\", :y 27/122756587520})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}] [\"Nenen\" \"Neuen\" #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0174596c-e15d-4c53-bd0a-89903e75714d\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0174596c-e15d-4c53-bd0a-89903e75714d\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"0174596c-e15d-4c53-bd0a-89903e75714d\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"0174596c-e15d-4c53-bd0a-89903e75714d\", :values ({:x \"Neuen\", :y 5697/7672286720} {:x \"Nenen\", :y 81/30689146880} {:x \"Reuen\", :y 81/61378293760} {:x \"Neueu\", :y 9/15344573440} {:x \"Renen\", :y 27/61378293760})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]), :opts nil}"}
;;   ;; <=

;;   ;; @@
;;   (let [r (take 10 (transform dta "bcnachrichten"))]
;;     [(plot/bar-chart
;;       (map :word r)
;;       (map :prob r)
;;       :plot-size 600)
;;      (plot/bar-chart
;;       (map :word r)
;;       (map :word-prob r)
;;       :plot-size 600)
;;      (plot/bar-chart
;;       (map :word r)
;;       (map :subst-prob r)
;;       :plot-size 600)])
;;   ;; @@
;;   ;; =>
;; ;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"7ec74d0a-eea7-4818-8b51-2990a92e5a5b","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"7ec74d0a-eea7-4818-8b51-2990a92e5a5b","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"7ec74d0a-eea7-4818-8b51-2990a92e5a5b"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"7ec74d0a-eea7-4818-8b51-2990a92e5a5b","values":[{"x":"benachrichten","y":1.319684778412452E-9}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"7ec74d0a-eea7-4818-8b51-2990a92e5a5b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"7ec74d0a-eea7-4818-8b51-2990a92e5a5b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"7ec74d0a-eea7-4818-8b51-2990a92e5a5b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"7ec74d0a-eea7-4818-8b51-2990a92e5a5b\", :values ({:x \"benachrichten\", :y 81/61378293760})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"91bc2cd5-893f-414b-9496-5a966e51e631","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"91bc2cd5-893f-414b-9496-5a966e51e631","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"91bc2cd5-893f-414b-9496-5a966e51e631"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"91bc2cd5-893f-414b-9496-5a966e51e631","values":[{"x":"benachrichten","y":2.224456317850784E-8}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"91bc2cd5-893f-414b-9496-5a966e51e631\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"91bc2cd5-893f-414b-9496-5a966e51e631\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"91bc2cd5-893f-414b-9496-5a966e51e631\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"91bc2cd5-893f-414b-9496-5a966e51e631\", :values ({:x \"benachrichten\", :y 1/44954805})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"},{"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"dc41de6c-cef9-418e-9e1e-b9e42501ef2a","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"dc41de6c-cef9-418e-9e1e-b9e42501ef2a","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"dc41de6c-cef9-418e-9e1e-b9e42501ef2a"},"properties":{"enter":{"width":{"offset":-1,"band":true,"scale":"x"},"y":{"field":"data.y","scale":"y"},"x":{"field":"data.x","scale":"x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"dc41de6c-cef9-418e-9e1e-b9e42501ef2a","values":[{"x":"benachrichten","y":0.059326171875}]}],"width":600,"height":370.82818603515625,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"dc41de6c-cef9-418e-9e1e-b9e42501ef2a\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"dc41de6c-cef9-418e-9e1e-b9e42501ef2a\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"dc41de6c-cef9-418e-9e1e-b9e42501ef2a\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"dc41de6c-cef9-418e-9e1e-b9e42501ef2a\", :values ({:x \"benachrichten\", :y 243/4096})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"7ec74d0a-eea7-4818-8b51-2990a92e5a5b\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"7ec74d0a-eea7-4818-8b51-2990a92e5a5b\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"7ec74d0a-eea7-4818-8b51-2990a92e5a5b\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"7ec74d0a-eea7-4818-8b51-2990a92e5a5b\", :values ({:x \"benachrichten\", :y 81/61378293760})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}} #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"91bc2cd5-893f-414b-9496-5a966e51e631\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"91bc2cd5-893f-414b-9496-5a966e51e631\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"91bc2cd5-893f-414b-9496-5a966e51e631\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"91bc2cd5-893f-414b-9496-5a966e51e631\", :values ({:x \"benachrichten\", :y 1/44954805})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}} #gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"dc41de6c-cef9-418e-9e1e-b9e42501ef2a\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"dc41de6c-cef9-418e-9e1e-b9e42501ef2a\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"dc41de6c-cef9-418e-9e1e-b9e42501ef2a\"}, :properties {:enter {:width {:offset -1, :band true, :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :x {:field \"data.x\", :scale \"x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"dc41de6c-cef9-418e-9e1e-b9e42501ef2a\", :values ({:x \"benachrichten\", :y 243/4096})}], :width 600, :height 370.8282, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"}
;;   ;; <=

;;   ;; @@
;;   ()
;;   ;; @@
;;   ;; =>
;; ;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"}
;;   ;; <=

;;   ;; @@

;;   ;; @@

;;   ;; @@

;;   ;; @@

;;   ;; @@

;;   ;; @@

;;   ;; @@

;;   ;; @@

;;   ;; @@

;;   ;; @@
;;   )
=======

;; @@
(use 'marmoset.util)
;; @@

;; @@
(future (doseq [i (range 10000)]
          (swap! a inc)
          (Thread/sleep 1)))
;; @@

;; @@
(def f (future
  (time (dorun
  (pmap #(do
           (swap! a inc)
           (spit (str "/Users/ticking/Desktop/test" (first %) ".xml")
               (with-out-str (xml/emit (correct (xml/parse (second %)) dta)))))
        files)))))
;; @@

;; @@
(def a (atom 0))
;; @@

;; @@
(reset! a 0)
;; @@

;; @@
(progress-view a (count (files "/Users/ticking/Desktop/vls-ro")))
;; @@

;; @@
(def di (->> "/Users/ticking/Desktop/vls-ro/"
             abbyy/files
             (pmap #(->> %
                         (do (swap! a inc))
                         second
                         xml/parse
                         abbyy/lines
                         abbyy/remove-linewrap
                         (mapv abbyy/text)))
             (apply concat)
             frequencies
             future))
;; @@

;; @@
(def bigrams (->> "/Users/ticking/Desktop/vls-ro/"
             abbyy/files
             (pmap #(->> %
                         (do (swap! a inc))
                         second
                         xml/parse
                         abbyy/lines
                         abbyy/remove-linewrap
                         (mapv abbyy/text)))
             (apply concat)
             (partition 2 1)
             frequencies
             future))
;; @@

;; @@
(apply max-key second @di)
;; @@

;; @@
(apply max-key second @bigrams)
;; @@
>>>>>>> dev
