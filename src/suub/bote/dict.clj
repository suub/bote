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
            [error-codes.gorilla :as gv]
            [clojure.core.reducers :as r]
            [clojure.test :as t]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [suub.bote.clojure.xml :as xml]
            [clojure.string :as string]
            [clojure.data.priority-map :as pm]
            [clojure.edn :as edn]
            [me.raynes.fs :as fs]
            [suub.bote.abbyy :as abbyy]))
;; @@
;; ->
;;; &quot;fs &quot; [[[1 1]]]
;;; &quot;fi &quot; ()
;;; &quot;ext &quot; ()
;;; &quot;extr &quot; () &quot;res&quot; ()
;;; &quot;fs &quot; [[[1 1]]]
;;; &quot;fi &quot; ()
;;; &quot;ext &quot; ()
;;; &quot;extr &quot; () &quot;res&quot; ()
;;; 
;; <-
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
(defn drop-prefix
  "Returns the rest of the seq after the prefix elements have been removed.
   When no match can be found returns nil."
  [prefix coll]
  (loop [pre (seq prefix),
         post (seq coll)]
    (cond (empty? pre) post
          (= (first pre) (first post)) (recur (rest pre) (rest post)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/drop-prefix</span>","value":"#'suub.bote.dict/drop-prefix"}
;; <=

;; @@
(defn simple-matcher [p q]
  (when-let [r (drop-prefix p q)]
    [p r]))
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
(def dta-dict (read-dict "resources/dta-freq.d/dta-core.fuw"))
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
                "Dic"
                "gebranch"
                "Irrcreden"
                "Tanfe"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta-dict</span>","value":"#'suub.bote.dict/dta-dict"}
;; <=

;; @@
(def gold-subst (read-substs "resources/substitutions.edn"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/gold-subst</span>","value":"#'suub.bote.dict/gold-subst"}
;; <=

;; @@
(def pots-dict (->> "resources/dict.edn"
                    slurp
                    edn/read-string
                    (map #(update-in % [1] bigint))
                    (into {})))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/pots-dict</span>","value":"#'suub.bote.dict/pots-dict"}
;; <=

;; @@
(def simple-subst
        {["u" "n"] 1/4
         ["n" "u"] 1/4
         ["c" "e"] 1/4
         ["e" "c"] 1/4
         ["N" "U"] 1/8
         ["N" "R"] 1/8
         ["ö" "ß"] 1/4
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
         ["s" "s"] 3/4
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
         ["ö" "ö"] 3/4
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
         ["m" "en"] 1/8
         ["s" "f"] 1/4})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/simple-subst</span>","value":"#'suub.bote.dict/simple-subst"}
;; <=

;; @@
(def dta-prfx (time (prefixes dta-dict)))
;; @@
;; ->
;;; &quot;Elapsed time: 69512.567 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta-prfx</span>","value":"#'suub.bote.dict/dta-prfx"}
;; <=

;; @@
(def pots-prfx (time (prefixes pots-dict)))
;; @@
;; ->
;;; &quot;Elapsed time: 1953.965 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/pots-prfx</span>","value":"#'suub.bote.dict/pots-prfx"}
;; <=

;; @@
(def dta  {:matcher abbyy/matcher
           :dict dta-dict
           :prefixes dta-prfx
           :substs simple-subst})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/dta</span>","value":"#'suub.bote.dict/dta"}
;; <=

;; @@
(def pots {:matcher abbyy/matcher
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
(def pages (->> "resources/texts"
                io/file
                file-seq
                (filter #(and
                           (fs/file? %)
                           (= ".xml" (fs/extension %))))))
(def p (xml/parse (nth pages 8)))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/p</span>","value":"#'suub.bote.dict/p"}
;; <=

;; @@
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/correct</span>","value":"#'suub.bote.dict/correct"}
;; <=

;; @@
(defn text [p]
  (apply str (map (fn [l]
                    (str (apply str
                                (map :char l))
                         "\n"))
                  (abbyy/lines p))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/text</span>","value":"#'suub.bote.dict/text"}
;; <=

;; @@
nil
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(println (time (text p)))
;; @@
;; ->
;;; &quot;Elapsed time: 156.087 msecs&quot;
;;; Politische Rückblicke.
;;; Deutsche Vvr. und Rückschritte im Jahre 1S4Z.
;;; Zwei ganz entgegengesetzte Negierungssysteme streiten sich in Eu¬
;;; ropa um die Physische und geistige Hegemonie. Während man im
;;; Westen freiwillig, oder durch die Umstände gezwungen, entschieden
;;; dem öffentlichen Staatöleben huldigt, glaubt man im entfernten Osten
;;; durch eine geheime Negierungsform die größte Macht erzeugen
;;; und zusammenhalten zu können. Beide Systeme stehen sich feindlich
;;; seit langer Zeit gegenüber und es liegt in der Natur der Sache, daß
;;; sie mit der Zeit, wenn eine naturgemäße Ausgleichung nicht eintritt,
;;; in Conflict gerathen -müssen. Mitten in diesem europäischen Dualis¬
;;; mus steht nun Deutschland, das in dem letzten Vierteljahrhundcrt
;;; seine innere Organisation auffallend vernachlässigt hat, noch immer
;;; schwankend, ob es dem westlichen oder dem östlichen Negierungs-
;;; system folgen soll. Das Volk, d. h. die denkende Masse, neigt sich
;;; unverkennbar zum erstem, während die Beamten der Negierungen,
;;; jeder öffentlichen und volksthümlichen Controle abhold, lieber dem
;;; letzteren folgen möchten. Es war für Deutschland ein offenkundiges
;;; Unglück, daß Napoleon nach den Bedingungen des Congresses von
;;; Chatillon nicht Frankreich als Königreich nach den alten Grenzen an¬
;;; genommen hat. Wie schnell hätte sich dann nicht daS träumende
;;; und philosophircndc Deutschland im Angesicht des großen Feldherrn,
;;; dessen harte Mißhandlimgen es noch im frischen Andenken hatte, neu
;;; und auf eine volkstümliche Weise organisirt! Unsere jetzige Lage
;;; ist aber noch schlimmer als damals. Wir stehen nicht mehr einem
;;; geschlagenen Soldaten-Kaiser, dessen glänzender Stern auf den Fel¬
;;; dern von Leipzig und Wäterloo verblichen ist, sondern dem ga&#x27;-zcir
;;; Westen gegenüber, der im Besitz eines öffentlichen Staatsleben^ und
;;; einer nationalen Repräsentation eine unberechenbare Kraft zu ent-
;;; ÄrcuMen &gt;- .4
;;; 
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(println (time (text (correct p dta))))
;; @@
;; ->
;;; &quot;Elapsed time: 2244.686 msecs&quot;
;;; Politische Rückblicke.
;;; Deutsche Vvr. und Rückschritte im Jahre 1S4Z.
;;; Zwei ganz entgegengesetzte Regierungssysteme streiten sich in Eu¬
;;; ropa um die Physische und geistige Hegemonie. Während man im
;;; Westen freiwillig, oder durch die Umstände gezwungen, entschieden
;;; dem öffentlichen Staatöleben huldigt, glaubt man im entfernten Osten
;;; durch eine geheime Regierungsform die größte Macht erzeugen
;;; und zusammenhalten zu können. Beide Systeme stehen sich feindlich
;;; seit langer Zeit gegenüber und es liegt in der Natur der Sache, daß
;;; sie mit der Zeit, wenn eine naturgemäße Ausgleichung nicht eintritt,
;;; in Conflict gerathen -müssen. Mitten in diesem europäischen Dualis¬
;;; mus steht nun Deutschland, das in dem letzten Vierteljahrhundert
;;; seine innere Organisation auffallend vernachlässigt hat, noch immer
;;; schwankend, ob es dem westlichen oder dem östlichen Regierungs-
;;; system folgen soll. Das Volk, d. h. die denkende Masse, neigt sich
;;; unverkennbar zum erstem, während die Beamten der Regierungen,
;;; jeder öffentlichen und volksthümlichen Controle abhold, lieber dem
;;; letzteren folgen möchten. Es war für Deutschland ein offenkundiges
;;; Unglück, daß Napoleon nach den Bedingungen des Congresses von
;;; Chatillon nicht Frankreich als Königreich nach den alten Grenzen an¬
;;; genommen hat. Wie schnell hätte sich dann nicht daS träumende
;;; und philosophirende Deutschland im Angesicht des großen Feldherrn,
;;; dessen harte Mißhandlimgen es noch im frischen Andenken hatte, neu
;;; und auf eine volkstümliche Weise organisirt! Unsere jetzige Lage
;;; ist aber noch schlimmer als damals. Wir stehen nicht mehr einem
;;; geschlagenen Soldaten-Kaiser, dessen glänzender Stern auf den Fel¬
;;; dern von Leipzig und Wäterloo verblichen ist, sondern dem ga&#x27;-zcir
;;; Westen gegenüber, der im Besitz eines öffentlichen Staatsleben^ und
;;; einer nationalen Repräsentation eine unberechenbare Kraft zu ent-
;;; ÄrcuMen &gt;- .4
;;; 
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(type (xml/emit (correct p dta)))
;; @@
;; ->
;;; &lt;?xml version=&#x27;1.0&#x27; encoding=&#x27;UTF-8&#x27;?&gt;
;;; &lt;document version=&#x27;1.0&#x27; producer=&#x27;FineReader 8.0&#x27; xmlns=&#x27;http://www.abbyy.com/FineReader_xml/FineReader8-schema-v2.xml&#x27; xmlns:xsi=&#x27;http://www.w3.org/2001/XMLSchema-instance&#x27; xsi:schemaLocation=&#x27;http://www.abbyy.com/FineReader_xml/FineReader8-schema-v2.xml http://www.abbyy.com/FineReader_xml/FineReader8-schema-v2.xml&#x27; mainLanguage=&#x27;OldGerman&#x27; languages=&#x27;OldGerman&#x27;&gt;
;;; 
;;; 
;;; &lt;page width=&#x27;1923&#x27; height=&#x27;2817&#x27; resolution=&#x27;300&#x27; isSkewCorrect=&#x27;false&#x27; skewAngle=&#x27;-0.0144910647203975&#x27;&gt;
;;; 
;;; 
;;; &lt;block blockType=&#x27;Text&#x27; blockName=&#x27;&#x27; isHidden=&#x27;true&#x27; l=&#x27;408&#x27; t=&#x27;661&#x27; r=&#x27;1331&#x27; b=&#x27;811&#x27;&gt;
;;; &lt;region&gt;
;;; &lt;rect l=&#x27;408&#x27; t=&#x27;661&#x27; r=&#x27;1331&#x27; b=&#x27;811&#x27;/&gt;
;;; &lt;/region&gt;
;;; 
;;; 
;;; &lt;text&gt;
;;; 
;;; 
;;; &lt;par align=&#x27;Center&#x27; rightIndent=&#x27;2&#x27;&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;716&#x27; l=&#x27;582&#x27; t=&#x27;667&#x27; r=&#x27;1155&#x27; b=&#x27;721&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;14.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;70&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;632&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;582&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;717&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;667&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; P
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;666&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;636&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;710&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;679&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;685&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;668&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;710&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;668&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;703&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;688&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;712&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;668&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;727&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;705&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;711&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;672&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;745&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;729&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;712&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;670&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;771&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;746&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;721&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;670&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;815&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;768&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;721&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;669&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;815&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;768&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;721&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;669&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;81&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;844&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;820&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;715&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;683&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;844&#x27; t=&#x27;667&#x27; r=&#x27;872&#x27; b=&#x27;718&#x27; characterHeight=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;76&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;925&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;872&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;717&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;672&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; R
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;957&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;927&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;717&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;673&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ü
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;69&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1003&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;961&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;718&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;673&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;69&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1003&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;961&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;718&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;673&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1034&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1005&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;717&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;674&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1055&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1038&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;717&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;674&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1073&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1058&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;717&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;674&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;67&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1119&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1076&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;717&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;673&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;67&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1119&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1076&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;717&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;673&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;80&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1144&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1121&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;717&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;686&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;97&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1155&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1147&#x27; characterHeight=&#x27;31&#x27; wordNormal=&#x27;true&#x27; b=&#x27;714&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;705&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; &lt;/par&gt;
;;; 
;;; 
;;; &lt;par align=&#x27;Justified&#x27;&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;796&#x27; l=&#x27;414&#x27; t=&#x27;764&#x27; r=&#x27;1325&#x27; b=&#x27;805&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;8.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;83&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;441&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;414&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;801&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;775&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;25&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; D
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;76&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;462&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;452&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;780&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;25&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;485&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;471&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;797&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;778&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;25&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;503&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;495&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;795&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;774&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;25&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;522&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;512&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;800&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;769&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;25&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;550&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;528&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;798&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;766&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;25&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;550&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;528&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;798&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;766&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;25&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;76&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;570&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;560&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;791&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;772&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;25&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;570&#x27; t=&#x27;764&#x27; r=&#x27;590&#x27; b=&#x27;796&#x27; characterHeight=&#x27;19&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;81&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;616&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;590&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;791&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;764&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; V
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;639&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;627&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;791&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;772&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; v
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;660&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;648&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;791&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;773&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;677&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;664&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;789&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;777&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;677&#x27; t=&#x27;764&#x27; r=&#x27;698&#x27; b=&#x27;796&#x27; characterHeight=&#x27;19&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;715&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;698&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;792&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;773&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;741&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;726&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;793&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;774&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;763&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;749&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;794&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;769&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;763&#x27; t=&#x27;769&#x27; r=&#x27;784&#x27; b=&#x27;796&#x27; characterHeight=&#x27;19&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;811&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;784&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;796&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;770&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; R
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;833&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;818&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;796&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;769&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ü
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;864&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;844&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;797&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;770&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;864&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;844&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;797&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;770&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;884&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;873&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;805&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;772&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;915&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;891&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;805&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;771&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;915&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;891&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;805&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;771&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;938&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;925&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;780&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;956&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;949&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;772&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;977&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;966&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;775&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;996&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;986&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;775&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1014&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1004&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;780&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1014&#x27; t=&#x27;769&#x27; r=&#x27;1034&#x27; b=&#x27;800&#x27; characterHeight=&#x27;19&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1042&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1034&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;772&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1076&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1051&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;800&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;779&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1076&#x27; t=&#x27;772&#x27; r=&#x27;1095&#x27; b=&#x27;800&#x27; characterHeight=&#x27;19&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;81&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1118&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1095&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;802&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;773&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; J
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;76&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1142&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1128&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;798&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;779&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1166&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1153&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;805&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;773&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;81&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1189&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1178&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;780&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1208&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1198&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;true&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;779&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1208&#x27; t=&#x27;772&#x27; r=&#x27;1229&#x27; b=&#x27;799&#x27; characterHeight=&#x27;19&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1242&#x27; wordIdentifier=&#x27;true&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1229&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;false&#x27; b=&#x27;798&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;773&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; 1
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1268&#x27; wordIdentifier=&#x27;true&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1252&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;false&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;772&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; S
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1294&#x27; wordIdentifier=&#x27;true&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1276&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;false&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;772&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; 4
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;81&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1316&#x27; wordIdentifier=&#x27;true&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1301&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;false&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;773&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; Z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1325&#x27; wordIdentifier=&#x27;true&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1319&#x27; characterHeight=&#x27;19&#x27; wordNormal=&#x27;false&#x27; b=&#x27;799&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;792&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; &lt;/par&gt;
;;; 
;;; 
;;; &lt;/text&gt;
;;; 
;;; 
;;; &lt;/block&gt;
;;; 
;;; 
;;; &lt;block blockType=&#x27;Text&#x27; blockName=&#x27;&#x27; isHidden=&#x27;true&#x27; l=&#x27;252&#x27; t=&#x27;920&#x27; r=&#x27;1494&#x27; b=&#x27;2406&#x27;&gt;
;;; &lt;region&gt;
;;; &lt;rect l=&#x27;252&#x27; t=&#x27;920&#x27; r=&#x27;1494&#x27; b=&#x27;2406&#x27;/&gt;
;;; &lt;/region&gt;
;;; 
;;; 
;;; &lt;text&gt;
;;; 
;;; 
;;; &lt;par align=&#x27;Justified&#x27; startIndent=&#x27;90&#x27; lineSpacing=&#x27;52&#x27;&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;979&#x27; l=&#x27;349&#x27; t=&#x27;940&#x27; r=&#x27;1483&#x27; b=&#x27;992&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;374&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;349&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;983&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;944&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; Z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;404&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;378&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;975&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;952&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; w
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;76&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;421&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;409&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;974&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;952&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;433&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;424&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;974&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;940&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;433&#x27; t=&#x27;940&#x27; r=&#x27;451&#x27; b=&#x27;983&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;467&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;451&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;983&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;950&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;488&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;471&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;971&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;949&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;509&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;491&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;971&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;948&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;523&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;513&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;978&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;947&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;523&#x27; t=&#x27;942&#x27; r=&#x27;547&#x27; b=&#x27;983&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;560&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;547&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;971&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;948&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;581&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;564&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;971&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;948&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;595&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;585&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;971&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;942&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;613&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;597&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;982&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;950&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;629&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;617&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;972&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;950&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;649&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;633&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;984&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;951&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;665&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;654&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;974&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;952&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;688&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;669&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;975&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;951&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;708&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;691&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;985&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;953&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;724&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;712&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;975&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;953&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;739&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;727&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;984&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;944&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;750&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;739&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;976&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;954&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;770&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;754&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;985&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;949&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;770&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;754&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;985&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;949&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;785&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;775&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;978&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;948&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;800&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;787&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;977&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;955&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;800&#x27; t=&#x27;942&#x27; r=&#x27;823&#x27; b=&#x27;985&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;854&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;823&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;981&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;945&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; R
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;871&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;858&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;980&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;957&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;891&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;874&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;991&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;957&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;903&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;894&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;980&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;945&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;918&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;906&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;981&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;957&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;933&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;921&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;980&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;957&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;956&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;936&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;981&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;956&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;976&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;958&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;981&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;957&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;998&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;981&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;991&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;958&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1017&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1001&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;980&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;951&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1035&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1022&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;989&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;947&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1048&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1033&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;988&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;954&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; y
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1067&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1052&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;988&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;946&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1073&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1061&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;980&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;950&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1088&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1075&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;980&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;957&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1117&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1091&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;980&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;956&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1135&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1122&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;980&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;957&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1135&#x27; t=&#x27;945&#x27; r=&#x27;1159&#x27; b=&#x27;988&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1175&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1159&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;988&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;946&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1181&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1170&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;980&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;950&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;87&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1196&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1183&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;980&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;957&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;70&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1211&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1199&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;980&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;958&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1223&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1214&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;981&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;946&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1235&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1226&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;981&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;951&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1250&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1238&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;981&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;958&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;85&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1273&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1253&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;982&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;957&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1273&#x27; t=&#x27;946&#x27; r=&#x27;1297&#x27; b=&#x27;988&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1317&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1297&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;989&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;947&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1317&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1297&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;989&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;947&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1347&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1319&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;992&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;947&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1347&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1319&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;992&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;947&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;1347&#x27; t=&#x27;947&#x27; r=&#x27;1372&#x27; b=&#x27;982&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1382&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1372&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;982&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;947&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1403&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1384&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;982&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;959&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1403&#x27; t=&#x27;947&#x27; r=&#x27;1418&#x27; b=&#x27;982&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1446&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1418&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;984&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;948&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; E
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1467&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1448&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;983&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;959&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;87&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1483&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1469&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;978&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;965&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ¬
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1031&#x27; l=&#x27;259&#x27; t=&#x27;989&#x27; r=&#x27;1484&#x27; b=&#x27;1045&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;273&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;259&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1032&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1010&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;291&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;277&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1032&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1008&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;312&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;294&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1039&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1005&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; p
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;334&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;316&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1030&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1006&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;334&#x27; t=&#x27;1003&#x27; r=&#x27;358&#x27; b=&#x27;1031&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;378&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;358&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1028&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1004&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;409&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;382&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1028&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1003&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;409&#x27; t=&#x27;990&#x27; r=&#x27;433&#x27; b=&#x27;1031&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;450&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;433&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1026&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;994&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;462&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;454&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1025&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;990&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;478&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;466&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1024&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1001&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;478&#x27; t=&#x27;989&#x27; r=&#x27;509&#x27; b=&#x27;1031&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;528&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;509&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1031&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;996&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; P
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;548&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;530&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;990&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;87&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;567&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;552&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;997&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; y
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;590&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;571&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1032&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;989&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;590&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;571&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1032&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;989&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;605&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;593&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1035&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;992&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;632&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;605&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1035&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;992&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;632&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;605&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1035&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;992&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;649&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;636&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1026&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1003&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;649&#x27; t=&#x27;989&#x27; r=&#x27;680&#x27; b=&#x27;1031&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;699&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;680&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1028&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1003&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;720&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;702&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1028&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1004&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;741&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;724&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1029&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;996&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;741&#x27; t=&#x27;996&#x27; r=&#x27;774&#x27; b=&#x27;1031&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;791&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;774&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1041&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1008&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;807&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;796&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1031&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1009&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;819&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;811&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1031&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;997&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;837&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;822&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1039&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;998&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;843&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;832&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1032&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1002&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;853&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;846&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;998&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;874&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;857&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1044&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1010&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;891&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;878&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1010&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;891&#x27; t=&#x27;997&#x27; r=&#x27;915&#x27; b=&#x27;1044&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;948&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;915&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1045&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1000&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; H
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;965&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;953&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1010&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;984&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;968&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1044&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1011&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1001&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;988&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1032&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1010&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;81&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1031&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1005&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1032&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1009&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1051&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1036&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1009&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1073&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1055&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1009&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1085&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1077&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;998&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1102&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1089&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1010&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1111&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1105&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1026&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1111&#x27; t=&#x27;998&#x27; r=&#x27;1159&#x27; b=&#x27;1043&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1201&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1159&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1034&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;999&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; W
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1220&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1204&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1034&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1001&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ä
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1240&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1224&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1043&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1000&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1257&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1244&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1011&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1272&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1260&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1034&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1011&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1294&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1275&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1035&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1010&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1313&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1297&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1035&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1002&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1313&#x27; t=&#x27;999&#x27; r=&#x27;1346&#x27; b=&#x27;1035&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1373&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1346&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1034&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1010&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1395&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1378&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1035&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1011&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1418&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1398&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1035&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1010&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1418&#x27; t=&#x27;1000&#x27; r=&#x27;1441&#x27; b=&#x27;1035&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1452&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1441&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1000&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1484&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1454&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1012&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1084&#x27; l=&#x27;260&#x27; t=&#x27;1041&#x27; r=&#x27;1484&#x27; b=&#x27;1098&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;301&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;260&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1084&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1049&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; W
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;318&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;306&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1083&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1060&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;336&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;321&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1092&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1049&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;342&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;331&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1082&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1052&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;357&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;344&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1083&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1059&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;379&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;359&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1082&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1058&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;379&#x27; t=&#x27;1041&#x27; r=&#x27;413&#x27; b=&#x27;1089&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;426&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;413&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1089&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1046&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;438&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;427&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1078&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1054&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;453&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;442&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1077&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1054&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;466&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;457&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1077&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1042&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;495&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;468&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1077&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1050&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; w
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;507&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;499&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1076&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1041&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;520&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;511&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1076&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1043&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;530&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;522&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1076&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1045&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;541&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;533&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1076&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1042&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;561&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;544&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1087&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1054&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;52&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;570&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;564&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1084&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1070&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;570&#x27; t=&#x27;1041&#x27; r=&#x27;604&#x27; b=&#x27;1084&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;620&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;604&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1078&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1055&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;640&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;623&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1078&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1047&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;656&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;644&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1078&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1056&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;672&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;658&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1079&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1056&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;672&#x27; t=&#x27;1047&#x27; r=&#x27;701&#x27; b=&#x27;1084&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;718&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;701&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1081&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1049&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;741&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;721&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1081&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1057&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;756&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;743&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1081&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1058&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;788&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;760&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1092&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1050&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;788&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;760&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1092&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1050&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;788&#x27; t=&#x27;1049&#x27; r=&#x27;820&#x27; b=&#x27;1087&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;837&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;820&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1085&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1053&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;850&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;840&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1087&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1050&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;866&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;853&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1085&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1063&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;866&#x27; t=&#x27;1050&#x27; r=&#x27;897&#x27; b=&#x27;1087&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;925&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;897&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1087&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1053&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; U
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;954&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;929&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1086&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1063&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;973&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;959&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1095&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1053&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;979&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;969&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1086&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1056&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;998&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;982&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1086&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1053&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ä
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1019&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1001&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1085&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1062&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1039&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1022&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1086&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1053&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1057&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1044&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1086&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1062&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1057&#x27; t=&#x27;1053&#x27; r=&#x27;1089&#x27; b=&#x27;1095&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1106&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1089&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1096&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1062&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1122&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1111&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1085&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1062&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1135&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1125&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1094&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1061&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1165&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1137&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1086&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1059&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; w
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1189&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1168&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1085&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1061&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1209&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1191&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1086&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1062&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1229&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1213&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1097&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1063&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1246&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1234&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1085&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1063&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1267&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1248&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1087&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1063&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1276&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1269&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1094&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1079&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1276&#x27; t=&#x27;1053&#x27; r=&#x27;1310&#x27; b=&#x27;1097&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1322&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1310&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1087&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1063&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1344&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1325&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1087&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1064&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1356&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1348&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1087&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1058&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1372&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1359&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1097&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1054&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1397&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1370&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1098&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1054&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1397&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1370&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1098&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1054&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1410&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1401&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1088&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1053&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1425&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1413&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1088&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1064&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1444&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1428&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1089&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1056&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1460&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1448&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1089&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1065&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1484&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1462&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1090&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1065&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1136&#x27; l=&#x27;259&#x27; t=&#x27;1095&#x27; r=&#x27;1485&#x27; b=&#x27;1149&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;277&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;259&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1138&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1107&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;293&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;282&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1138&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1114&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;325&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;297&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1137&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1113&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;325&#x27; t=&#x27;1095&#x27; r=&#x27;350&#x27; b=&#x27;1138&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;87&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;365&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;350&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1136&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1102&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ö
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;392&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;369&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1144&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1099&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;392&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;369&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1144&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1099&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;403&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;392&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1133&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1110&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;424&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;407&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1133&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1108&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;437&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;429&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1131&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1101&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;447&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;440&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1131&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1098&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;460&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;451&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1130&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1095&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;489&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;462&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1096&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;489&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;462&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1096&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;505&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;493&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1128&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1105&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;527&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;508&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1128&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1105&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;527&#x27; t=&#x27;1095&#x27; r=&#x27;541&#x27; b=&#x27;1136&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;574&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;541&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1129&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1095&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; S
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;586&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;577&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1129&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1100&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;607&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;589&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1130&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1106&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;628&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;610&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1131&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1107&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;640&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;631&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1132&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1101&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;659&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;643&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1131&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1103&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ö
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;672&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;662&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1133&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1100&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;686&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;675&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1132&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1110&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;706&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;689&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1133&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1099&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;722&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;710&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1134&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1110&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;744&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;725&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1134&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1110&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;744&#x27; t=&#x27;1095&#x27; r=&#x27;768&#x27; b=&#x27;1136&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;785&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;768&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1142&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1102&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;808&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;789&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1136&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1112&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;820&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;811&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1137&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1105&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;840&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;823&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1138&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1106&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;851&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;843&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1138&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1103&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;872&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;856&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1149&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1115&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;885&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;875&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1138&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1108&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;85&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;893&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;886&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1147&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1131&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;893&#x27; t=&#x27;1102&#x27; r=&#x27;918&#x27; b=&#x27;1149&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;936&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;918&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1149&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1116&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;947&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;938&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1107&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;968&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;951&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1115&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;992&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;971&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1138&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1115&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1010&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;995&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1106&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1024&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1014&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1108&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1024&#x27; t=&#x27;1106&#x27; r=&#x27;1047&#x27; b=&#x27;1139&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1075&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1047&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1115&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1097&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1079&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1115&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1119&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1099&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1115&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1119&#x27; t=&#x27;1103&#x27; r=&#x27;1143&#x27; b=&#x27;1139&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1153&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1143&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1140&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1103&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1184&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1156&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1115&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1184&#x27; t=&#x27;1103&#x27; r=&#x27;1211&#x27; b=&#x27;1140&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1223&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1211&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1116&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1245&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1226&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1115&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1256&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1248&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1138&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1109&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1272&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1259&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1147&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1106&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1284&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1272&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1116&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1300&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1287&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1116&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1320&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1302&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1140&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1115&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1334&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1324&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1109&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1348&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1336&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1140&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1117&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1371&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1351&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1140&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1116&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1371&#x27; t=&#x27;1106&#x27; r=&#x27;1387&#x27; b=&#x27;1147&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1422&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1387&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1140&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1107&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; O
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1441&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1426&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1149&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1108&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1446&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1435&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1140&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1111&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1461&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1448&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1141&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1118&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;80&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1485&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1464&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1142&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1117&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1190&#x27; l=&#x27;261&#x27; t=&#x27;1147&#x27; r=&#x27;1485&#x27; b=&#x27;1204&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;278&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;261&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1192&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1160&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;301&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;283&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1191&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1168&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;316&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;304&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1190&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1168&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;347&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;319&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1199&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1156&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;347&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;319&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1199&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1156&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;347&#x27; t=&#x27;1150&#x27; r=&#x27;394&#x27; b=&#x27;1190&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;407&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;394&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1186&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1163&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;417&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;409&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1185&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1150&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;439&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;420&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1185&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1160&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;455&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;443&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1183&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1160&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;455&#x27; t=&#x27;1147&#x27; r=&#x27;490&#x27; b=&#x27;1190&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;507&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;490&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1190&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1158&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;523&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;511&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1180&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1157&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;543&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;526&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1190&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1147&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;560&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;548&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1180&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1158&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;572&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;563&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1181&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1147&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;601&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;576&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1182&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1159&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;618&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;606&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1182&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1160&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;618&#x27; t=&#x27;1147&#x27; r=&#x27;662&#x27; b=&#x27;1190&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;692&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;662&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1186&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1151&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; R
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;709&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;697&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1185&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1163&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;730&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;714&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1196&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1163&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;742&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;734&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1186&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1151&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;757&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;745&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1186&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1164&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;773&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;760&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1187&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1164&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;794&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;775&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1187&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1164&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;815&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;797&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1189&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1165&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;837&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;820&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1200&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1167&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;857&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;841&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1190&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1161&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;874&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;861&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1201&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1157&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;889&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;874&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1190&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1167&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;905&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;893&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1190&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1167&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;937&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;909&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1191&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1167&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;937&#x27; t=&#x27;1151&#x27; r=&#x27;977&#x27; b=&#x27;1190&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;993&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;977&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1190&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1160&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1007&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;997&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1190&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1156&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1022&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1010&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1189&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1168&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1022&#x27; t=&#x27;1156&#x27; r=&#x27;1062&#x27; b=&#x27;1190&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1080&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1062&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1201&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1169&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1096&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1083&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1191&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1168&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1114&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1098&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1191&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1158&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ö
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1133&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1117&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1200&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1158&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ß
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1147&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1137&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1192&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1162&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1162&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1150&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1191&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1169&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1162&#x27; t=&#x27;1157&#x27; r=&#x27;1198&#x27; b=&#x27;1201&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1240&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1198&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1192&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1157&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; M
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1262&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1245&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1192&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1169&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1293&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1266&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1201&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1159&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1293&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1266&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1201&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1159&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1307&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1297&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1192&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1162&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1307&#x27; t=&#x27;1157&#x27; r=&#x27;1347&#x27; b=&#x27;1201&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1360&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1347&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1192&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1169&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1375&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1362&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1192&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1169&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1388&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1377&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1201&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1169&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1403&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1391&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1192&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1169&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1425&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1406&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1192&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1168&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1445&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1429&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1204&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1171&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1461&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1449&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1194&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1170&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;31&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1485&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1464&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1194&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1170&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1241&#x27; l=&#x27;261&#x27; t=&#x27;1199&#x27; r=&#x27;1484&#x27; b=&#x27;1255&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;280&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;261&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1245&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1221&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;302&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;284&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1244&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1221&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;322&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;306&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1243&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;322&#x27; t=&#x27;1199&#x27; r=&#x27;347&#x27; b=&#x27;1245&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;357&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;347&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1249&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1218&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;379&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;360&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1240&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1216&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;395&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;383&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1248&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1206&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;410&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;394&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1239&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1215&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;441&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;415&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1237&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1213&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;471&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;445&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1234&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;488&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;476&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1233&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1210&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;508&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;490&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1233&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1210&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;529&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;512&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1242&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1199&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;550&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;533&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1234&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1210&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;562&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;554&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1234&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1201&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;575&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;566&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1234&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1204&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;590&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;577&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1234&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;612&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;592&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1235&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;612&#x27; t=&#x27;1199&#x27; r=&#x27;636&#x27; b=&#x27;1245&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;647&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;636&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1245&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1212&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;669&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;649&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1237&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1212&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;669&#x27; t=&#x27;1205&#x27; r=&#x27;692&#x27; b=&#x27;1241&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;705&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;692&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1238&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1205&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;722&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;707&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1238&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1205&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ö
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;744&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;725&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1239&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1215&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;767&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;747&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1240&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1216&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;782&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;770&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1240&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1218&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;803&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;785&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1240&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1217&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;814&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;808&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1241&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1233&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;814&#x27; t=&#x27;1205&#x27; r=&#x27;860&#x27; b=&#x27;1241&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;892&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;860&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1244&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1209&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; B
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;909&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;896&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1244&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1221&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;922&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;912&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1244&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1209&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;940&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;924&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1244&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1212&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;957&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;944&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1244&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1220&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;957&#x27; t=&#x27;1208&#x27; r=&#x27;982&#x27; b=&#x27;1244&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1015&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;982&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1243&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1208&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; S
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1033&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1017&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1248&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1216&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; y
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1051&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1036&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1251&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1209&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1057&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1046&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1242&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1212&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1072&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1060&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1242&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1220&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1103&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1076&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1242&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1219&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1121&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1107&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1243&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1220&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1121&#x27; t=&#x27;1208&#x27; r=&#x27;1151&#x27; b=&#x27;1251&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1167&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1151&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1253&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1210&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1172&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1162&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1244&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1215&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1187&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1175&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1243&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1221&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1207&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1190&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1253&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1210&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1223&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1212&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1244&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1221&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1246&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1227&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1244&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1220&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1246&#x27; t=&#x27;1210&#x27; r=&#x27;1275&#x27; b=&#x27;1253&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1295&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1275&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1253&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1210&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1295&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1275&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1253&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1210&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1326&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1297&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1253&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1326&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1297&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1253&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;1326&#x27; t=&#x27;1210&#x27; r=&#x27;1351&#x27; b=&#x27;1253&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1365&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1351&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1255&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1212&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1376&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1364&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1245&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1222&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1387&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1380&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1245&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1408&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1391&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1246&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1222&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1429&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1412&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1246&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1214&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1440&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1432&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1246&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1214&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1452&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1445&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1246&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1484&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1455&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1255&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1212&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1484&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1455&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1255&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1212&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1293&#x27; l=&#x27;262&#x27; t=&#x27;1251&#x27; r=&#x27;1485&#x27; b=&#x27;1307&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;276&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;262&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1306&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1265&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;286&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;274&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1298&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1275&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;299&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;290&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1298&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1262&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;311&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;300&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1266&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;311&#x27; t=&#x27;1261&#x27; r=&#x27;332&#x27; b=&#x27;1300&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;341&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;332&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1294&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1261&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;362&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;345&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1292&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1269&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;382&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;365&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1291&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1268&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;403&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;387&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1300&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1267&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;419&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;407&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1288&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1266&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;435&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;422&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1288&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1264&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;435&#x27; t=&#x27;1251&#x27; r=&#x27;458&#x27; b=&#x27;1293&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;483&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;458&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1293&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1252&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; Z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;500&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;488&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1285&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1263&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;512&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;504&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1286&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1251&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;525&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;515&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1286&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1255&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;525&#x27; t=&#x27;1251&#x27; r=&#x27;548&#x27; b=&#x27;1293&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;565&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;548&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1297&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1263&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;581&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;569&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1286&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1264&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;601&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;585&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1298&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1265&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;617&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;605&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1287&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1265&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;639&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;620&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1289&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1265&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;662&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;642&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1290&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1257&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ü
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;680&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;665&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1290&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1257&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;697&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;684&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1291&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1267&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;715&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;700&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1290&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1267&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;715&#x27; t=&#x27;1257&#x27; r=&#x27;739&#x27; b=&#x27;1293&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;758&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;739&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1292&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1268&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;779&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;762&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1292&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1268&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;800&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;783&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1293&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1263&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;800&#x27; t=&#x27;1263&#x27; r=&#x27;826&#x27; b=&#x27;1293&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;838&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;826&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1294&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1271&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;859&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;842&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1294&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1265&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;859&#x27; t=&#x27;1262&#x27; r=&#x27;883&#x27; b=&#x27;1294&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;892&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;883&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1264&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;905&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;897&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1262&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;920&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;908&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1274&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;940&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;924&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1306&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1274&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;953&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;944&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1267&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;953&#x27; t=&#x27;1261&#x27; r=&#x27;978&#x27; b=&#x27;1296&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;987&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;978&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1261&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1009&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;989&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1295&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1272&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1009&#x27; t=&#x27;1261&#x27; r=&#x27;1033&#x27; b=&#x27;1295&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1049&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1033&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1295&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1263&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1066&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1054&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1294&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1272&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1082&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1069&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1294&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1271&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1082&#x27; t=&#x27;1261&#x27; r=&#x27;1096&#x27; b=&#x27;1295&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1127&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1096&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1295&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1261&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; N
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1149&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1133&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1273&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1163&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1153&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1266&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1184&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1165&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1272&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1200&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1187&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1297&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1273&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1200&#x27; t=&#x27;1261&#x27; r=&#x27;1224&#x27; b=&#x27;1297&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1241&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1224&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1298&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1266&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1257&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1245&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1297&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1274&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1274&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1261&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1297&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1273&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1274&#x27; t=&#x27;1263&#x27; r=&#x27;1291&#x27; b=&#x27;1298&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1324&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1291&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1297&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1263&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; S
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1345&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1327&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1297&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1274&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1375&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1349&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1307&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1264&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1375&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1349&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1307&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1264&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1393&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1380&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1297&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1274&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;74&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1401&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1394&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1306&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1290&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1401&#x27; t=&#x27;1263&#x27; r=&#x27;1426&#x27; b=&#x27;1306&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1443&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1426&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1298&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1266&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1464&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1447&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1299&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1275&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1485&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1467&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1306&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1265&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ß
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1347&#x27; l=&#x27;261&#x27; t=&#x27;1303&#x27; r=&#x27;1485&#x27; b=&#x27;1360&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;276&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;261&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1356&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1316&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;281&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;274&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1349&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1316&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;297&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;285&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1349&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1327&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;297&#x27; t=&#x27;1311&#x27; r=&#x27;322&#x27; b=&#x27;1348&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;349&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;322&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1348&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1324&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;361&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;352&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1347&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1311&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;373&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;363&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1346&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1315&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;373&#x27; t=&#x27;1311&#x27; r=&#x27;395&#x27; b=&#x27;1347&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;411&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;395&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1342&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1311&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;428&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;416&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1341&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1317&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;444&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;430&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1340&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1316&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;444&#x27; t=&#x27;1303&#x27; r=&#x27;467&#x27; b=&#x27;1347&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;492&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;467&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1346&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1304&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; Z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;509&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;497&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1338&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1315&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;521&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;512&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1339&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1303&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;534&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;524&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1339&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1309&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;78&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;541&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;534&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1348&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1331&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;541&#x27; t=&#x27;1303&#x27; r=&#x27;565&#x27; b=&#x27;1347&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;593&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;565&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1340&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1314&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; w
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;610&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;598&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1340&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1318&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;632&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;613&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1341&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1317&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;654&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;634&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1342&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1317&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;654&#x27; t=&#x27;1308&#x27; r=&#x27;685&#x27; b=&#x27;1347&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;698&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;685&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1342&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1320&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;711&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;701&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1343&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1308&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;731&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;712&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1343&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1319&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;748&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;735&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1344&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1320&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;748&#x27; t=&#x27;1308&#x27; r=&#x27;771&#x27; b=&#x27;1347&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;792&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;771&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1345&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1320&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;812&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;795&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1345&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1321&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;825&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;816&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1345&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1315&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;845&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;828&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1346&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1322&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;862&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;848&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1346&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1323&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;882&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;866&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1357&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1325&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;897&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;886&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1347&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1325&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;928&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;902&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1348&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1325&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;949&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;932&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1348&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1316&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ä
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;968&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;953&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1357&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1316&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ß
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;985&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;973&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1348&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1326&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;985&#x27; t=&#x27;1313&#x27; r=&#x27;1010&#x27; b=&#x27;1357&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1038&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1010&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1349&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1314&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; A
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1060&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1040&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1347&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1324&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;61&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1079&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1063&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1347&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1318&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1099&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1083&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1358&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1324&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1113&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1103&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1348&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1315&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1128&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1116&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1347&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1324&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1140&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1132&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1347&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1313&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1170&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1143&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1358&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1315&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1170&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1143&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1358&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1315&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1194&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1174&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1348&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1325&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1214&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1196&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1348&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1325&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1235&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1219&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1359&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1327&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1235&#x27; t=&#x27;1313&#x27; r=&#x27;1260&#x27; b=&#x27;1359&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1279&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1260&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1349&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1326&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1292&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1284&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1349&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1315&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1322&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1295&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1359&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1317&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1322&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1295&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1359&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1317&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1336&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1326&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1350&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1320&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1336&#x27; t=&#x27;1315&#x27; r=&#x27;1369&#x27; b=&#x27;1359&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1382&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1369&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1350&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1328&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1393&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1385&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1351&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1316&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1414&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1396&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1351&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1327&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1427&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1418&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1351&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1321&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1441&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1429&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1351&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1327&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1452&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1444&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1351&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1316&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1477&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1456&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1351&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1321&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1477&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1456&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1351&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1321&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1485&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1477&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1360&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1345&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1399&#x27; l=&#x27;261&#x27; t=&#x27;1359&#x27; r=&#x27;1486&#x27; b=&#x27;1412&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;271&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;261&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1403&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1368&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;293&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;274&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1403&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1379&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;293&#x27; t=&#x27;1360&#x27; r=&#x27;316&#x27; b=&#x27;1403&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;344&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;316&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1367&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; C
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;361&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;346&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1400&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1376&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;382&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;364&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1399&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1374&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;404&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;386&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1406&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1362&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;404&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;386&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1406&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1362&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;417&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;407&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1395&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1360&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;431&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;420&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1394&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1369&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;444&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;434&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1393&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1364&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;444&#x27; t=&#x27;1359&#x27; r=&#x27;468&#x27; b=&#x27;1403&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;484&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;468&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1403&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1369&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;501&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;489&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1392&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1369&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;516&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;504&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1392&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1369&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;537&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;520&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1393&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1369&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;550&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;541&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1392&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1363&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;569&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;552&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1403&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1359&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;585&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;573&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1393&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1370&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;609&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;588&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1394&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1369&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;609&#x27; t=&#x27;1359&#x27; r=&#x27;624&#x27; b=&#x27;1403&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;87&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;632&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;624&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1386&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1377&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; -
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;660&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;633&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1394&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1371&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;683&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;664&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1396&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1362&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ü
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;700&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;687&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1404&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1362&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;709&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;697&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1362&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;721&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;709&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1395&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1373&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;743&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;724&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1396&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1372&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;753&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;746&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1396&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1388&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;753&#x27; t=&#x27;1362&#x27; r=&#x27;801&#x27; b=&#x27;1401&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;843&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;801&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1399&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1364&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; M
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;857&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;848&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1399&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1365&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;880&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;860&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1400&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1371&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;880&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;860&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1400&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1371&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;896&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;884&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1400&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1378&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;919&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;899&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1401&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1377&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;919&#x27; t=&#x27;1364&#x27; r=&#x27;943&#x27; b=&#x27;1401&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;953&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;943&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1367&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;975&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;956&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1401&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1378&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;975&#x27; t=&#x27;1366&#x27; r=&#x27;999&#x27; b=&#x27;1402&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1015&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;999&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1371&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1028&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1020&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1366&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1043&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1031&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1378&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1058&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1048&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1412&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1368&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1070&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1058&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1378&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1102&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1074&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1378&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1102&#x27; t=&#x27;1366&#x27; r=&#x27;1127&#x27; b=&#x27;1412&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1139&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1127&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1378&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1161&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1144&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1401&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1378&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1177&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1164&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1378&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1196&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1181&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1378&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;87&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1216&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1198&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1410&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1375&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; p
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1237&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1221&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1370&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ä
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1248&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1241&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1402&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1367&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1265&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1252&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1412&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1369&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1290&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1262&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1412&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1369&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1290&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1262&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1412&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1369&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1307&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1295&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1403&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1379&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1330&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1310&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1403&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1379&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1330&#x27; t=&#x27;1367&#x27; r=&#x27;1348&#x27; b=&#x27;1405&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1382&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1348&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1404&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1370&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; D
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1404&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1385&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1404&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1380&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1425&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1408&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1405&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1380&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1436&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1429&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1404&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1372&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1449&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1441&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1404&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1369&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;76&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1468&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1452&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1404&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1375&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1486&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1471&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1400&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1386&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ¬
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1453&#x27; l=&#x27;263&#x27; t=&#x27;1413&#x27; r=&#x27;1487&#x27; b=&#x27;1466&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;291&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;263&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1456&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1433&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;314&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;294&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1455&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1431&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;333&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;316&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1454&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1424&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;333&#x27; t=&#x27;1414&#x27; r=&#x27;357&#x27; b=&#x27;1456&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;378&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;357&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1458&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1418&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;378&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;357&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1458&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1418&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;392&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;381&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1448&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1426&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;412&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;396&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1454&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1414&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;426&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;416&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1446&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1415&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;426&#x27; t=&#x27;1414&#x27; r=&#x27;448&#x27; b=&#x27;1453&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;467&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;448&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1445&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1421&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;490&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;470&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1445&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1421&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;513&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;493&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1447&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1422&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;513&#x27; t=&#x27;1413&#x27; r=&#x27;536&#x27; b=&#x27;1453&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;571&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;536&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1448&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1413&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; D
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;587&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;575&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1448&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1425&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;608&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;590&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1448&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1423&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;622&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;612&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1448&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1418&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;637&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;625&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1458&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1414&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;663&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;636&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1458&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1414&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;663&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;636&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1458&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1414&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;676&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;667&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1449&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1416&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;697&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;680&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1449&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1425&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;719&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;700&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1450&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1425&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;738&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;722&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1450&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1417&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;755&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;748&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1459&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1442&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;755&#x27; t=&#x27;1413&#x27; r=&#x27;786&#x27; b=&#x27;1453&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;803&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;786&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1451&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1418&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;826&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;807&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1451&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1427&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;845&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;828&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1451&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1422&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;845&#x27; t=&#x27;1418&#x27; r=&#x27;876&#x27; b=&#x27;1453&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;886&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;876&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1452&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1418&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;908&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;889&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1453&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1429&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;908&#x27; t=&#x27;1418&#x27; r=&#x27;948&#x27; b=&#x27;1453&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;965&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;948&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1455&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1422&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;981&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;970&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1454&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1432&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1014&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;985&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1455&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1431&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1014&#x27; t=&#x27;1422&#x27; r=&#x27;1046&#x27; b=&#x27;1455&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1054&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1046&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1455&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1422&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1071&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1059&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1454&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1432&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;78&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1091&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1075&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1463&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1425&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;78&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1091&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1075&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1463&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1425&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1105&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1096&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1455&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1425&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1120&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1107&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1455&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1432&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1143&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1123&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1456&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1431&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1143&#x27; t=&#x27;1421&#x27; r=&#x27;1176&#x27; b=&#x27;1463&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1206&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1176&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1456&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1422&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; V
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1219&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1210&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1455&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1421&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1234&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1222&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1455&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1433&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1249&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1237&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1455&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1432&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1262&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1253&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1456&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1426&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1277&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1265&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1455&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1433&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1288&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1280&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1455&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1424&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1300&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1290&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1463&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1423&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; j
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1320&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1303&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1456&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1433&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1340&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1324&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1463&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1423&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1356&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1343&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1456&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1433&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1376&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1359&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1466&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1423&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1398&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1380&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1456&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1433&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1420&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1402&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1456&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1433&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1440&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1424&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1456&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1426&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1458&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1446&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1457&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1434&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1473&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1460&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1458&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1434&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1487&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1476&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1457&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1427&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1506&#x27; l=&#x27;262&#x27; t=&#x27;1465&#x27; r=&#x27;1486&#x27; b=&#x27;1519&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;277&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;262&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1519&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1475&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;287&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;275&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1509&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1486&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;300&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;291&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1508&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1473&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;321&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;301&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1508&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1483&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;336&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;324&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1507&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1482&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;336&#x27; t=&#x27;1468&#x27; r=&#x27;359&#x27; b=&#x27;1506&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;370&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;359&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1504&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1468&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;390&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;371&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1504&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1478&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;411&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;392&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1502&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1477&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;426&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;414&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1499&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1476&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;442&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;429&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1498&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1475&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;458&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;445&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1498&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1475&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;458&#x27; t=&#x27;1465&#x27; r=&#x27;482&#x27; b=&#x27;1506&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;517&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;482&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1499&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1465&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; O
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;533&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;520&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1498&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1476&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;553&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;537&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1509&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1476&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;578&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;560&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1500&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1476&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;599&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;582&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1501&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1477&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;612&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;603&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1502&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1466&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;627&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;615&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1511&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1468&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;645&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;627&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1502&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1479&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;657&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;648&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1502&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1473&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;668&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;660&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1502&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1467&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;686&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;672&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1502&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1479&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;710&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;690&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1503&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1478&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;710&#x27; t=&#x27;1465&#x27; r=&#x27;742&#x27; b=&#x27;1511&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;760&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;742&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1504&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1479&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;782&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;763&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1503&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1479&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;808&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;786&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1513&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1470&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;808&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;786&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1513&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1470&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;826&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;808&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1504&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1480&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;849&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;828&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1504&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1470&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;849&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;828&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1504&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1470&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;864&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;852&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1504&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1482&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;886&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;866&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1505&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1481&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;906&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;889&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1506&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1474&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;906&#x27; t=&#x27;1470&#x27; r=&#x27;937&#x27; b=&#x27;1513&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;85&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;955&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;937&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1506&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1480&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; v
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;972&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;960&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1507&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1484&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;987&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;975&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1508&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1484&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1008&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;990&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1507&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1483&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1029&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1012&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1507&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1484&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1061&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1033&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1517&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1475&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1061&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1033&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1517&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1475&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1073&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1064&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1507&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1475&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1093&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1076&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1507&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1475&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ä
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1109&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1097&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1516&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1474&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1126&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1108&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1514&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1473&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1126&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1108&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1514&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1473&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1146&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1130&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1518&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1485&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1160&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1150&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1507&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1478&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1160&#x27; t=&#x27;1473&#x27; r=&#x27;1183&#x27; b=&#x27;1517&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1200&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1183&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1517&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1475&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1221&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1204&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1509&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1485&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1235&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1225&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1508&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1479&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1248&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1241&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1516&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1501&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1248&#x27; t=&#x27;1475&#x27; r=&#x27;1280&#x27; b=&#x27;1517&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1300&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1280&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1509&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1485&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1319&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1305&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1509&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1485&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1352&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1324&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1518&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1476&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1352&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1324&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1518&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1476&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;1352&#x27; t=&#x27;1475&#x27; r=&#x27;1382&#x27; b=&#x27;1511&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1392&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1382&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1510&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1475&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1423&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1395&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1510&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1487&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1453&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1427&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1511&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1487&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1469&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1458&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1511&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1487&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1486&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1473&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1511&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1487&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1557&#x27; l=&#x27;263&#x27; t=&#x27;1518&#x27; r=&#x27;1486&#x27; b=&#x27;1574&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;276&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;263&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1571&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1529&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;303&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;275&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1571&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1529&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;303&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;275&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1571&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1529&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;333&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;306&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1560&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1535&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; w
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;354&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;337&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1559&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1535&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;376&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;357&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1558&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1534&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;390&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;378&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1556&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1522&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;404&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;393&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1555&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1532&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;424&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;407&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1554&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1529&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;444&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;428&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1552&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1520&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;453&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;447&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1560&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1544&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;453&#x27; t=&#x27;1518&#x27; r=&#x27;485&#x27; b=&#x27;1557&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;500&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;485&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1550&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1528&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;521&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;504&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1550&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1518&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;521&#x27; t=&#x27;1518&#x27; r=&#x27;552&#x27; b=&#x27;1557&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;565&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;552&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1550&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1528&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;585&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;568&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1550&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1524&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;585&#x27; t=&#x27;1522&#x27; r=&#x27;618&#x27; b=&#x27;1557&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;635&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;618&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1553&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1522&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;651&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;639&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1554&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1531&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;683&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;655&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1554&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1531&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;683&#x27; t=&#x27;1522&#x27; r=&#x27;711&#x27; b=&#x27;1557&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;739&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;711&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1556&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1529&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; w
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;756&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;744&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1556&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1533&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;773&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;759&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1564&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1523&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;779&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;769&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1556&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1527&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;790&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;782&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1557&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1524&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;802&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;794&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1556&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1522&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;832&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;805&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1566&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1523&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;832&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;805&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1566&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1523&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;849&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;837&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1557&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1534&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;873&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;852&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1557&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1533&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;873&#x27; t=&#x27;1522&#x27; r=&#x27;907&#x27; b=&#x27;1560&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;922&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;907&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1558&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1535&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;942&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;926&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1559&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1527&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;958&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;946&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1560&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1537&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;975&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;961&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1559&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1536&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;975&#x27; t=&#x27;1527&#x27; r=&#x27;1005&#x27; b=&#x27;1560&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1021&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1005&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1560&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1528&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1038&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1026&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1559&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1537&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1069&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1042&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1560&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1536&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1069&#x27; t=&#x27;1526&#x27; r=&#x27;1110&#x27; b=&#x27;1560&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1126&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1110&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1561&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1528&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ö
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1144&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1130&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1568&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1528&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1150&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1139&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1560&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1533&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1161&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1153&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1561&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1529&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1174&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1166&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1561&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1526&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1204&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1176&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1570&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1528&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1204&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1176&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1570&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1528&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1220&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1208&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1560&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1538&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1243&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1224&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1561&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1538&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1243&#x27; t=&#x27;1526&#x27; r=&#x27;1275&#x27; b=&#x27;1570&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1306&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1275&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1563&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1528&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; R
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1322&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1310&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1562&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1539&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1342&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1325&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1573&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1540&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1355&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1346&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1563&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1528&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1370&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1358&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1563&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1540&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1385&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1372&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1563&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1540&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1407&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1388&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1563&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1539&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1428&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1409&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1563&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1539&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1449&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1432&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1574&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1541&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1469&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1452&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1563&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1535&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1486&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1472&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1559&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1546&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; -
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1611&#x27; l=&#x27;264&#x27; t=&#x27;1569&#x27; r=&#x27;1485&#x27; b=&#x27;1626&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;277&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;264&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1623&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1582&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;291&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;275&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1623&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1590&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; y
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;309&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;294&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1622&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1580&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;315&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;304&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1613&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1584&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;329&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;317&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1613&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1590&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;360&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;333&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1612&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1587&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;360&#x27; t=&#x27;1574&#x27; r=&#x27;385&#x27; b=&#x27;1618&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;398&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;385&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1618&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1575&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;63&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;413&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;399&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1607&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1584&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;425&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;417&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1607&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1574&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;445&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;429&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1616&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1582&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;461&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;449&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1603&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1581&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;78&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;483&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;464&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1604&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1579&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;483&#x27; t=&#x27;1569&#x27; r=&#x27;507&#x27; b=&#x27;1611&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;521&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;507&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1611&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1569&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;535&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;520&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1602&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1579&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;548&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;538&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1602&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1570&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;558&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;550&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1602&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1570&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;567&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;561&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1602&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1596&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;567&#x27; t=&#x27;1569&#x27; r=&#x27;615&#x27; b=&#x27;1611&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;649&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;615&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1605&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1572&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; D
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;670&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;653&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1606&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1582&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;691&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;674&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1606&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1578&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;691&#x27; t=&#x27;1572&#x27; r=&#x27;717&#x27; b=&#x27;1611&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;748&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;717&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1609&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1574&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; V
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;768&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;753&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1609&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1585&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;781&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;772&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1609&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1577&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;795&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;784&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1610&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1576&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;802&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;796&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1618&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1602&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;802&#x27; t=&#x27;1574&#x27; r=&#x27;828&#x27; b=&#x27;1611&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;844&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;828&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1610&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1579&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;855&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;849&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1610&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1602&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;855&#x27; t=&#x27;1577&#x27; r=&#x27;881&#x27; b=&#x27;1611&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;898&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;881&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1619&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1577&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;909&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;903&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1611&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1603&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;909&#x27; t=&#x27;1577&#x27; r=&#x27;935&#x27; b=&#x27;1612&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;951&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;935&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1612&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1581&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;964&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;955&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1612&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1577&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;980&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;967&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1612&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1589&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;980&#x27; t=&#x27;1577&#x27; r=&#x27;1017&#x27; b=&#x27;1612&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1033&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1017&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1612&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1581&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1050&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1038&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1612&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1589&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1071&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1053&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1612&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1589&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1086&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1074&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1612&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1580&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1100&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1088&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1612&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1590&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1120&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1103&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1612&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1589&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1141&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1124&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1613&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1584&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1158&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1145&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1613&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1590&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1158&#x27; t=&#x27;1579&#x27; r=&#x27;1183&#x27; b=&#x27;1613&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1226&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1183&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1615&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1579&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; M
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1247&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1230&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1614&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1590&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1265&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1251&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1624&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1581&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1274&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1261&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1624&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1581&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1286&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1273&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1614&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1591&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1295&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1288&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1623&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1607&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1295&#x27; t=&#x27;1579&#x27; r=&#x27;1329&#x27; b=&#x27;1624&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1349&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1329&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1615&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1591&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1365&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1353&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1615&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1592&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1377&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1369&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1615&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1580&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1397&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1380&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1626&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1592&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1411&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1401&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1616&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1585&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1411&#x27; t=&#x27;1580&#x27; r=&#x27;1434&#x27; b=&#x27;1625&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1453&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1434&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1624&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1582&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1453&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1434&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1624&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1582&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1485&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1456&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1625&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1583&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1485&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1456&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1625&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1583&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1662&#x27; l=&#x27;265&#x27; t=&#x27;1624&#x27; r=&#x27;1485&#x27; b=&#x27;1679&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;282&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;265&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1667&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1644&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;304&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;287&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1667&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1644&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;324&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;307&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1666&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1641&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; v
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;340&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;329&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1664&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1642&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;355&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;344&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1664&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1641&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;369&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;358&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1663&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1630&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;384&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;372&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1662&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1639&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;404&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;386&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1661&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1638&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;427&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;408&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1660&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1637&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;445&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;430&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1659&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1625&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;466&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;449&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1658&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1634&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;482&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;469&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1656&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1632&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;482&#x27; t=&#x27;1625&#x27; r=&#x27;513&#x27; b=&#x27;1663&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;523&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;513&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1663&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1631&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;546&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;527&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1654&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1630&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;577&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;550&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1654&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1632&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;577&#x27; t=&#x27;1624&#x27; r=&#x27;612&#x27; b=&#x27;1663&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;624&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;612&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1655&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1633&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;641&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;629&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1655&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1633&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;658&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;643&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1664&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1624&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;664&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;653&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1656&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1627&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;679&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;667&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1657&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1635&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;716&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;682&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1659&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1634&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;725&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;718&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1667&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1652&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;725&#x27; t=&#x27;1624&#x27; r=&#x27;755&#x27; b=&#x27;1667&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;783&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;755&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1661&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1634&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; w
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;804&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;788&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1662&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1629&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ä
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;824&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;808&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1671&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1628&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;841&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;828&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1662&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1639&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;856&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;844&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1662&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1639&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;878&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;859&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1663&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1639&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;898&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;881&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1664&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1631&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;898&#x27; t=&#x27;1628&#x27; r=&#x27;930&#x27; b=&#x27;1664&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;947&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;930&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1664&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1631&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;960&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;951&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1664&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1629&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;976&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;964&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1664&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1641&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;976&#x27; t=&#x27;1629&#x27; r=&#x27;1005&#x27; b=&#x27;1664&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1036&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1005&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1664&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1630&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; B
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1053&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1041&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1665&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1642&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1075&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1057&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1665&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1641&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1105&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1078&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1665&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1642&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1119&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1109&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1665&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1635&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1133&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1120&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1666&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1643&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1155&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1135&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1666&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1642&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1155&#x27; t=&#x27;1630&#x27; r=&#x27;1187&#x27; b=&#x27;1666&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1204&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1187&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1667&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1634&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1221&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1208&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1666&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1643&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1236&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1223&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1666&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1643&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1236&#x27; t=&#x27;1633&#x27; r=&#x27;1265&#x27; b=&#x27;1667&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1295&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1265&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1667&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1633&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; R
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1312&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1299&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1667&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1644&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1331&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1315&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1678&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1644&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1344&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1335&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1667&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1633&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1359&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1347&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1667&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1644&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1375&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1362&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1668&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1644&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1396&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1377&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1668&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1645&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1417&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1399&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1668&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1645&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1438&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1422&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1679&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1646&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1455&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1442&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1669&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1646&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1476&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1458&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1669&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1645&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1485&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1478&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1677&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1662&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1716&#x27; l=&#x27;262&#x27; t=&#x27;1674&#x27; r=&#x27;1486&#x27; b=&#x27;1729&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;272&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;262&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1726&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1686&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; j
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;287&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;275&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1720&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1697&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;307&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;290&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1720&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1688&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;323&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;311&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1720&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1695&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;338&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;326&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1718&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1694&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;338&#x27; t=&#x27;1674&#x27; r=&#x27;365&#x27; b=&#x27;1723&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;380&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;365&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1716&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1683&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ö
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;405&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;384&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1723&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1680&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;405&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;384&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1723&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1680&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;418&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;407&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1713&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1690&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;439&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;422&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1713&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1688&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;452&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;443&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1711&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1681&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;462&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;454&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1711&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1678&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;474&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;466&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1710&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1674&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;488&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;477&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1708&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1686&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;504&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;487&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1718&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1675&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;521&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;509&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1708&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1685&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;544&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;524&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1708&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1683&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;544&#x27; t=&#x27;1674&#x27; r=&#x27;574&#x27; b=&#x27;1716&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;594&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;574&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1706&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1683&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;615&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;597&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1707&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1683&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;636&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;619&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1707&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1675&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;636&#x27; t=&#x27;1675&#x27; r=&#x27;673&#x27; b=&#x27;1716&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;691&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;673&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1709&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1683&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; v
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;711&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;697&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1710&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1687&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;723&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;715&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1711&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1679&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;738&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;727&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1710&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1679&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;756&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;740&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1711&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1683&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;769&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;760&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1712&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1683&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;788&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;770&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1723&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1680&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;811&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;791&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1715&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1682&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ü
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;841&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;815&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1715&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1691&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;853&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;845&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1714&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1683&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;865&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;857&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1715&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1680&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;896&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;868&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1725&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1682&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;896&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;868&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1725&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1682&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;913&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;901&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1716&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1693&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;936&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;916&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1717&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1693&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;936&#x27; t=&#x27;1679&#x27; r=&#x27;967&#x27; b=&#x27;1719&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;995&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;967&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1718&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1683&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; C
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1014&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;999&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1718&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1694&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1035&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1017&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1718&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1694&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1049&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1040&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1717&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1688&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1064&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1051&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1718&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1695&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1083&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1068&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1719&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1696&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1096&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1086&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1719&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1686&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1111&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1098&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1719&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1696&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1111&#x27; t=&#x27;1683&#x27; r=&#x27;1143&#x27; b=&#x27;1719&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1160&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1143&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1720&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1697&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1179&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1163&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1719&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1686&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1200&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1184&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1729&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1686&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1220&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1206&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1719&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1697&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1232&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1224&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1720&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1688&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1252&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1236&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1719&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1688&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1267&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1260&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1727&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1712&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1267&#x27; t=&#x27;1685&#x27; r=&#x27;1309&#x27; b=&#x27;1721&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1316&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1309&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1720&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1689&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1330&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1322&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1720&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1685&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1345&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1333&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1720&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1697&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1364&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1348&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1720&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1687&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1380&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1369&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1721&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1698&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1397&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1383&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1721&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1697&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1397&#x27; t=&#x27;1685&#x27; r=&#x27;1420&#x27; b=&#x27;1721&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1437&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1420&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1721&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1690&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1454&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1442&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1722&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1698&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1486&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1458&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1722&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1698&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1769&#x27; l=&#x27;264&#x27; t=&#x27;1726&#x27; r=&#x27;1487&#x27; b=&#x27;1785&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;273&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;264&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1772&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1740&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;289&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;277&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1772&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1750&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;81&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;308&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;292&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1781&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1743&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;81&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;308&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;292&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1781&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1743&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;322&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;312&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1773&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1743&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;336&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;324&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1773&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1749&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;351&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;339&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1772&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1748&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;366&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;354&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1771&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1747&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;388&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;369&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1770&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1745&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;388&#x27; t=&#x27;1732&#x27; r=&#x27;412&#x27; b=&#x27;1777&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;425&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;412&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1777&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1732&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;70&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;440&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;426&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1766&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1742&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;452&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;444&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1765&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1732&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;472&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;456&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1775&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1741&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;488&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;476&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1763&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1740&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;510&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;490&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1763&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1738&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;510&#x27; t=&#x27;1726&#x27; r=&#x27;534&#x27; b=&#x27;1769&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;562&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;534&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1761&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1736&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;581&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;566&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1760&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1726&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ö
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;612&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;585&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1769&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1727&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;612&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;585&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1769&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1727&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;626&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;616&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1759&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1730&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;640&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;628&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1760&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1737&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;662&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;642&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1760&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1736&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;672&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;666&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1761&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1754&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;672&#x27; t=&#x27;1726&#x27; r=&#x27;720&#x27; b=&#x27;1769&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;748&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;720&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1764&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1730&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; E
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;768&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;752&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1765&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1736&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;768&#x27; t=&#x27;1730&#x27; r=&#x27;793&#x27; b=&#x27;1769&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;821&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;793&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1767&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1740&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; w
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;844&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;826&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1768&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1744&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;860&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;846&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1768&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1744&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;860&#x27; t=&#x27;1736&#x27; r=&#x27;883&#x27; b=&#x27;1769&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;897&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;883&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1775&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1736&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;915&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;897&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1769&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1737&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ü
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;931&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;918&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1769&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1746&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;931&#x27; t=&#x27;1736&#x27; r=&#x27;955&#x27; b=&#x27;1775&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;989&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;955&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1770&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1736&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; D
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1006&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;994&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1770&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1748&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1028&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1009&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1770&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1747&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1041&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1032&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1771&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1741&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1056&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1044&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1780&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1739&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1083&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1055&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1779&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1739&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1083&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1055&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1779&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1739&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1096&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1087&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1772&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1740&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1116&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1099&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1772&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1748&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1138&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1119&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1772&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1748&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1158&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1141&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1772&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1740&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1158&#x27; t=&#x27;1736&#x27; r=&#x27;1192&#x27; b=&#x27;1772&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1204&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1192&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1772&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1749&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1217&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1208&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1772&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1738&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1237&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1219&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1772&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1749&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1237&#x27; t=&#x27;1738&#x27; r=&#x27;1264&#x27; b=&#x27;1772&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1279&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1264&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1772&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1749&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1306&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1282&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1781&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1739&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1306&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1282&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1781&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1739&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1318&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1306&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1773&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1750&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1339&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1321&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1773&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1749&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1354&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1342&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1773&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1740&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1375&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1357&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1773&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1749&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1396&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1378&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1774&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1749&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1416&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1400&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1774&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1742&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1429&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1420&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1774&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1739&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1448&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1432&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1785&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1752&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1465&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1452&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1775&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1752&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1487&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1470&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1774&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1745&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1822&#x27; l=&#x27;263&#x27; t=&#x27;1780&#x27; r=&#x27;1487&#x27; b=&#x27;1837&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;291&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;263&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1825&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1791&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; U
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;312&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;294&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1825&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1802&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;333&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;317&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1836&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1803&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;344&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;336&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1826&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1793&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;366&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;349&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1825&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1792&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ü
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;392&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;370&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1823&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1790&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;392&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;370&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1823&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1790&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;399&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;394&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1831&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1816&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;399&#x27; t=&#x27;1784&#x27; r=&#x27;432&#x27; b=&#x27;1827&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;448&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;432&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1820&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1787&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;469&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;453&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1819&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1795&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;85&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;489&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;473&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1827&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1784&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ß
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;489&#x27; t=&#x27;1780&#x27; r=&#x27;514&#x27; b=&#x27;1822&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;544&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;514&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1815&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1780&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; N
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;566&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;548&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1813&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1790&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;586&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;568&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1822&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1787&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; p
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;605&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;591&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1812&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1789&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;619&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;610&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1812&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1780&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;633&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;621&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1812&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1789&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;653&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;638&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1812&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1789&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;676&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;656&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1813&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1789&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;676&#x27; t=&#x27;1780&#x27; r=&#x27;699&#x27; b=&#x27;1822&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;719&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;699&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1814&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1790&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;740&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;723&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1816&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1792&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;772&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;744&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1827&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1784&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;772&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;744&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1827&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1784&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;772&#x27; t=&#x27;1784&#x27; r=&#x27;804&#x27; b=&#x27;1822&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;821&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;804&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1819&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1787&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;839&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;826&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1820&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1797&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;860&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;842&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1821&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1797&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;860&#x27; t=&#x27;1787&#x27; r=&#x27;886&#x27; b=&#x27;1822&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;917&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;886&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1822&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1788&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; B
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;934&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;921&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1823&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1800&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;953&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;936&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1823&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1790&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;966&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;957&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1824&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1788&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;987&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;969&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1824&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1800&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1008&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;991&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1834&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1801&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1031&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1012&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1823&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1800&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1051&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1034&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1824&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1801&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1074&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1056&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1835&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1802&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1088&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1076&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1825&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1802&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1112&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1093&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1825&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1801&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1112&#x27; t=&#x27;1788&#x27; r=&#x27;1144&#x27; b=&#x27;1826&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1160&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1144&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1826&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1793&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1177&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1165&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1825&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1802&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1197&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1180&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1825&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1796&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1197&#x27; t=&#x27;1790&#x27; r=&#x27;1223&#x27; b=&#x27;1826&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1251&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1223&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1825&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1790&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; C
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1270&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1255&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1825&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1802&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1292&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1273&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1826&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1802&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1313&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1296&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1837&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1803&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1328&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1316&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1826&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1802&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1344&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1331&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1825&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1803&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1361&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1347&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1836&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1793&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1370&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1358&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1836&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1793&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1381&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1369&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1827&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1804&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1402&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1384&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1827&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1798&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1402&#x27; t=&#x27;1790&#x27; r=&#x27;1426&#x27; b=&#x27;1827&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1444&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1426&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1827&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1801&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; v
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1464&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1448&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1827&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1803&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1487&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1467&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1827&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1803&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1874&#x27; l=&#x27;266&#x27; t=&#x27;1831&#x27; r=&#x27;1486&#x27; b=&#x27;1888&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;294&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;266&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1879&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1843&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; C
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;313&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;296&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1888&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1845&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;335&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;318&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1879&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1855&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;347&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;338&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1878&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1849&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;358&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;351&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1879&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1843&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;370&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;360&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1877&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1844&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;380&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;371&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1877&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1844&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;398&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;384&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1876&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1853&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;421&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;402&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1876&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1851&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;421&#x27; t=&#x27;1836&#x27; r=&#x27;444&#x27; b=&#x27;1880&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;463&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;444&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1873&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1849&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;475&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;466&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1872&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1836&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;504&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;478&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1880&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1837&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;504&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;478&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1880&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1837&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;517&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;508&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1867&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1837&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;517&#x27; t=&#x27;1831&#x27; r=&#x27;532&#x27; b=&#x27;1875&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;556&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;532&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1875&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1834&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; F
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;571&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;559&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1867&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1843&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;591&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;574&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1867&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1843&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;613&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;594&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1866&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1842&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;627&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;615&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1866&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1833&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;641&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;629&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1865&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1842&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;657&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;645&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1865&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1843&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;669&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;661&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1865&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1831&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;700&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;673&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1875&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1833&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;700&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;673&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1875&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1833&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;700&#x27; t=&#x27;1831&#x27; r=&#x27;726&#x27; b=&#x27;1874&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;745&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;726&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1868&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1844&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;756&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;748&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1869&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1837&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;777&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;760&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1869&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1840&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;777&#x27; t=&#x27;1837&#x27; r=&#x27;793&#x27; b=&#x27;1874&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;824&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;793&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1871&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1838&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; K
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;842&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;827&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1871&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1839&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ö
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;864&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;846&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1873&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1849&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;876&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;867&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1873&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1839&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;896&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;880&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1884&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1851&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;912&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;899&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1874&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1851&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;927&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;915&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1875&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1852&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;939&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;930&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1876&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1841&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;970&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;942&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1886&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1843&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;970&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;942&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1886&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1843&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;970&#x27; t=&#x27;1838&#x27; r=&#x27;994&#x27; b=&#x27;1885&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1015&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;994&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1880&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1856&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1035&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1018&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1880&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1856&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1067&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1038&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1885&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1845&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1067&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1038&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1885&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1845&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;1067&#x27; t=&#x27;1845&#x27; r=&#x27;1091&#x27; b=&#x27;1879&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1108&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1091&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1879&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1846&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1125&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1112&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1879&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1855&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1147&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1128&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1879&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1854&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1147&#x27; t=&#x27;1845&#x27; r=&#x27;1172&#x27; b=&#x27;1879&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1191&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1172&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1878&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1854&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1202&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1193&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1877&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1845&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1215&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1205&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1878&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1847&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1229&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1217&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1878&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1855&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1252&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1232&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1879&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1854&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1252&#x27; t=&#x27;1845&#x27; r=&#x27;1270&#x27; b=&#x27;1879&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1303&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1270&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1879&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1845&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; G
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1318&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1306&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1878&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1855&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1334&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1322&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1878&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1856&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1355&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1337&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1879&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1855&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1369&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1358&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1886&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1856&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1384&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1372&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1879&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1856&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1406&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1386&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1879&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1855&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1406&#x27; t=&#x27;1845&#x27; r=&#x27;1430&#x27; b=&#x27;1880&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1449&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1430&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1879&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1855&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1470&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1452&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1880&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1856&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1486&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1473&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1875&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1863&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ¬
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1927&#x27; l=&#x27;266&#x27; t=&#x27;1883&#x27; r=&#x27;1486&#x27; b=&#x27;1941&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;283&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;266&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1941&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1909&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;299&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;287&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1931&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1908&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;321&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;302&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1932&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1908&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;341&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;325&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1931&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1907&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;371&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;345&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1931&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1907&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;403&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;376&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1930&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1905&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;418&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;407&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1928&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1905&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;440&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;422&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1928&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1904&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;440&#x27; t=&#x27;1890&#x27; r=&#x27;471&#x27; b=&#x27;1935&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;488&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;471&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1935&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1890&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;509&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;492&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1925&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1900&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;522&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;514&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1923&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1893&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;531&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;525&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1922&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1915&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;531&#x27; t=&#x27;1883&#x27; r=&#x27;578&#x27; b=&#x27;1927&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;619&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;578&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1921&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1885&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; W
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;631&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;622&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1919&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1883&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;647&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;635&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1918&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1895&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;647&#x27; t=&#x27;1883&#x27; r=&#x27;678&#x27; b=&#x27;1927&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;692&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;678&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1927&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1886&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;718&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;691&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1925&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1885&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;718&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;691&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1925&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1885&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;741&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;722&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1919&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1895&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;757&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;745&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1919&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1897&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;780&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;759&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1921&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1888&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;780&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;759&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1921&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1888&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;780&#x27; t=&#x27;1885&#x27; r=&#x27;811&#x27; b=&#x27;1927&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;828&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;811&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1933&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1890&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;849&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;832&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1924&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1892&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ä
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;862&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;852&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1925&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1895&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;874&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;865&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1925&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1896&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;889&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;877&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1926&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1903&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;889&#x27; t=&#x27;1890&#x27; r=&#x27;921&#x27; b=&#x27;1933&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;940&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;921&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1931&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1894&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;940&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;921&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1931&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1894&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;972&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;944&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1938&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1897&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;972&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;944&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1938&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1897&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;972&#x27; t=&#x27;1894&#x27; r=&#x27;999&#x27; b=&#x27;1932&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1016&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;999&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1932&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1901&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;18&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1038&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1021&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1932&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1909&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;18&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1060&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1041&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1932&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1908&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;18&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1084&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1063&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1931&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1907&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;18&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1084&#x27; t=&#x27;1895&#x27; r=&#x27;1113&#x27; b=&#x27;1932&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1133&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1113&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1930&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1907&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1145&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1136&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1929&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1895&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1175&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1148&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1939&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1896&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1175&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1148&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1939&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1896&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1189&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1179&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1929&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1899&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1189&#x27; t=&#x27;1895&#x27; r=&#x27;1217&#x27; b=&#x27;1931&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1234&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1217&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1930&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1898&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1256&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1239&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1931&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1908&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1277&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1260&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1930&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1901&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; S
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1277&#x27; t=&#x27;1898&#x27; r=&#x27;1315&#x27; b=&#x27;1931&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1325&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1315&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1930&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1900&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1339&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1326&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1930&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1907&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1359&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1341&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1931&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1898&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ä
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1381&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1362&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1931&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1907&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1411&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1384&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1931&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1908&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1427&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1415&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1931&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1909&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1448&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1430&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1932&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1908&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1469&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1452&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1933&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1900&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1486&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1473&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1932&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1910&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;1979&#x27; l=&#x27;263&#x27; t=&#x27;1937&#x27; r=&#x27;1487&#x27; b=&#x27;1994&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;284&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;263&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1981&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1958&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;306&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;286&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1983&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1959&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;326&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;309&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1983&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1951&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;326&#x27; t=&#x27;1942&#x27; r=&#x27;356&#x27; b=&#x27;1983&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;375&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;356&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1992&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1957&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; p
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;394&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;377&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1991&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1949&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;406&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;399&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1982&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1947&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;419&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;409&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1982&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1948&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;437&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;422&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1981&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1957&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;454&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;440&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1990&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1946&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;468&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;452&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1980&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1956&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;488&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;470&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1987&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1953&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; p
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;508&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;492&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1987&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1944&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;520&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;512&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1977&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1942&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;535&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;522&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1976&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1952&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;550&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;538&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1975&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1952&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;571&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;554&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1974&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1951&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;590&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;574&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1974&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1942&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;607&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;595&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1973&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;1949&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;607&#x27; t=&#x27;1937&#x27; r=&#x27;631&#x27; b=&#x27;1980&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;664&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;631&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1971&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1937&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; D
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;681&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;669&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1970&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1947&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;703&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;684&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1970&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1946&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;715&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;706&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1970&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1944&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;730&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;718&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1980&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1939&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;756&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;729&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1979&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1938&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;756&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;729&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1979&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1938&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;770&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;760&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1972&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1940&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;789&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;772&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1973&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1949&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;811&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;792&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1974&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1950&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;831&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;814&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1975&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1943&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;831&#x27; t=&#x27;1937&#x27; r=&#x27;855&#x27; b=&#x27;1979&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;865&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;855&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1977&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1943&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;896&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;868&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1978&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1955&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;896&#x27; t=&#x27;1943&#x27; r=&#x27;929&#x27; b=&#x27;1979&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;956&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;929&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1981&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1947&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; A
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;978&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;959&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1981&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1957&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;998&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;982&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1992&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1958&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1015&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1002&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1984&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1961&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1037&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1018&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1990&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1950&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1037&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1018&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1990&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1950&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1067&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1039&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1993&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1952&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1067&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1039&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1993&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1952&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1081&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1071&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1983&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1954&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1081&#x27; t=&#x27;1947&#x27; r=&#x27;1104&#x27; b=&#x27;1983&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1120&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1104&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1983&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1952&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1137&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1124&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1982&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1959&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1157&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1140&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1982&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1953&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1157&#x27; t=&#x27;1950&#x27; r=&#x27;1182&#x27; b=&#x27;1983&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1200&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1182&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1994&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1960&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1215&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1202&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1983&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1960&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1234&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1219&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1983&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1960&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;85&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1254&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1237&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1991&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1950&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ß
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1272&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1259&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1983&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1960&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;83&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1294&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1273&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1983&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1959&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1294&#x27; t=&#x27;1950&#x27; r=&#x27;1309&#x27; b=&#x27;1994&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;18&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1343&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1309&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1990&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1950&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; F
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1358&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1346&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1984&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1961&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1369&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1361&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1985&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1952&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1389&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1373&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1985&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1953&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1409&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1392&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1994&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1951&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1426&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1413&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1985&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1962&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1441&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1428&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1985&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1962&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1456&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1443&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1985&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1962&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1478&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1458&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1986&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1962&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1487&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1479&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;1994&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1979&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;2031&#x27; l=&#x27;264&#x27; t=&#x27;1991&#x27; r=&#x27;1487&#x27; b=&#x27;2045&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;282&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;264&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2030&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2000&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;297&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;286&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2031&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2010&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;315&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;303&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2040&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2000&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;325&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;312&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2042&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2001&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;336&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;324&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2011&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;359&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;338&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2034&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2011&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;359&#x27; t=&#x27;2000&#x27; r=&#x27;383&#x27; b=&#x27;2042&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;400&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;383&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2044&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2002&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;422&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;405&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2035&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2012&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;437&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;425&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2010&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;450&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;440&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2003&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;465&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;452&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2032&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2009&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;465&#x27; t=&#x27;1991&#x27; r=&#x27;489&#x27; b=&#x27;2038&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;531&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;489&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2032&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1996&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; M
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;544&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;535&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2029&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1993&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;562&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;546&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2038&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1995&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ß
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;582&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;565&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2038&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1993&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;603&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;586&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2028&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2003&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;624&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;606&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2026&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2002&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;644&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;627&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2026&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1994&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;656&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;647&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2024&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1991&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;76&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;668&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;659&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2025&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2000&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;700&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;671&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2024&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2000&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;720&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;704&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2034&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2001&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;735&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;724&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2023&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2000&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;759&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;738&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2023&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1999&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;759&#x27; t=&#x27;1991&#x27; r=&#x27;783&#x27; b=&#x27;2031&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;795&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;783&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2024&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2003&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;817&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;800&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2025&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1997&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;817&#x27; t=&#x27;1997&#x27; r=&#x27;841&#x27; b=&#x27;2031&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;861&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;841&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2027&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2004&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;881&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;866&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2029&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2007&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;913&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;885&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2040&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1999&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;913&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;885&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2040&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1999&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;913&#x27; t=&#x27;1998&#x27; r=&#x27;938&#x27; b=&#x27;2034&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;947&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;938&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2033&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;1998&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;978&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;951&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2034&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2010&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;12&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;978&#x27; t=&#x27;1998&#x27; r=&#x27;1004&#x27; b=&#x27;2034&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1017&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1004&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2045&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2004&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1030&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1017&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2013&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1040&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1033&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2001&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1058&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1043&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2045&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2002&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1083&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1055&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2042&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2002&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1083&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1055&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2042&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2002&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1099&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1087&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2035&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2012&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1122&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1101&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2034&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2011&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1122&#x27; t=&#x27;2001&#x27; r=&#x27;1134&#x27; b=&#x27;2036&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1162&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1134&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2001&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; A
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1184&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1164&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2012&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1202&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1186&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2005&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1219&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1206&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2013&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1241&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1222&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2012&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1255&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1243&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2003&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1269&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1257&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2013&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1292&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1272&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2035&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2012&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1292&#x27; t=&#x27;2001&#x27; r=&#x27;1315&#x27; b=&#x27;2036&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1333&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1315&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2045&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2004&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1355&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1338&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2037&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2013&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1368&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1358&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2007&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1380&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1371&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2037&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2007&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1395&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1382&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2036&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2014&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1403&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1396&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2045&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2030&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1403&#x27; t=&#x27;2004&#x27; r=&#x27;1428&#x27; b=&#x27;2038&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1447&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1428&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2038&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2014&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1464&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1451&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2038&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2016&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1487&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1467&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2038&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2015&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;2084&#x27; l=&#x27;264&#x27; t=&#x27;2041&#x27; r=&#x27;1487&#x27; b=&#x27;2101&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;285&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;264&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2079&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2056&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;306&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;288&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2081&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2058&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;326&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;309&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2082&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2052&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;326&#x27; t=&#x27;2052&#x27; r=&#x27;357&#x27; b=&#x27;2084&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;375&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;357&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2084&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2062&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;397&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;379&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2085&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2062&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;414&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;401&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2094&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2054&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;414&#x27; t=&#x27;2051&#x27; r=&#x27;445&#x27; b=&#x27;2086&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;458&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;445&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2086&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2063&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;470&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;463&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2086&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2051&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;492&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;473&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2085&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2061&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;507&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;495&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2084&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2061&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;507&#x27; t=&#x27;2041&#x27; r=&#x27;537&#x27; b=&#x27;2086&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;554&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;537&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2082&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2056&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; v
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;574&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;559&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2082&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2058&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;587&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;578&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2082&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2048&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;601&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;589&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2081&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2047&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; k
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;619&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;603&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2081&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2050&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;632&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;623&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2080&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2049&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;18&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;673&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;634&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2090&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2045&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ü
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;704&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;677&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2078&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2054&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;716&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;707&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2077&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2044&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;728&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;719&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2077&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2041&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;758&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;730&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2086&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2042&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;758&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;730&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2086&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2042&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;774&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;762&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2076&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2053&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;774&#x27; t=&#x27;2041&#x27; r=&#x27;806&#x27; b=&#x27;2090&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;848&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;806&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2079&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2044&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; W
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;864&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;851&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2079&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2057&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;876&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;867&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2081&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2046&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;892&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;878&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2090&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2049&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;903&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;890&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2082&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2059&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;903&#x27; t=&#x27;2044&#x27; r=&#x27;940&#x27; b=&#x27;2090&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;957&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;940&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2087&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2062&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;973&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;959&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2087&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2063&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;993&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;976&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2098&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2065&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1014&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;997&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2088&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2064&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1036&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1017&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2088&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2065&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1049&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1039&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2088&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2053&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1070&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1051&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2095&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2053&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1070&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1051&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2095&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2053&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1085&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1072&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2087&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2064&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1097&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1088&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2086&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2056&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1113&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1107&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2087&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2055&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; !
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1113&#x27; t=&#x27;2053&#x27; r=&#x27;1160&#x27; b=&#x27;2097&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1189&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1160&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2089&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2054&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; U
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1209&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1190&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2089&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2065&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1225&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1212&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2097&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2055&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1240&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1224&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2088&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2065&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1252&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1240&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2088&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2065&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1268&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1255&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2088&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2066&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1268&#x27; t=&#x27;2054&#x27; r=&#x27;1291&#x27; b=&#x27;2097&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1301&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1291&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2096&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2055&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; j
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1316&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1304&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2089&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2066&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1337&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1320&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2098&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2060&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1337&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1320&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2098&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2060&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1349&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1340&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2089&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2055&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1369&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1352&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2100&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2067&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1385&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1372&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2089&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2067&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1385&#x27; t=&#x27;2055&#x27; r=&#x27;1408&#x27; b=&#x27;2100&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1427&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1408&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2090&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2056&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; L
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1448&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1431&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2090&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2067&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1469&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1452&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2101&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2068&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1487&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1473&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2091&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2068&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;2137&#x27; l=&#x27;266&#x27; t=&#x27;2096&#x27; r=&#x27;1487&#x27; b=&#x27;2151&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;276&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;266&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2134&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2097&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;293&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;279&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2142&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2099&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;300&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;289&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2132&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2102&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;300&#x27; t=&#x27;2097&#x27; r=&#x27;324&#x27; b=&#x27;2137&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;342&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;324&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2132&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2109&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;361&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;345&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2132&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2101&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;378&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;365&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2134&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2112&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;394&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;381&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2134&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2112&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;394&#x27; t=&#x27;2101&#x27; r=&#x27;418&#x27; b=&#x27;2137&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;438&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;418&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2136&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2113&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;457&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;442&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2137&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2115&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;489&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;461&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2147&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2105&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;489&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;461&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2147&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2105&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;489&#x27; t=&#x27;2100&#x27; r=&#x27;514&#x27; b=&#x27;2147&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;528&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;514&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2145&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2104&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;553&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;526&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2147&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2103&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;553&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;526&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2147&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2103&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;565&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;557&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2136&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2104&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;577&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;569&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2136&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2100&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;607&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;581&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2135&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2111&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;637&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;611&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2134&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2109&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;653&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;641&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2133&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2109&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;669&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;656&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2132&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2108&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;669&#x27; t=&#x27;2096&#x27; r=&#x27;703&#x27; b=&#x27;2137&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;721&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;703&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2131&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2107&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;734&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;724&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2131&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2096&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;753&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;737&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2129&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2101&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;753&#x27; t=&#x27;2096&#x27; r=&#x27;778&#x27; b=&#x27;2137&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;794&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;778&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2129&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2096&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;817&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;799&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2129&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2105&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;848&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;820&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2130&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2107&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;868&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;851&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2132&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2109&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;881&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;872&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2133&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2102&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;901&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;884&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2133&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2105&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;910&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;904&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2135&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2129&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;910&#x27; t=&#x27;2096&#x27; r=&#x27;959&#x27; b=&#x27;2137&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1001&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;959&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2140&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2106&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; W
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1013&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1004&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2140&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2105&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1028&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1015&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2140&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2116&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1028&#x27; t=&#x27;2105&#x27; r=&#x27;1053&#x27; b=&#x27;2140&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1068&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1053&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2147&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2105&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1073&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1063&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2138&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2109&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1088&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1076&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2139&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2116&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1109&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1091&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2149&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2105&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1125&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1113&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2140&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2117&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1149&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1129&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2141&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2117&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1149&#x27; t=&#x27;2105&#x27; r=&#x27;1178&#x27; b=&#x27;2149&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1198&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1178&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2141&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2117&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1211&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1201&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2141&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2106&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1241&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1213&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2151&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2107&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1241&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1213&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2151&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2107&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1255&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1245&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2142&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2112&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1255&#x27; t=&#x27;2106&#x27; r=&#x27;1284&#x27; b=&#x27;2151&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1312&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1284&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2142&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2118&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1328&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1316&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2142&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2120&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1348&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1330&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2151&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2109&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1365&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1351&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2142&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2119&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1365&#x27; t=&#x27;2107&#x27; r=&#x27;1394&#x27; b=&#x27;2144&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1406&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1394&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2142&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2119&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1417&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1409&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2142&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2107&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1440&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1420&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2144&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2119&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1455&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1443&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2144&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2121&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;35&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1487&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1458&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2143&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2121&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;2189&#x27; l=&#x27;269&#x27; t=&#x27;2148&#x27; r=&#x27;1484&#x27; b=&#x27;2203&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;286&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;269&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2200&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2166&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;300&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;289&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2187&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2164&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;316&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;304&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2197&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2151&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;342&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;315&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2193&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2151&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;342&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;315&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2193&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2151&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;354&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;346&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2185&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2152&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;375&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;358&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2184&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2161&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;395&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;379&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2195&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2162&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;412&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;400&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2185&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2163&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;433&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;414&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2186&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2162&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;449&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;437&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2186&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2164&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;473&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;452&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2187&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2164&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;8&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;473&#x27; t=&#x27;2148&#x27; r=&#x27;497&#x27; b=&#x27;2194&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;530&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;497&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2189&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2156&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; S
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;549&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;534&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2189&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2167&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;561&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;552&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2190&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2157&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;580&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;564&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2190&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2159&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;602&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;585&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2190&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2166&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;614&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;605&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2188&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2159&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;629&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;617&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2188&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2165&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;651&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;632&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2187&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2163&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;80&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;673&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;661&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2181&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2169&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; -
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;715&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;684&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2187&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2151&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; K
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;735&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;718&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2186&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2161&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;748&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;738&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2184&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2148&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;763&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;750&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2194&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2148&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;773&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;762&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2183&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2159&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;789&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;776&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2182&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2158&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;797&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;791&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2190&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2175&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;797&#x27; t=&#x27;2148&#x27; r=&#x27;822&#x27; b=&#x27;2194&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;838&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;822&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2183&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2150&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;855&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;843&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2183&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2161&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;871&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;858&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2193&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2151&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;882&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;868&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2194&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2152&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;893&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;881&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2186&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2164&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;916&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;896&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2188&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2164&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;916&#x27; t=&#x27;2150&#x27; r=&#x27;940&#x27; b=&#x27;2194&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;958&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;940&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2201&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2168&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;9&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;969&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;960&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2193&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2160&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;9&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;990&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;973&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2193&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2160&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;9&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ä
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1011&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;992&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2193&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2170&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;9&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1025&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1014&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2201&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2169&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;9&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1040&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1028&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2192&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2169&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;9&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1062&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1043&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2192&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2168&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;9&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1081&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1064&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2193&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2160&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;9&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1097&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1085&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2192&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2170&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;9&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1114&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1100&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2193&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2169&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;9&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1114&#x27; t=&#x27;2159&#x27; r=&#x27;1139&#x27; b=&#x27;2194&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1172&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1139&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2194&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2159&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; S
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1185&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1175&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2193&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2164&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1200&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1187&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2193&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2171&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1215&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1202&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2193&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2170&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1237&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1218&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2194&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2170&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1237&#x27; t=&#x27;2159&#x27; r=&#x27;1262&#x27; b=&#x27;2194&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1280&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1262&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2195&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2171&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1302&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1284&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2194&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2170&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1319&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1305&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2203&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2161&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1319&#x27; t=&#x27;2161&#x27; r=&#x27;1339&#x27; b=&#x27;2195&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1356&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1339&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2195&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2163&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1372&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1360&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2194&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2172&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1395&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1375&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2195&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2171&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1395&#x27; t=&#x27;2162&#x27; r=&#x27;1419&#x27; b=&#x27;2195&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1444&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1419&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2203&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2162&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; F
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1459&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1447&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2197&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2173&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;81&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1470&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1462&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2193&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2165&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;29&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1484&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1470&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2187&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2176&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ¬
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;2242&#x27; l=&#x27;268&#x27; t=&#x27;2203&#x27; r=&#x27;1485&#x27; b=&#x27;2259&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;285&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;268&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2244&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2213&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;301&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;289&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2243&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2220&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;316&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;303&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2242&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2218&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;338&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;318&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2242&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2216&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;14&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;338&#x27; t=&#x27;2212&#x27; r=&#x27;360&#x27; b=&#x27;2242&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;377&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;360&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2238&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2212&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; v
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;397&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;383&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2237&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2213&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;420&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;401&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2236&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2213&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;3&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;420&#x27; t=&#x27;2204&#x27; r=&#x27;444&#x27; b=&#x27;2242&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;464&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;444&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2237&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2204&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; L
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;479&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;467&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2237&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2215&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;491&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;482&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2238&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2205&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;510&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;493&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2246&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2213&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; p
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;524&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;514&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2246&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2217&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;536&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;527&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2239&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2206&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;556&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;539&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2250&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2218&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;556&#x27; t=&#x27;2204&#x27; r=&#x27;585&#x27; b=&#x27;2242&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;606&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;585&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2241&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2217&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;627&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;608&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2241&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2217&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;647&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;630&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2241&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2209&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;647&#x27; t=&#x27;2203&#x27; r=&#x27;673&#x27; b=&#x27;2242&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;714&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;673&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2240&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2206&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; W
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;734&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;718&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2239&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ä
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;748&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;738&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2237&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2207&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;762&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;751&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2236&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2213&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;778&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;765&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2236&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2213&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;789&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;780&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2236&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2203&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;807&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;794&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2235&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;827&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;813&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2235&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;827&#x27; t=&#x27;2203&#x27; r=&#x27;852&#x27; b=&#x27;2242&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;870&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;852&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2237&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2210&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; v
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;886&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;875&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2238&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2216&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;901&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;889&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2240&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2216&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;920&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;905&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2241&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2208&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;933&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;924&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2242&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;945&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;936&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2243&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2208&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;975&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;948&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2253&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;975&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;948&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2253&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;992&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;980&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2245&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2222&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1015&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;995&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2245&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2220&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1015&#x27; t=&#x27;2208&#x27; r=&#x27;1039&#x27; b=&#x27;2253&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1048&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1039&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2244&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2210&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1066&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1051&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2252&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2211&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1072&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1062&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2245&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2215&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1087&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1080&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2254&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2239&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1087&#x27; t=&#x27;2210&#x27; r=&#x27;1124&#x27; b=&#x27;2254&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1138&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1124&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2255&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2213&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1152&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1137&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2247&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2223&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1174&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1156&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2246&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2222&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1194&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1178&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2246&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2214&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1210&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1198&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2246&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2223&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1226&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1213&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2245&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2222&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1248&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1228&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2246&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2222&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1248&#x27; t=&#x27;2213&#x27; r=&#x27;1279&#x27; b=&#x27;2247&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1296&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1279&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2247&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2214&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1313&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1300&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2247&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2223&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1345&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1316&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2247&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2223&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1345&#x27; t=&#x27;2214&#x27; r=&#x27;1377&#x27; b=&#x27;2247&#x27; characterHeight=&#x27;23&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1394&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1377&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2259&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2225&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;34&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1415&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1398&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2249&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2225&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;34&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1425&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1418&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2241&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2226&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;34&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; &#x27;
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1432&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1426&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2243&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2226&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;34&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; -
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1445&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1435&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2252&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2221&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;34&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1462&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1449&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2244&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2222&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;34&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1471&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1464&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2245&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2221&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;34&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;87&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1485&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1474&#x27; characterHeight=&#x27;23&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2245&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2224&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;34&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;2296&#x27; l=&#x27;268&#x27; t=&#x27;2253&#x27; r=&#x27;1480&#x27; b=&#x27;2308&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;311&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;268&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2298&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2262&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; W
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;326&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;314&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2297&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2273&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;344&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;329&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2305&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2260&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;349&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;338&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2264&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;363&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;352&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2295&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2271&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;385&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;366&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2294&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2268&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;385&#x27; t=&#x27;2258&#x27; r=&#x27;410&#x27; b=&#x27;2301&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;426&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;410&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2301&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2267&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;442&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;430&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2290&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2266&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;462&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;446&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2301&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2267&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; g
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;479&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;467&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2290&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2267&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;500&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;482&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2290&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2266&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;522&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;504&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2290&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2258&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ü
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;540&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;526&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2289&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2258&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;557&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;545&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2291&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2268&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;574&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;561&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2291&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2268&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;581&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;575&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2299&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2285&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ,
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;581&#x27; t=&#x27;2258&#x27; r=&#x27;607&#x27; b=&#x27;2296&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;624&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;607&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2292&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2261&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;640&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;628&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2292&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2269&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;657&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;643&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2292&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2269&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;657&#x27; t=&#x27;2257&#x27; r=&#x27;680&#x27; b=&#x27;2296&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;689&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;680&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2292&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2257&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;719&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;693&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2291&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2267&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; m
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;719&#x27; t=&#x27;2253&#x27; r=&#x27;742&#x27; b=&#x27;2296&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;774&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;742&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2289&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2255&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; B
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;790&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;778&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2288&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2265&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;806&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;793&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2254&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;811&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;804&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2288&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2253&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;831&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;815&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2258&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;831&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;815&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2258&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams l=&#x27;831&#x27; t=&#x27;2253&#x27; r=&#x27;858&#x27; b=&#x27;2296&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;870&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;858&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2289&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2266&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;882&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;874&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2290&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2256&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;903&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;885&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2292&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2267&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;919&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;907&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2293&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2271&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;940&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;924&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2295&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2265&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;4&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;940&#x27; t=&#x27;2256&#x27; r=&#x27;966&#x27; b=&#x27;2296&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;982&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;966&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2297&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2264&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ö
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1010&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;986&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2306&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2262&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1010&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;986&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2306&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2262&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1021&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1009&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2273&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1042&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1024&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2298&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2273&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1056&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1046&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2297&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2268&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1066&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1057&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2298&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2265&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1078&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1069&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2298&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2263&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1108&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1081&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2308&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2265&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1108&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1081&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2308&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2265&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1125&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1112&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2300&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2276&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1148&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1128&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2300&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2275&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1148&#x27; t=&#x27;2262&#x27; r=&#x27;1173&#x27; b=&#x27;2302&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1206&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1173&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2300&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2264&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;15&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; S
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1218&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1209&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2300&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2269&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;15&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1237&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1221&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2300&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2276&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;15&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1259&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1242&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2300&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2276&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;15&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1272&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1263&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2300&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2269&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;15&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;80&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1291&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1275&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2300&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2269&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;15&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1305&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1295&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2301&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2266&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;15&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1320&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1307&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2300&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2276&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;15&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1340&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1324&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2301&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2265&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;15&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1356&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1345&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2302&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2277&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;15&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1378&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1360&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2302&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2277&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;15&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;18&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1393&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1379&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2295&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2273&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;15&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ^
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1393&#x27; t=&#x27;2264&#x27; r=&#x27;1418&#x27; b=&#x27;2297&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1437&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1418&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2271&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;7&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1459&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1440&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2296&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2272&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;7&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1480&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1462&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2297&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2267&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;7&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; d
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;2347&#x27; l=&#x27;268&#x27; t=&#x27;2306&#x27; r=&#x27;1482&#x27; b=&#x27;2363&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;10.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;281&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;268&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2350&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2327&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;293&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;285&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2350&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2315&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;314&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;295&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2351&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2326&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;330&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;318&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2350&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2327&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;346&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;333&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2350&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2326&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;346&#x27; t=&#x27;2309&#x27; r=&#x27;376&#x27; b=&#x27;2348&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;395&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;376&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2348&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2324&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;416&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;399&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2347&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2322&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;428&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;419&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2346&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2315&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;438&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;431&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2346&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2309&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;456&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;443&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2344&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2320&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;478&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;461&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2344&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2319&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;500&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;483&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2344&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2319&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;513&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;503&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2344&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2310&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; l
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;527&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;515&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2342&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2319&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;550&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;531&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2343&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2318&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;10&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;550&#x27; t=&#x27;2306&#x27; r=&#x27;580&#x27; b=&#x27;2348&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;85&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;610&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;580&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2344&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2308&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; R
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;626&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;614&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2342&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2320&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;646&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;629&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2348&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2318&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; p
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;661&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;649&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2344&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2320&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;681&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;665&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2344&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2311&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; ä
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;698&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;685&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2353&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2310&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; s
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;80&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;708&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;697&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2343&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2321&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;730&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;712&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2343&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2318&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;742&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;733&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2342&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2312&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;762&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;745&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2342&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2318&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;774&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;765&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2341&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2312&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;785&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;777&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2341&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2306&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;803&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;789&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2341&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2317&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; o
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;826&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;806&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2341&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2316&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;11&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;826&#x27; t=&#x27;2306&#x27; r=&#x27;858&#x27; b=&#x27;2347&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;871&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;858&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2342&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2318&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;882&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;876&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2342&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2307&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; i
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;904&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;886&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2344&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2320&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;921&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;908&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2345&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2323&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;2&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;921&#x27; t=&#x27;2307&#x27; r=&#x27;953&#x27; b=&#x27;2347&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;972&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;953&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2348&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2324&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;995&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;975&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2349&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2324&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1013&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;998&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2350&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2316&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1030&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1018&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2350&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2327&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1045&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1034&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2350&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2327&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1061&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1049&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2351&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2328&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1092&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1064&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2361&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2318&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1092&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1064&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2361&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2318&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; h
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1108&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1096&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2351&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2328&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1130&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1112&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2352&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2328&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1150&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1134&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2353&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2319&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; b
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1172&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1154&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2353&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2329&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1187&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1175&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2353&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2329&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1204&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1190&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2353&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2329&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1204&#x27; t=&#x27;2316&#x27; r=&#x27;1235&#x27; b=&#x27;2361&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1267&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1235&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2354&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2319&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; K
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1282&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1268&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2353&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2330&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1302&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1285&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2353&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2330&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; a
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1320&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1306&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2363&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2320&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; f
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1328&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1319&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2354&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2324&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1328&#x27; t=&#x27;2319&#x27; r=&#x27;1352&#x27; b=&#x27;2355&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;92&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1363&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1352&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2355&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2324&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;17&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; z
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1386&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1366&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2348&#x27; wordFromDictionary=&#x27;true&#x27; t=&#x27;2324&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;17&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;1386&#x27; t=&#x27;2319&#x27; r=&#x27;1418&#x27; b=&#x27;2349&#x27; characterHeight=&#x27;24&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1430&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1418&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2348&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2324&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;98&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1453&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1433&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2349&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2324&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;100&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1466&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1456&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2348&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2319&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; t
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;30&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1482&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1468&#x27; characterHeight=&#x27;24&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2344&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2332&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;1&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; -
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; &lt;/par&gt;
;;; 
;;; 
;;; &lt;par leftIndent=&#x27;47&#x27;&gt;
;;; 
;;; 
;;; &lt;line baseline=&#x27;2392&#x27; l=&#x27;306&#x27; t=&#x27;2363&#x27; r=&#x27;1317&#x27; b=&#x27;2400&#x27;&gt;
;;; &lt;formatting lang=&#x27;OldGerman&#x27; ff=&#x27;Arial&#x27; fs=&#x27;8.&#x27;&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;80&#x27; meanStrokeWidth=&#x27;28&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;328&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;306&#x27; characterHeight=&#x27;17&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2393&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2368&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; Ä
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;90&#x27; meanStrokeWidth=&#x27;28&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;342&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;332&#x27; characterHeight=&#x27;17&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2394&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2378&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; r
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;28&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;354&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;346&#x27; characterHeight=&#x27;17&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2395&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2377&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; c
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;80&#x27; meanStrokeWidth=&#x27;28&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;369&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;356&#x27; characterHeight=&#x27;17&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2394&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2377&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; u
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;65&#x27; meanStrokeWidth=&#x27;28&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;422&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;373&#x27; characterHeight=&#x27;17&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2400&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2369&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; M
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;28&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;433&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;425&#x27; characterHeight=&#x27;17&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2393&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2375&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; e
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;96&#x27; meanStrokeWidth=&#x27;28&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;449&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;435&#x27; characterHeight=&#x27;17&#x27; wordNormal=&#x27;true&#x27; b=&#x27;2392&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2374&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;6&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; n
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;449&#x27; t=&#x27;2363&#x27; r=&#x27;564&#x27; b=&#x27;2392&#x27; characterHeight=&#x27;17&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;89&#x27; meanStrokeWidth=&#x27;28&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;571&#x27; wordIdentifier=&#x27;true&#x27; serifProbability=&#x27;255&#x27; l=&#x27;564&#x27; characterHeight=&#x27;17&#x27; wordNormal=&#x27;false&#x27; b=&#x27;2386&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2363&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; &gt;
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;28&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;582&#x27; wordIdentifier=&#x27;true&#x27; serifProbability=&#x27;255&#x27; l=&#x27;576&#x27; characterHeight=&#x27;17&#x27; wordNormal=&#x27;false&#x27; b=&#x27;2385&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2380&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;0&#x27; wordNumeric=&#x27;false&#x27;&gt;
;;; -
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams l=&#x27;582&#x27; t=&#x27;2363&#x27; r=&#x27;1299&#x27; b=&#x27;2392&#x27; characterHeight=&#x27;17&#x27; hasUncertainHeight=&#x27;false&#x27; baseLine=&#x27;0&#x27;&gt;
;;;  
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;94&#x27; meanStrokeWidth=&#x27;28&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1303&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1299&#x27; characterHeight=&#x27;17&#x27; wordNormal=&#x27;false&#x27; b=&#x27;2397&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2395&#x27; wordStart=&#x27;true&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;true&#x27;&gt;
;;; .
;;; &lt;/charParams&gt;
;;; 
;;; 
;;; &lt;charParams suspicious=&#x27;true&#x27; baseLine=&#x27;0&#x27; charConfidence=&#x27;18&#x27; meanStrokeWidth=&#x27;28&#x27; hasUncertainHeight=&#x27;false&#x27; r=&#x27;1317&#x27; wordIdentifier=&#x27;false&#x27; serifProbability=&#x27;255&#x27; l=&#x27;1304&#x27; characterHeight=&#x27;17&#x27; wordNormal=&#x27;false&#x27; b=&#x27;2398&#x27; wordFromDictionary=&#x27;false&#x27; t=&#x27;2373&#x27; wordStart=&#x27;false&#x27; wordPenalty=&#x27;5&#x27; wordNumeric=&#x27;true&#x27;&gt;
;;; 4
;;; &lt;/charParams&gt;
;;; &lt;/formatting&gt;
;;; &lt;/line&gt;
;;; &lt;/par&gt;
;;; 
;;; 
;;; &lt;/text&gt;
;;; 
;;; 
;;; &lt;/block&gt;
;;; 
;;; 
;;; &lt;/page&gt;
;;; 
;;; 
;;; &lt;/document&gt;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;&quot;</span>","value":"\"\""}
;; <=

;; **
;;; #Difficult words
;; **

;; @@
(def difficult (filter #(not= % (->> %
                                    (transform dta)
                                    first
                                    :word))
                       (keys (:dict dta))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.dict/difficult</span>","value":"#'suub.bote.dict/difficult"}
;; <=

;; @@
(table/table-view (take 20 (filter second
                                   (map (fn [w] (let [r (take 10 (transform dta w))]
                                          [w
                                           (:word (first r))
                                           (plot/bar-chart
                                             (map :word r)
                                             (map :prob r)
                                             :plot-size 600)]))
                                difficult))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=
