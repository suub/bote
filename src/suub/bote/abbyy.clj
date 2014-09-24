;; gorilla-repl.fileformat = 1

;; **
;;; #Abbyy
;;; OCR XML file reading and writing.
;; **

;; @@
(ns suub.bote.abbyy
  (:require [taoensso.timbre :refer [spy debug info error]]
            [clojure.java.io :as io]
            [clojure.core.reducers :as r]
            [clojure.xml :as xml]
            [suub.bote.util :as util]
            [clojure.zip :as zip]
            [me.raynes.laser :as l]
            [pandect.core :as hash]
            [me.raynes.fs :as fs]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ##Reading XML files.
;; **

;; @@
(defn parse
  "Takes a xml string or file and returns its hicory parse."
  [p]
  (l/parse p :parser :xml))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/parse</span>","value":"#'suub.bote.abbyy/parse"}
;; <=

;; **
;;; ##Extracting information.
;; **

;; @@
(defn page-info
  "Extracts page metadata."
  [node]
  (let [loc (l/zip node)
        pge (-> loc
                (l/select (l/element= :page))
                first
                :attrs)]
    {:height (-> pge :height Integer.)
     :width  (-> pge :width Integer.)
     :skew   (-> loc
                 (l/select (l/attr? :skewangle))
                 first
                 :attrs
                 :skewangle
                 Double.)}))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/page-info</span>","value":"#'suub.bote.abbyy/page-info"}
;; <=

;; @@
(defn zone
  "Calculates the zone around the given page element.
   This is used to uniquely identify page elements across multiple systems."
  [node]
  (when (every? (:attrs node) [:l :t :r :b])
    (let [l (-> node :attrs :l Integer.)
          t (-> node :attrs :t Integer.)
          r (-> node :attrs :r Integer.)
          b (-> node :attrs :b Integer.)]
    {:l l
     :t t
     :r r
     :b b})))

(defn zone?
  "A laser selector for selecting all nodes that have a zone."
  [zone]
  (fn [loc] (-> loc zip/node zone boolean)))

(defn zone=
  "A laser selector for selecting a node by its zone."
  [zone]
  (fn [loc] (= zone (-> loc zip/node zone))))

(defn zone-in
  "Returns a laser selector that selects elements that are
   withing the provided collection of zones."
  [zones]
  (fn [loc] (boolean ((set zones) (-> loc zip/node zone)))))

(defn convex-hull [& args]
  {:l (apply min (map :l args))
   :t (apply min (map :t args))
   :r (apply max (map :r args))
   :b (apply max (map :b args))})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/convex-hull</span>","value":"#'suub.bote.abbyy/convex-hull"}
;; <=

;; @@
(defn characters
  "Extracts all character zones with their relevant data."
  [node]
  (for [node (l/select (l/zip node) (l/element= :charparams))
        :let [z (zone node)
              s (-> node :content first)
              x (-> node :attrs :l Integer.)
              y (-> node :attrs :t Integer.)
              w (- (-> node :attrs :r Integer.) x)
              h (- (-> node :attrs :b Integer.) y)]
        c s]
    {:zone z
     :char c
     :dim
     {:x x
      :y y
      :width w
      :height h}}))

(defn lines [node]
  (map characters
       (l/select (l/zip node) (l/element= :line))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/lines</span>","value":"#'suub.bote.abbyy/lines"}
;; <=

;; @@
(def alphabet (set "abcdefghijklmnopqrstuvwxyzäöüßABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ"))
(def linewrap (set "-¬"))

(defn remove-linewrap [lines]
  (->> lines
       (map #(partition-by
               (comp boolean alphabet :char)
               %))
       (reduce (fn
                 ([] nil)
                 ([words line]
                  (let [[lst sep] (take-last 2 words)]
                    (if (and (= 1 (count sep))
                             (-> sep first :char linewrap)
                             (-> lst first :char alphabet)
                             (-> line ffirst :char alphabet))
                      (concat (drop-last 2 words)
                              [(concat lst (first line))]
                              (rest line))
                      (concat words line))))))
       (filter #(-> % first :char alphabet))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/remove-linewrap</span>","value":"#'suub.bote.abbyy/remove-linewrap"}
;; <=

;; **
;;; ##Reintegrating data into XML documents.
;; **

;; @@
(defn change [corr page]
  (let [expansions
        (into {} (for [{f :from t :to} corr]
                   [(:zone (first f))
                    {:content [t]
                     :zone (apply convex-hull (map :zone f))}]))
        removal
        (into #{} (for [{f :from t :to} corr
                        r (rest f)]
                    (:zone r)))]
    (l/at page
      (l/element= :charparams)
      #(let [z (zone %)]
         (if-let [e (expansions z)]
           (-> %
               (assoc :content (:content e))
               (update-in [:attrs] merge (:zone e)))
           (when (not (removal z))
             %))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/change</span>","value":"#'suub.bote.abbyy/change"}
;; <=

;; @@
(defn emit [page]
  (xml/emit page))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/emit</span>","value":"#'suub.bote.abbyy/emit"}
;; <=

;; **
;;;  ##Correction
;; **

;; @@
(defn abbyy-matcher [matcher query]
  (loop [match []
         mrest (seq matcher)
         qrest (seq query)]
    (cond (empty? mrest)
            [match qrest]
          (= (first mrest)
             (:char (first qrest)))
            (recur (conj match (first qrest))
                   (rest mrest)
                   (rest qrest)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/abbyy-matcher</span>","value":"#'suub.bote.abbyy/abbyy-matcher"}
;; <=
