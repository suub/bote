(ns suub.bote.abbyy
  (:require [taoensso.timbre :refer [spy debug info error]]
            [suub.bote.util :as util]
            [clojure.zip :as zip]
            [me.raynes.laser :as l]
            [midje.sweet :refer :all]
            [pandect.core :as hash]))

(defn parse
  "Takes a xml file and returns its parse zipper."
  [f]
  (l/parse f :xml))


(defn zone
  "Calculates the zone around the given page element.
   This is used to uniquely identify page elements across multiple systems."
  [node]
  (let [l (-> node :attrs :l Integer.)
        t (-> node :attrs :t Integer.)
        r (-> node :attrs :r Integer.)
        b (-> node :attrs :b Integer.)]
    #{[l t] [l b] [r t] [r b]}))

(defn zone=
  "A laser selector for selecting a node by its zone."
  [zone]
  (fn [loc] (= zone (-> loc zip/node zone))))

(defn zone-in
  "Returns a laser selector that selects elements that are
   withing the provided collection of zones."
  [zones]
  (fn [loc] (boolean (((set zones) (-> loc zip/node zone))))))

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

(defn character
  "Extracts all character zones with their relevant data."
  [node]
  (for [node (l/select (l/zip node) (l/element= :charparams))]
    [(zone node)
     (-> node :content first)
     (let [x (-> node :attrs :l Integer.)
           y (-> node :attrs :t Integer.)
           w (- (-> node :attrs :r Integer.) x)
           h (- (-> node :attrs :b Integer.) y)]
       {:x x :y y :width w :height h})]))

(defn lines [loc]
  (for [line (l/select-locs loc (l/element= :line))]
    (map zone (l/select line (l/element= :charparams)))))


(def alphabet (set (map str "abcdefghijklmnopqrstuvwxyzäöüABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ")))
(def linewrap (set (map str "-¬")))

(defn words [node]
  (->> (l/select (l/zip node) (l/element= :line))
       (map (fn [line] (l/select (l/zip line) (l/element= :charparams))))
       (map (fn [line]
              (partition-by (comp alphabet first :content)
                            line)))
       (reduce (fn [words line]
                 (let [[lst sep] (take-last 2 words)]
                   (if (and (= 1 (count sep))
                            (-> sep first :content first count (= 1))
                            (-> sep first :content first linewrap)
                            (-> lst first :content first alphabet)
                            (-> line first first :content first alphabet))
                     (concat (drop-last 2 words) [(concat lst (first line))] (rest line))
                     (concat words line)))))
       (filter (fn [word] (-> word first :content first alphabet)))
       (map (fn [word] [(apply str (map #(-> % :content first) word)) (map zone word)]))))


(defn abbyy-matcher [m q]
  nil)

(fact (abbyy-matcher "he"[{} {} {} {}]))

(defn unzap [zp]
  (let [{:keys [origin zones structures]} zp]
    (as-> origin d
          (reduce
           (fn [loc [zone {:keys [content]}]]
             (l/at loc
                   (zone= zone)
                   (l/content content)))
           d zones)
          (l/at d
                (l/element= :charparams)
                (l/attr :wordStart nil))
          (reduce
           (fn [loc [zone {:keys [content]}]]
             (l/at loc
                   (zone= zone)
                   (l/content content)))
           d
           (:words structures))
          (l/at d
                (l/negate (zone-in (keys zones)))
                (l/remove)))))
