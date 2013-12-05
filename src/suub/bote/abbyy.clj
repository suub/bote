(ns suub.bote.abbyy
  (:require [taoensso.timbre :refer [spy debug info error]]
            [cascalog.api :refer :all]
            [cascalog.cascading.io :as io]
            [cascalog.more-taps :refer [hfs-wholefile]]
            [clojure.zip :as zip]
            [me.raynes.laser :as l]
            [midje.sweet :refer :all]
            [midje.cascalog :refer [produces]]
            [pandect.core :as hash]))

(defmapfn filename-components [filename]
  (let [[_ name ext] (re-matches #"(.*)\.(.*)" filename)]
    [name ext]))

(fact
 (filename-components "hel.lo.txt")
 => ["hel.lo" "txt"])

(defmapfn parse-xml [blob]
  (let [doc (String. (io/get-bytes blob))
        xml (zip/node (l/parse doc :parser :xml))]
    [xml]))

(defmapfn sha1 [blob]
  (let [bytes (io/get-bytes blob)
        hash (hash/sha1 bytes)]
    [hash]))

(defn load-abbyy [path]
  (let [files (hfs-wholefile path)]
    (<- [?vlid ?hash ?xml ?img-blob]
        (files ?xml-name ?xml-blob)
        (files ?img-name ?img-blob)
        (filename-components ?xml-name :> ?vlid "xml")
        (filename-components ?img-name :> ?vlid "tif")
        (parse-xml ?xml-blob :> ?xml)
        (sha1 ?img-blob :> ?hash))))


(defn- zone
  "Calculates the zone around the given page element.
   This is used to uniquely identify page elements across multiple systems."
  [node]
  (let [l (-> node :attrs :l Integer.)
        t (-> node :attrs :t Integer.)
        r (-> node :attrs :r Integer.)
        b (-> node :attrs :b Integer.)]
    #{[l t] [l b] [r t] [r b]}))

(defn- zone= [zone]
  (fn [loc] (= zone (-> loc zip/node zone))))

(defn- zone-in [zones]
  (fn [loc] (boolean (zones (-> loc zip/node zone)))))

(defmapfn page-info
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

(defmapcatfn character
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

(defmapcatfn lines [loc]
  (for [line (l/select-locs loc (l/element= :line))]
    (map zone (l/select line (l/element= :charparams)))))


(def alphabet (set (map str "abcdefghijklmnopqrstuvwxyzäöüABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ")))
(def linewrap (set (map str "-¬")))

(defmapcatfn words [node]
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
                (l/negate (zone-in (set (keys zones))))
                (l/remove)))))
