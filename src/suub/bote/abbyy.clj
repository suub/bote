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
            [suub.bote.util :as util]
            [clojure.zip :as z]
            [clojure.data.zip.xml :as xz]
            [pandect.core :as hash]
            [mikera.image.core :refer :all]
            [mikera.image.filters :as filt]
            [me.raynes.fs :as fs]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ##Extracting information.
;; **

;; @@
(defn page-info ;TODO fix xml1
  "Extracts page metadata."
  [node]
  (let [pge (->> node
                 xml-seq
                 (some #(when (= :page (:tag %)) %))
                 :attrs)
        angle (->> node
                   xml-seq
                   (some #(:skewAngle (:attrs %)))
                   Double/parseDouble)]
    {:height (-> pge :height Integer/parseInt)
     :width  (-> pge :width Integer/parseInt)
     :skew   angle}))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/page-info</span>","value":"#'suub.bote.abbyy/page-info"}
;; <=

;; @@
(defn zone
  "Calculates the zone around the given page element.
   This is used to uniquely identify page elements across multiple systems."
  [node]
  (when (clojure.set/subset? #{:l :t :r :b}
                             (-> node :attrs keys set))
    (let [l (-> node :attrs :l Integer.)
          t (-> node :attrs :t Integer.)
          r (-> node :attrs :r Integer.)
          b (-> node :attrs :b Integer.)]
    {:l l
     :t t
     :r r
     :b b})))

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
(defn zone>bbox [z]
  (let [{:keys [l t r b]} z]
    {:x l
     :y t
     :w (Math/abs (- l r))
     :h (Math/abs (- t b))}))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/zone&gt;bbox</span>","value":"#'suub.bote.abbyy/zone>bbox"}
;; <=

;; @@
(defn characters
  "Extracts all character zones with their relevant data."
  [node]
  (for [node (xml-seq node)
        :when (= :charParams (:tag node))
        :let [z (zone node)
              s (-> node :content first (or [nil]))
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
  (->> node
       xml-seq
       (filter #(= :line (:tag %)))
       (map characters)))
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

;; @@
(defn text [e]
  (->> e
       flatten
       (map :char)
       (apply str)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/text</span>","value":"#'suub.bote.abbyy/text"}
;; <=

;; @@
(defn page-text [p]
  (apply str (map (fn [l]
                    (str (text l)
                         "\n"))
                  (lines p))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/page-text</span>","value":"#'suub.bote.abbyy/page-text"}
;; <=

;; **
;;; ##Reintegrating data into XML documents.
;; **

;; @@
(defn change [corr page]
  (let [expansions
        (ref
          (group-by :anchor-zone
                    (for [{f :from t :to} corr]
                      {:anchor-zone (:zone (first f))
                       :content [t]
                       :new-zone (into {}
                                       (map
                                         (fn [[k v]][k (str v)])
                                         (apply convex-hull (map :zone f))))})))
        removal
        (ref
          (clojure.set/difference
            (set (for [{f :from t :to} corr
                       r (rest f)]
                   (:zone r)))))]
    (loop [loc (z/xml-zip page)]
      (cond
        (z/end? loc)
          (z/root loc)
        (not= :charParams (:tag (z/node loc)))
          (-> loc
              z/next
              recur)
        (expansions (zone (z/node loc)))
          (let [n (z/node loc)
                z (zone n)
                nn (map
                     #(-> n
                          (assoc :content (:content %))
                          (update-in [:attrs] merge (:new-zone %)))
                     (expansions z))]
            (dosync
              (alter expansions dissoc z)
              (alter removal conj z))
            (recur
              (as-> loc %
                    (reduce z/insert-left % nn)
                    (z/remove %)
                    (z/next %))))
        (removal (zone (z/node loc)))
          (-> loc
              z/remove
              z/next
              recur)
        :else
          (-> loc
              z/next
              recur)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/change</span>","value":"#'suub.bote.abbyy/change"}
;; <=

;; **
;;; ##Correction
;; **

;; @@
(defn matcher [matcher query]
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.abbyy/matcher</span>","value":"#'suub.bote.abbyy/matcher"}
;; <=

;; **
;;; ##Util
;; **

;; @@
(defn files [path]
  (->> path
       io/file
       file-seq
       (filter #(and (fs/file? %)
                     (= ".xml" (fs/extension %))
                     (re-matches #"\d+" (fs/base-name % true))))
       (map #(vector (fs/base-name % true) %))))
;; @@

(declare image)
(defn image [char-node tiff-img]
  (let [{:keys [x y width height]} (:dim char-node)]
    (sub-image tiff-img x y width height)))

(defn extract-images
  "extracts images for the characters in node which match
   the pred?"
  [page pred?]
  (let [chars (apply concat (lines page))]
    (map image (filter (comp pred? :char) chars))))


(defn process-xml [xml-file tif]
  (let [page (clojure.xml/parse xml-file)]
    (extract-images page #{\n \u})))


(defn process-jg [jg]
  (let [fulltexts (sort-by (memfn getName) (fs/list-dir (io/file jg "fulltext")))
        images (sort-by (memfn getName) (fs/list-dir (io/file jg "image")))]
    (doseq [[xml tif] (map vector fulltexts images)]
      (process-xml xml tif))))

;;skript zum ausschneiden aller Teilbilder im gesamten Grenzboten
(defn process-gb [root-path]
  (let [jahrgaenge (->> (fs/list-dir (io/file root-path))
                        (filter (memfn isDirectory)))]
    (doseq [jg jahrgaenge] (process-jg jg))))