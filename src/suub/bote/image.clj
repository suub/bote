;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns suub.bote.image
  (:require [gorilla-plot.core :as plot]
            [gorilla-repl.image :as gi]
            [mikera.image.core :as img]
            [mikera.image.filters :as filt]
            [suub.bote.abbyy :as abbyy]
            [suub.bote.clojure.xml :as xml]))
;; @@

;; @@
(img/load-image (clojure.java.io/file "resources/imge"))
;; @@

;; @@
(def f (abbyy/files "/Users/ticking/Desktop/vls-ro/"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;suub.bote.image/f</span>","value":"#'suub.bote.image/f"}
;; <=

;; @@
(take 10 (map
  #(-> %
       second
       xml/parse
       abbyy/page-info
       :skew)
  f))
;; @@

;; @@
(gi/image-view (download-image 123093))
;; @@
;; =>
;; <=