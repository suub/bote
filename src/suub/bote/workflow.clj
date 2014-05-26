(ns suub.bote.workflow
  (:require [suub.bote.abbyy :as a]
            [suub.bote.dictionary :as d]
            [suub.bote.util :as u]))

(defn correct-dir
  "Corrects all xml files in directory
   and writes them to target directory."
  [d t]
  (let [dict (d/buildin-dict)
        ]))
