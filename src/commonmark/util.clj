(ns commonmark.util
  (:require [clojure.string :as string]))

(defn split
  [string pattern]
  (let [parts (string/split string pattern)
        length (count parts)]
    (cond-> parts
      (zero? length) (conj "" "")
      (= 1 length) (conj ""))))

