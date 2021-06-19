(ns commonmark.util
  (:require [clojure.string :as string]))

(defn split
  [string pattern]
  (let [parts (string/split string pattern)]
    (cond-> parts
      (= 1 (count parts)) (conj ""))))

