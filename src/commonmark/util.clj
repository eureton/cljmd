(ns commonmark.util
  (:require [clojure.string :as string]
            [commonmark.block :as block]
            [commonmark.inline :as inline]
            [commonmark.blockrun :as blockrun]))

(defn split
  [string pattern]
  (let [parts (string/split string pattern)]
    (cond-> parts
      (= 1 (count parts)) (conj ""))))

