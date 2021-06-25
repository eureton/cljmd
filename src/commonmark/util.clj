(ns commonmark.util
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]))

(defn split
  [string pattern]
  (let [parts (string/split string pattern)
        length (count parts)]
    (cond-> parts
      (zero? length) (conj "" "")
      (= 1 length) (conj ""))))

(defn balanced-delimiters?
  [opener closer string]
  (let [escape-hash {\[ "\\[" \] "\\]"}
        unescaped-re (re-pattern (str #"(?<!\\)"
                                      "["
                                      (string/escape opener escape-hash)
                                      (string/escape closer escape-hash)
                                      "]"))
        delimeters #(or (re-seq unescaped-re %) [])]
    (some->> string
             delimeters
             (reduce (fn [acc x]
                       (ufn/fix acc (comp not neg?) (condp = x
                                                      opener inc
                                                      closer dec)))
                     0)
             zero?)))

