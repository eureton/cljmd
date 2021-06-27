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

(defn balanced-re
  ([opener closer {:keys [intersect]}]
   (let [escape-hash (->> "()[]{}"
                          ((juxt identity #(map (partial str "\\") %)))
                          (apply zipmap))
         [l r] (->> [opener closer] (map escape-hash))
         escaped-delimeters (str "\\\\[" l r "]")
         non-delimeters (cond-> (str "&&[^" l r "]")
                          intersect (str "&&" intersect))
         fill (re-pattern (str "(?:"
                                 escaped-delimeters "|"
                                 "[\\p{Print}" non-delimeters "]"
                               ")*"))
         pad #(->> (interleave %& (repeat fill)) (cons fill) string/join)]
     (->> [[l l l r r r] [l l r l r r] [l l r r l r] [l r l l r r] [l r l r l r]
           [l l r r] [l r l r]
           [l r]
           []]
          (map #(apply pad %))
          (interpose "|")
          string/join
          (format "(?:%s)")
          re-pattern
         )))
  ([opener closer]
   (balanced-re opener closer {})))

