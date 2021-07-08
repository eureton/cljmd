(ns commonmark.util
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]))

(defn split
  [string pattern]
  (let [suffix (str (+ 10000 (rand-int 10000)))
        desufficize #(subs % 0 (- (count %) (count suffix)))]
    (->> (string/split (str string suffix) pattern)
         ((juxt butlast
                (comp vector desufficize peek)))
         (apply concat)
         vec)))

(def re-delimiter-escape-hash
  (->> "()[]{}"
       ((juxt identity #(map (partial str "\\") %)))
       (apply zipmap)))

(defn escape-re-delimiter
  [string]
  (string/escape string re-delimiter-escape-hash))

(defn balanced-re
  ([opener closer {:keys [intersect]}]
   (let [[l r] (map re-delimiter-escape-hash [opener closer])
         non-delimeters (cond-> (str "[^" l r "]")
                          intersect (str "&&" intersect))
         fill (re-pattern (str "(?:"
                                 (str #"\\" "[" l r "]") "|"
                                 "[" non-delimeters "]"
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
          re-pattern)))
  ([opener closer]
   (balanced-re opener closer {})))

(defn some-re-fn-indexed
  "Returns a function which, when called on string input s, returns
   [index item], where item is the first regular expression parameter to match
   s and index is its index in coll."
  [regexps]
  (->> regexps
       (map-indexed #(fn [x] (some->> x (re-find %2) (vector %1))))
       (apply some-fn)))

