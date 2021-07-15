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
  (->> "()[]{}\""
       ((juxt identity #(map (partial str "\\") %)))
       (apply zipmap)))

(defn escape-re-delimiter
  [in]
  (string/escape (str in) re-delimiter-escape-hash))

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

(defn excluding-re
  "Returns a negative lookbehind RE which forbids match immediately prior to
   match the given regular expression."
  ([re limit]
   (let [any (str ".{0," limit "}")]
     (re-pattern (str "(?<!\\G" any re any ")"))))
  ([re]
   (excluding-re re 999)))

(defn but-unescaped-re
  "Returns a RE which matches any character except the ones given. The last
   argument may be an options hash. Valid options are :allow and :exclude.

     :allow is expected to be one or more character classes to match. When
     omitted, all characters except the arguments are matched.

     :exclude is expected to be one or more character classes to not match.
     When omitted, only the arguments are forbidden to match. Providing a value
     here extends, not replaces, the blocklist."
  [& cs]
  (let [{:as opts :keys [allow exclude]} ((ufn/validator map?) (last cs))
        vectorize (ufn/to-fix (every-pred some? (complement vector?)) vector)
        [allow exclude] (map vectorize [allow exclude])
        cs (cond-> cs opts butlast)
        cs (->> cs
                distinct
                (map escape-re-delimiter)
                string/join)
        char-classes (cond-> [(str "[^" cs (string/join exclude) "]")]
                       allow (conj (str "[" (string/join allow) "]")))]
    (re-pattern (str "(?:" #"(?<!\\)\\" "[" cs "]"
                           "|"
                           "[" (string/join "&&" char-classes) "]"
                     ")"))))

(defn non-backslash-re
  ""
  [character]
  (re-pattern (str #"(?<!(?<!\\)\\)" (escape-re-delimiter (str character)))))

