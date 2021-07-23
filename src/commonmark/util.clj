(ns commonmark.util
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]))

(def re-delimiter-escape-hash
  (->> "()[]{}\""
       ((juxt identity #(map (partial str "\\") %)))
       (apply zipmap)))

(defn escape-re-delimiter
  [in]
  (string/escape (str in) re-delimiter-escape-hash))

(defn balanced-unescaped-re
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
   (balanced-unescaped-re opener closer {})))

(defn balanced-re
  [l r]
  (let [fill (str "(?:(?:.(?!" l "|" r "))*.|)")
        pad #(->> %& (interpose fill) string/join)]
    (->> [;[l l l l r r r r] [l l l r l r r r] [l l l r r l r r] [l l r l r l r r]
          ;[l l l r r r] [l l r l r r]
          [l l r r]
          [l r]]
         (map #(apply pad %))
         (interpose "|")
         string/join
         (format "(?:%s)")
         re-pattern)))

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
                       allow (conj (str "[" (string/join "&&" allow) "]")))]
    (re-pattern (str "(?:" #"(?<!\\)\\" "[" cs "]"
                           "|"
                           "[" (string/join "&&" char-classes) "]"
                     ")"))))

(defn non-backslash-re
  "Returns a RE to match the input, not preceded by a backslash. Expects input
   to be either a character, a string or a RE."
  [input]
  (let [split (fn [x]
                (let [x (str x)]
                  [(first x)
                   (subs x (min 1 (count x)))]))
        splittable? (some-fn string?
                             #(= java.util.regex.Pattern (type %)))]
    (when-some [[x trail] (cond (splittable? input) (split input)
                                (char? input) [input])]
      (re-pattern (str #"(?<!(?<!\\)\\)"
                       (escape-re-delimiter x)
                       trail)))))

(defn or-re
  "Returns a RE which matches any of the given expressions."
  [& exps]
  (->> exps
       (string/join "|")
       (format "(?:%s)")
       re-pattern))

(defn normalize-link-label
  [label]
  (-> label
      (string/replace #"\s+" " ")
      string/lower-case))

(defn shard-count
  "The number of parts character c splits string s into, including empty ones."
  [s c]
  (->> s (filter #{c}) count inc))

(defn percent-encode
  "java.net.URLEncoder wrapper which maps spaces to %20. Encodes in UTF-8."
  [s]
  (->> (string/split s #" " (shard-count s \space))
       (map #(java.net.URLEncoder/encode % "UTF-8"))
       (string/join "%20")))

(defn percent-encode-uri
  "Percent-encodes the path and query string, if any, of uri."
  [uri]
  (let [encode (comp percent-encode #(java.net.URLDecoder/decode % "UTF-8"))
        splice #(->> (string/split %1 (re-pattern (str %2)) (shard-count %1 %2))
                     (map encode)
                     (string/join %2))
        param-re #"([^&]*)"
        query-re (re-pattern (str param-re "(?:&" param-re ")*"))
        uri-re #"^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?"
        [_ _ scheme _ hostport path _ query _ fragment] (re-find uri-re uri)]
;   (prn ">>>" [scheme hostport path query fragment] )
    (string/join (cond-> [scheme "://" (splice hostport \:)]
                   path (conj (splice path \/))
                   query (conj "?" (->> query
                                        (re-find query-re)
                                        (drop 1)
                                        (remove nil?)
                                        (map #(splice % \=))
                                        (string/join "&")))
                   fragment (conj "#" (encode fragment))))))

