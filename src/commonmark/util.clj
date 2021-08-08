(ns commonmark.util
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]))

(defn cluster
  "Groups subsequent items x and y in coll for which (pred x y) returns true.
   Returns a vector of vectors."
  [pred coll]
  (reduce (fn [acc x]
            (let [tail (peek acc)]
              (if (pred (peek tail) x)
                (-> acc pop (conj (conj tail x)))
                (conj acc [x]))))
          []
          coll))

(def tabstop
  "Number of spaces to expand tabs to."
  4)

(defn expand-tab
  "Expands tab characters to 4 spaces."
  [s]
  (string/replace s "\t" (string/join (repeat tabstop \space))))

(defn coalesce
  "Merges subsequent items x and y in coll for which (pred x y) returns true by
   reducing with rf over the items."
  [pred rf coll]
  (->> coll
       (cluster pred)
       (map #(reduce rf %))))

(defn trim-leading-whitespace
  "Removes whitespace from the beginning of string s. In case of spaces, max n
   are removed. In case of tabs, max (quot n 4) are removed."
  [s n]
  (let [max-tabs (quot n 4)
        max-of #(re-pattern (str "^" %2 "{1," %1 "}"))]
    (cond-> s
      (>= max-tabs 1) (string/replace (max-of max-tabs \tab)   "")
      (>= n        1) (string/replace (max-of n        \space) ""))))

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

(defn percent-encode-uri
  "Percent-encodes the path and query string, if any, of uri."
  [uri]
  (let [decode #(java.net.URLDecoder/decode % "UTF-8")
        uri-re #"^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?"
        [_ _ scheme _ authority path _ query _ fragment] (re-find uri-re uri)]
    (-> (try (java.net.URI. scheme
                            (and authority (decode authority))
                            (and path (decode path))
                            (and query (decode query))
                            (and fragment (decode fragment)))
             (catch java.net.URISyntaxException _ (java.net.URI. uri)))
        .toString
        (string/replace #"\P{ASCII}+" #(java.net.URLEncoder/encode % "UTF-8")))))

(defn last-line
  "The last line of string s, honoring blanks."
  [s]
  (when s
    (or (re-find #"(?m)^.*\z" s)
        "")))

