(ns commonmark.util
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.unicode :as unicode]))

(defn cluster
  "Groups items in coll for which (pred acc x) returns true, where acc is the
   accumulated collection of clusters and x is the current item.
   Returns a vector of vectors."
  [pred coll]
  (reduce (fn [acc x]
            (if (pred acc x)
              (-> acc
                  (ufn/fix not-empty pop)
                  (conj (conj (peek acc) x)))
              (conj acc [x])))
          []
          coll))

(def tabstop
  "Number of spaces to expand tabs to."
  4)

(defn expand-tab
  "Expands a maximum of :limit tab characters to the :tabstop value. If :limit
   is not provided, expands all."
  ([s {:keys [tabstop limit]
       :or {tabstop tabstop}}]
   (let [re #"(.*?)\t"
         f (fn [[_ previous]]
             (let [length (- tabstop (mod (count previous) tabstop))]
               (str previous (string/join (repeat length \space)))))]
     (if (nil? limit)
       (string/replace s re f)
       (loop [n limit
              s s]
         (if (zero? n)
           s
           (recur (dec n) (string/replace-first s re f)))))))
  ([s]
   (expand-tab s {})))

(defn coalesce
  "Clusters items in coll with pred, then reduces over them with rf."
  [pred rf coll]
  (->> coll
       (cluster pred)
       (map #(reduce rf %))))

(defn trim-leading-whitespace
  "Removes whitespace from the beginning of string s. Tabs are expanded as
   needed before processing."
  [s n]
  (let [expand #(loop [s %]
                  (if (re-find (re-pattern (str "^ {0," (max 0 (dec n)) "}\t")) s)
                    (recur (expand-tab s {:limit 1}))
                    s))
        trim #(string/replace % (re-pattern (str "^ {1," n "}")) "")]
    (ufn/fix s (>= n 1) (comp trim expand))))

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

(defn collapse-whitespace
  "Replace consecutive whitespace characters with a single space."
  [string]
  (string/replace string #"\s+" " "))

(def normalize-link-label
  (comp unicode/fold collapse-whitespace))

(defn percent-encode-uri
  "Percent-encodes the path and query string, if any, of uri."
  [uri]
  (let [decode #(java.net.URLDecoder/decode % "UTF-8")
        encode #(java.net.URLEncoder/encode % "UTF-8")
        uri-re #"^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?"
        [_ _ scheme _ authority path _ query _ fragment] (re-find uri-re uri)]
    (-> (try (java.net.URI. scheme
                            (and authority (decode authority))
                            (and path (decode path))
                            (and query (decode query))
                            (and fragment (decode fragment)))
             (catch java.net.URISyntaxException _
                    (try (java.net.URI. uri)
                         (catch java.net.URISyntaxException _ uri))))
        .toString
        (string/replace #"\P{ASCII}+" encode)
        (string/replace #"[\[\]]" encode))))

(defn bounded-matches
  "Returns a vector of hashes, each of which contains:
     * the RE match, i.e. the output of (re-find re s)
     * the start index (include) of re in s
     * the end index (exclusive) of re in s"
  [re s]
  (let [matcher (re-matcher re s)]
    (loop [result []
           match (re-find matcher)]
      (if (nil? match)
        result
        (recur (conj result {:re/match match
                             :re/start (.start matcher)
                             :re/end (.end matcher)})
               (re-find matcher))))))

(defn graph
  "Hashmap of items x and y in coll for which (pred x y) is true. The key is x
   and the associated value is a sequence of all y."
  [pred coll]
  (->> coll
       (map #(filter (partial pred %) coll))
       (zipmap coll)
       (remove (comp empty? val))
       (into {})))

