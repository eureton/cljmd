(ns commonmark.ast.inline
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.inline :as inline]
            [commonmark.inline.token :as token]
            [commonmark.ast.common :refer [node ontology]]
            [commonmark.util :as util]))

(def degenerate?
  "Returns true if the AST node is degenerate, false otherwise.
   A degenerate node is a non-leaf text node."
  (every-pred (comp some? :children)
              (comp #{:txt} :tag :data)))

(defmulti inflate
  "Recursively replaces inline markdown entities with the AST to which they
   belong."
  (fn [input _]
    (cond
      (string? input) :string
      (map? input) (:tag input)))
  :hierarchy ontology)

(defn unroll
  "Transforms string into a vector of ASTs, each of which corresponds to an
   inline markdown entity."
  [string tokens]
  (if-some [{:as token :re/keys [start end]} (first tokens)]
    (->> [[(subs string 0 start) (->> tokens
                                      rest
                                      (filter #(token/before? % token)))]
          [token                 (->> tokens
                                      rest
                                      (filter #(token/within? % token))
                                      (map #(token/translate % (- start))))]
          [(subs string end)     (->> tokens
                                      rest
                                      (filter #(token/after? % token))
                                      (map #(token/translate % (- end))))]]
         (remove (comp empty? first))
         (map (ufn/ap inflate))
         (mapcat (ufn/to-fix degenerate? :children vector))
         vec)
    (when string
      [(node {:tag :txt
              :content (string/replace string #"\\(?=\p{Punct})" "")})])))

(defmethod inflate :link
  [{:as input :re/keys [match]} tokens]
  (let [content ((some-fn :text :label) input)]
    (->> tokens
         (map #(token/translate % (- (string/index-of match content))))
         (inflate content)
         :children
         (node (select-keys input [:tag :destination :title])))))

(defmethod inflate :break
  [input _]
  (node (select-keys input [:tag :content])))

(defmethod inflate :string
  [input tokens]
  (node {:tag :txt}
        (unroll input tokens)))

(defmethod inflate :default
  [{:keys [tag content]} tokens]
  (node {:tag tag}
        (unroll content tokens)))

(defn from-string
  "Parses string into an AST. Assumes string contains inline Markdown entities.
   Returns an AST whose root node is tagged :doc."
  ([string context]
   (some->> (inline/tokenizer string context)
            (unroll string)
            (node {:tag :doc})))
  ([string]
   (from-string string {})))

