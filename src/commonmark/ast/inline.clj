(ns commonmark.ast.inline
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.inline :as inline]
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

(defn roll
  "Produces a string, all inline Markdown tokens of which have been replaced
   by a digest. Iterates until the entire string has been collapsed into a
   single digest. Token meta-data are collected in a hashmap, keyed by digest.
   Returns the string and the meta-data in a hashmap under :rolled and :tokens,
   respectively."
  ([string context]
   (when string
     (loop [string string
            tokens {}
            context context]
       (if (contains? tokens string)
         {:rolled string
          :tokens tokens}
         (let [{:as info :keys [pattern]} (inline/tagger string context)
               digest (str (hash info))
               match (ufn/fix (re-find pattern string) vector? first)
               link (get-in context [:definitions match])]
             (recur (string/replace-first string pattern digest)
                    (assoc tokens digest (assoc info :source match))
                    (cond-> context
                      link (assoc-in [:definitions digest] link))))))))
  ([string]
   (roll string {})))

(defn unroll
  "Iteratively replaces the tokens found in string according to the stepf.
   Each time a token is detected in string, (stepf digest items) is returned,
   where items is the result of splitting the string wrt the token.
   When no token can be detected in string, (basef string) is returned."
  [string tokens basef stepf]
  (if-some [[digest token] (->> tokens
                                (filter (comp #(string/includes? string %) key))
                                first)]
    (stepf digest (util/split string (re-pattern digest)))
    (when string
      (basef string))))

(defn unroll-ast
  "Transforms string into a vector of ASTs corresponding to inline Markdown
   tokens."
  [string tokens]
  (unroll string
          tokens
          #(vector (node {:tag :txt :content %1}))
          (fn [digest items]
            (->> items
                 (interpose (tokens digest))
                 (remove empty?)
                 (map #(inflate % (dissoc tokens digest)))
                 (mapcat (ufn/to-fix degenerate? :children vector))
                 vec))))

(defn unroll-original
  "Transforms string into what is was before the roll process."
  [string tokens]
  (unroll string
          tokens
          identity
          (fn [digest items]
            (->> items
                 (interpose (:source (tokens digest)))
                 (remove empty?)
                 (map #(unroll-original % (dissoc tokens digest)))
                 string/join))))

(defmethod inflate :link
  [input tokens]
  (let [content-fn (some-fn :text :label)
        {:keys [rolled] overlooked :tokens} (roll (content-fn input))]
    (node (select-keys input [:tag :destination :title])
          (:children (inflate rolled (merge overlooked tokens))))))

(defmethod inflate :break
  [input _]
  (node (select-keys input [:tag :content])))

(defmethod inflate :string
  [input tokens]
  (node {:tag :txt}
        (unroll-ast input tokens)))

(defmethod inflate :default
  [{:keys [tag content]} tokens]
  (node {:tag tag}
        (unroll-ast content tokens)))

(defn from-string
  "Parses string into an AST. Assumes string contains inline Markdown entities.
   Returns an AST whose root node is tagged :doc."
  ([string context]
   (let [{:keys [rolled tokens]} (roll string context)]
     (some->> (unroll-ast rolled tokens)
              (node {:tag :doc}))))
  ([string]
   (from-string string {})))

