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
  [string]
  (when string
    (loop [string string
           tokens {}]
      (if (contains? tokens string)
        {:rolled string
         :tokens tokens}
        (let [{:as info :keys [pattern]} (inline/tagger string)
              digest (str (hash info))
              info (assoc info :source (ufn/fix (re-find pattern string)
                                                vector? first))]
            (recur (string/replace-first string pattern digest)
                   (assoc tokens digest info)))))))

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
  (let [{:keys [rolled] overlooked :tokens} (roll (:text input))]
    (node (select-keys input [:tag :destination :title])
          (:children (inflate rolled (merge overlooked tokens))))))

(defmethod inflate :aref
  [{:keys [tag label source text]} tokens]
  (let [{:keys [rolled] overlooked :tokens} (roll (or text label))
        tokens (merge overlooked tokens)]
    (node {:tag tag
           :label (unroll-original label tokens)
           :source (unroll-original source tokens)}
          (:children (inflate rolled tokens)))))

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
  [string]
  (let [{:keys [rolled tokens]} (roll string)]
    (some->> (unroll-ast rolled tokens)
             (node {:tag :doc}))))

