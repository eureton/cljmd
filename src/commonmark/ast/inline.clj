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
        (let [info (inline/tagger string)
              digest (str (hash info))]
            (recur (string/replace-first string (:pattern info) digest)
                   (assoc tokens digest info)))))))

(defn unroll
  "Transforms string into a vector of ASTs corresponding to inline Markdown
   tokens. Assumes string and tokens have been produced by the roll function."
  [string tokens]
  (if-some [[digest token] (->> tokens
                                (filter (comp #(string/includes? string %) key))
                                first)]
    (->> (util/split string (re-pattern digest))
         (interpose token)
         (remove empty?)
         (map #(inflate % (dissoc tokens digest)))
         (mapcat (ufn/to-fix degenerate? :children vector))
         vec)
    (when string
      [(node {:tag :txt :content string})])))

(defmethod inflate :link
  [input tokens]
  (let [{:keys [rolled] overlooked :tokens} (roll (:text input))]
    (node (select-keys input [:tag :destination :title])
          (:children (inflate rolled (merge overlooked tokens))))))

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
  [string]
  (let [{:keys [rolled tokens]} (roll string)]
    (some->> (unroll rolled tokens)
             (node {:tag :doc}))))

