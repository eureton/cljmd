(ns commonmark.ast.inline
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.inline :as inline]
            [commonmark.ast.common :refer [node]]
            [commonmark.util :as util]))

(def degenerate?
  "Returns true if the AST node is degenerate, false otherwise.
   A degenerate node is a non-leaf text node."
  (every-pred (comp some? :children)
              (comp #{:txt} :tag :data)))

(defmulti inflate
  "Recursively replaces inline markdown entities with the AST to which they
   belong. Returns an AST whose root node is tagged :doc."
  (fn [input]
    (cond
      (string? input) :string
      (map? input) (:tag input))))

(defmethod inflate :a
  [input]
  (node (select-keys input [:tag :destination :title])
        (:children (inflate (:text input)))))

(defmethod inflate :string
  [input]
  (when (string? input)
    (node {:tag :txt}
          (if-some [info (inline/tagger input)]
            (->> (util/split input (:pattern info))
                 (interpose info)
                 (remove empty?)
                 (map inflate)
                 (mapcat (ufn/to-fix degenerate? :children vector))
                 vec)
            [(node {:tag :txt
                    :content input})]))))

(defmethod inflate :default
  [input]
  (node (select-keys input [:tag])
        (:children (inflate (:content input)))))

(defn from-string
  "Parses string into an AST. Assumes string contains inline Markdown entities."
  [string]
  (assoc-in (inflate string) [:data :tag] :doc))

