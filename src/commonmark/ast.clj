(ns commonmark.ast
  (:require [clojure.string :as string]
            [clojure.core.incubator :refer [dissoc-in]]
            [flatland.useful.fn :as ufn]
            [treeduce.core :as tree]
            [commonmark.blockrun :as blockrun]
            [commonmark.ast.common :as common]
            [commonmark.ast.block :as block]
            [commonmark.ast.inline :as inline]
            [commonmark.ast.postprocessing :as postp]))

(def has-inline?
  "Returns true if the node has inline content which may be expanded into AST
   form, false otherwise."
  (every-pred common/leaf?
              (comp not #{:txt :hbr :sbr :blank :html-block} :tag :data)))

(defn expand-inline
  "Assuming node contains inline Markdown content:
     1. parses an AST from the inline content
     2. appends the children of the AST to node
     3. removes inline content from node."
  [node]
  (apply common/add (dissoc-in node [:data :content])
                    (-> node :data :content inline/from-string :children)))

(defn remove-link-reference-definitions
  "Removes nodes tagged with :adef."
  [ast]
  (tree/map (fn [{:as node :keys [children]}]
              (let [children (remove (comp #{:adef} :tag :data) children)]
                (if (empty? children)
                  (dissoc node :children)
                  (assoc node :children (vec children)))))
            ast))

(defn inflate-link-references
  "Matches link references with link definitions and completes the former with
   the information of the latter. Unmatched references are transformed into :txt
   nodes with the source text as content."
  [ast]
  (let [label (comp string/lower-case :label)
        definitions (tree/reduce #(cond-> %1
                                    (= :adef (:tag %2)) (assoc (label %2) %2))
                                 {}
                                 ast
                                 :depth-first)
        reference? (comp #{:aref} :tag :data)
        inflate (fn [{:as node :keys [data children]}]
                  (let [lookup (-> data label definitions)]
                    (if lookup
                      (assoc node :data {:tag :a
                                         :title (:title lookup)
                                         :destination (:destination lookup)})
                      (common/node {:tag :txt
                                    :content (:source data)}))))]
    (tree/map (ufn/to-fix reference? inflate) ast)))

(defn from-string
  "Parses markdown AST from string."
  [string]
  (reduce #(tree/map %2 %1)
          (->> string
               blockrun/from-string
               blockrun/postprocess
               block/from-blockrun
               (tree/map (ufn/to-fix has-inline? expand-inline))
               inflate-link-references
               remove-link-reference-definitions)
          postp/queue))

