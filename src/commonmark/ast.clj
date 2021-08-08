(ns commonmark.ast
  (:require [clojure.string :as string]
            [clojure.core.incubator :refer [dissoc-in]]
            [flatland.useful.fn :as ufn]
            [treeduce.core :as tree]
            [commonmark.blockrun :as blockrun]
            [commonmark.ast.common :refer [update-children]]
            [commonmark.ast.node :as node]
            [commonmark.ast.block :as block]
            [commonmark.ast.postprocessing :as postp]))

(defn remove-link-reference-definitions
  "Removes nodes tagged with :adef."
  [ast]
  (tree/map (fn [node]
              (update-children node #(remove (comp #{:adef} :tag :data) %)))
            ast))

(defn blockphase-context
  "Returns the context of the block phase of the parsing process."
  [ast]
  {:definitions (tree/reduce (fn [acc {:as x :keys [tag label]}]
                               (cond-> acc
                                 (= :adef tag) (update (string/lower-case label)
                                                       #(or %1 %2)
                                                       x)))
                               {}
                               ast
                               :depth-first)})

(defn expand-inline
  "Matches link references with link definitions and completes the former with
   the information of the latter. Unmatched references are transformed into :txt
   nodes with the source text as content."
  [ast]
  (let [contextful-expand #(node/expand-inline % (blockphase-context ast))]
    (tree/map (ufn/to-fix node/has-inline? contextful-expand)
              ast)))

(defn from-string
  "Parses markdown AST from string."
  [string]
  (reduce #(%2 %1)
          (->> string
               blockrun/from-string
               blockrun/postprocess
               block/from-blockrun
               expand-inline
               remove-link-reference-definitions)
          postp/queue))

