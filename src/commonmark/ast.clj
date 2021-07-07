(ns commonmark.ast
  (:require [clojure.string :as string]
            [clojure.core.incubator :refer [dissoc-in]]
            [flatland.useful.fn :as ufn]
            [treeduce.core :as treeduce]
            [commonmark.blockrun :as blockrun]
            [commonmark.ast.common :as common]
            [commonmark.ast.block :as block]
            [commonmark.ast.inline :as inline]
            [commonmark.ast.postprocessing :as postp]))

(def has-inline?
  "Returns true if the node has inline content which may be expanded into AST
   form, false otherwise."
  (every-pred common/leaf?
              (comp not #{:txt :hbr :sbr :blank} :tag :data)))

(defn expand-inline
  "Assuming node contains inline Markdown content:
     1. parses an AST from the inline content
     2. appends the children of the AST to node
     3. removes inline content from node."
  [node]
  (apply common/add (dissoc-in node [:data :content])
                    (-> node :data :content inline/from-string :children)))

(defn from-string
  "Parses markdown AST from string."
  [string]
  (reduce #(treeduce/map %2 %1)
          (->> string
               blockrun/parse
               block/from-blockrun
               (treeduce/map (ufn/to-fix has-inline? expand-inline)))
          postp/queue))

