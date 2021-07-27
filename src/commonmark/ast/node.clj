(ns commonmark.ast.node
  (:require [clojure.core.incubator :refer [dissoc-in]]
            [commonmark.ast.common :refer :all]
            [commonmark.ast.inline :as inline]))

(def has-inline?
  "Returns true if the node has inline content which may be expanded into AST
   form, false otherwise."
  (every-pred leaf?
              (comp not
                    #{:txt :hbr :sbr :html-inline :blank :html-block :icblk}
                    :tag
                    :data)))

(defn expand-inline
  "Assuming node contains inline Markdown content:
     1. parses an AST from the inline content
     2. appends the children of the AST to node
     3. removes inline content from node."
  [node context]
  (apply add (dissoc-in node [:data :content])
         (-> node :data :content (inline/from-string context) :children)))

