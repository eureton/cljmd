(ns commonmark.ast.postprocessing
  (:require [flatland.useful.fn :as ufn]
            [treeduce.core :as treeduce]
            [commonmark.ast.common :as common]))

(def block-with-hbr-end?
  "True if the node is a block and its last child is a :hbr, false otherwise."
  (every-pred common/block?
              (comp #{:hbr} :tag :data peek :children)))

(defn hbr-fix
  "Removes :hbr entities from the end of blocks."
  [ast]
  (treeduce/map (ufn/to-fix block-with-hbr-end?
                            #(update % :children pop))
                ast))

(def queue
  "A collection of post-processing fixes to apply to the AST."
  [hbr-fix])

