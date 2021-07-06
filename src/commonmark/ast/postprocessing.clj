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

(def empty-text?
  "True if the node is a paragraph which has no children."
  (every-pred (comp #{:txt} :tag :data)
              (comp empty? :content :data)))

(def empty-paragraph?
  "True if the node is a paragraph which has no children."
  (every-pred (comp #{:p} :tag :data)
              (some-fn (comp nil? :children)
                       (comp empty? #(remove empty-text? %) :children))))

(defn empty-p-fix
  "Removes empty :p entities from the AST."
  [ast]
  (treeduce/map #(update % :children (partial remove empty-paragraph?))
                ast))

(def queue
  "A collection of post-processing fixes to apply to the AST."
  [
   hbr-fix
   empty-p-fix
   ])

