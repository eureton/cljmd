(ns commonmark.ast.postprocessing
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [treeduce.core :as treeduce]
            [commonmark.ast.common :refer [block? node update-children]]))

(defn block-with-hbr-end?
  "True if the node is a block and its last child is an :hbr whose content
   satisfies pred, false otherwise."
  [pred]
  (every-pred block?
              (comp (every-pred (comp #{:hbr} :tag)
                                (comp pred :content))
                    :data
                    peek
                    :children)))

(def block-with-space-hbr-end?
  "Shorthand for calling block-with-hbr-end? with a predicate which checks for
   space."
  (block-with-hbr-end? #(string/includes? % "  ")))

(def block-with-backslash-hbr-end?
  "Shorthand for calling block-with-hbr-end? with a predicate which checks for
   backslash."
  (block-with-hbr-end? #(string/includes? % "\\")))

(defn hbr-fix
  "Deals with :hbr entities at the end of blocks."
  [ast]
  (let [pop-hbr #(update-children % pop)
        push-bslash #(update % :children conj (node {:tag :txt
                                                     :content "\\"}))]
    (treeduce/map (ufn/to-fix block-with-space-hbr-end?
                              pop-hbr
                              block-with-backslash-hbr-end?
                              (comp push-bslash pop-hbr))
                  ast)))

(def empty-text?
  "True if the parameter is a text node whose :content is either nil or \"\"."
  (every-pred (comp #{:txt} :tag :data)
              (comp empty? :content :data)))

(def empty-children?
  "True if the node * either has no children or all its children are empty text
   nodes, false otherwise."
  (some-fn (comp nil? :children)
           (comp empty? #(remove empty-text? %) :children)))

(def empty-paragraph?
  "True if all of the following apply to the parameter:
     * is a paragraph
     * either has no children or all its children are empty text nodes
   Returns false otherwise."
  (every-pred (comp #{:p} :tag :data)
              empty-children?))

(defn empty-p-fix
  "Removes empty :p entities from the AST."
  [ast]
  (treeduce/map #(update-children % (partial remove empty-paragraph?))
                ast))

(def empty-block?
  "True if all of the following apply to the parameter:
     * is a block
     * either has no children or all its children are empty text nodes
   Returns false otherwise."
  (every-pred block? empty-children?))

(defn empty-block-fix
  "Removes empty :p entities from the AST."
  [ast]
  (treeduce/map (ufn/to-fix empty-block? #(dissoc % :children))
                ast))

(defn blank-fix
  "Removes :blank entities from the AST."
  [ast]
  (treeduce/map #(update-children % (comp vec
                                          (partial remove (comp #{:blank}
                                                                :tag
                                                                :data))))
                ast))

(def queue
  "A collection of post-processing fixes to apply to the AST."
  [hbr-fix
   empty-p-fix
   empty-block-fix
   blank-fix])

