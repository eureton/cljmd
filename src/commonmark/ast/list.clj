(ns commonmark.ast.list
  (:require [flatland.useful.fn :as ufn]
            [commonmark.ast.common :refer [node add]]
            [commonmark.ast.predicate :as pred]
            [commonmark.ast.list.item :as item]))


(defn delimiter
  "Classifies as either \"paren\" or \"period\" if ordered, nil otherwise."
  [marker]
  (as-> marker v
        (re-find #"^\d{1,9}([.)])$" v)
        (second v)
        (ufn/fix v #{")"} "paren"
                   #{"."} "period"
                   nil)))

(defn start
  "The number to start from if ordered (as a string), nil otherwise."
  [marker]
  (->> marker
       (re-find #"^(\d{1,9})[.)]$")
       second))

(defn tight?
  "True if items comprise a tight list, false otherwise."
  [items]
  (and (->> items butlast (not-any? pred/blank?))
       (every? item/tight? items)))

(defn empty-for
  "List AST node with no children, configured for marker."
  [marker tight?]
  (let [list-type (-> marker item/marker-type name)
        delimiter (delimiter marker)
        start (start marker)]
    (node (cond-> {:tag :list
                   :type list-type
                   :tight (str tight?)}
            delimiter (assoc :delimiter delimiter)
            start (assoc :start start)))))

(defn from-items
  "AST node which represents a list comprising items."
  [items]
  (let [marker (-> items first :data :marker)
        root (empty-for marker (tight? items))]
    (->> items
         (remove pred/blank?)
         (map #(assoc % :data {:tag :li}))
         (reduce add root))))

