(ns commonmark.ast.list
  (:require [flatland.useful.fn :as ufn]
            [commonmark.ast.common :refer [node add]]))

(defn marker-type
  "Classifies as either :bullet or :ordered."
  [marker]
  (when marker
    (if (some? (re-find #"[-+*]" marker))
      :bullet
      :ordered)))

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

(defn sibling-items?
  "True if items x and y belong in the same list, false otherwise."
  [x y]
  (let [{{x-tag :tag x-marker :marker} :data} x
        {{y-tag :tag y-marker :marker} :data} y
        x-type (marker-type x-marker)
        y-type (marker-type y-marker)]
    (and (= x-tag :li)
         (= y-tag :li)
         (or (= x-marker y-marker)
             (and (= x-type :ordered)
                  (= y-type :ordered)
                  (= (last x-marker)
                     (last y-marker)))))))

(defn empty-for
  "List AST node with no children, configured for marker."
  [marker tight?]
  (let [list-type (-> marker marker-type name)
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
  (let [tight? (every? (comp :tight? :data) items)]
    (->> items
         (map #(assoc % :data {:tag :li}))
         (reduce add (-> items first :data :marker (empty-for tight?))))))

