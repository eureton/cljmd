(ns commonmark.ast.common)

(defn node
  "Returns an AST node with the given data and children."
  ([data children]
   (cond-> {:data data}
     children (assoc :children children)))
  ([data]
   (node data nil)))

(defn leaf?
  "Returns true if node has no children, false otherwise."
  [node]
  (not (contains? node :children)))

(defn add
  "Makes y the last child of x. If x has no children, y becomes the first child
   of x."
  ([x] x)
  ([x y]
   (update x :children (comp vec conj) y))
  ([x y1 y2]
   (update x :children (comp vec conj) y2 y1))
  ([x y1 y2 y3]
   (update x :children (comp vec conj) y3 y2 y1))
  ([x y1 y2 y3 & ys]
   (update x :children (comp vec concat) (conj ys y3 y2 y1))))

