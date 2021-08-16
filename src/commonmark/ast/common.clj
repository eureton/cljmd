(ns commonmark.ast.common
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]))

(defn node
  "Returns an AST node with the given data and children."
  ([data children]
   (cond-> {:data data}
     children (assoc :children children)))
  ([data]
   (node data nil)))

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

(defn update-children
  "If (apply f children args) evaluates to nil, :children is removed entirely.
   Otherwise, the result is associated with :children."
  [node f & args]
  (let [result (apply update node :children f args)]
    (cond-> result
      (empty? (:children result)) (dissoc :children))))

(def ontology (-> (make-hierarchy)
                  (derive :a           :inline)
                  (derive :img         :inline)
                  (derive :em          :inline)
                  (derive :strong      :inline)
                  (derive :cs          :inline)
                  (derive :hbr         :inline)
                  (derive :sbr         :inline)
                  (derive :html-inline :inline)

                  (derive :em           :emphasis)
                  (derive :strong       :emphasis)
                  (derive :strong-in-em :emphasis)

                  (derive :a      :link)
                  (derive :img    :link)

                  (derive :bq         :block)
                  (derive :list       :block)
                  (derive :li         :block)
                  (derive :p          :block)
                  (derive :blank      :block)
                  (derive :atxh       :block)
                  (derive :stxh       :block)
                  (derive :tbr        :block)
                  (derive :icblk      :block)
                  (derive :ofcblk     :block)
                  (derive :doc        :block)
                  (derive :html-block :block)

                  atom))

(defn pprint-content
  "Shorthand for printing the :content value of entities to *out*."
  [node]
  (->> node :data :content (.write ^java.io.Writer *out*)))

(defn pprint-prefix
  "Returns a string which is suitable for use as a pretty-printing logical block
   prefix for the given node."
  [{:keys [data]}]
  (let [attributes (->> (dissoc data :tag)
                        clojure.walk/stringify-keys
                        (map (juxt key (comp #(str "\"" % "\"") val)))
                        (map #(string/join "=" %))
                        (string/join " "))]
    (string/join ["<"
                  (name (:tag data))
                  (when-not (empty? attributes) " ")
                  attributes
                  ">"])))

(defn pprint-multiple
  "Shorthand for printing multiple entities. The entities are assumed to be
   siblings and therefore indented by the same amount. Each entity appears on
   a fresh line. Line breaks are rendered both before and after the entities."
  [node]
  (let [prefix (pprint-prefix node)]
    (pp/pprint-indent :block (-> prefix count (- 2) -))
    (pp/pprint-newline :mandatory)
    (pp/print-length-loop [nodes (:children node)]
      (when nodes
        (pp/write-out (first nodes))
        (when-let [tail (next nodes)]
          (pp/pprint-newline :linear)
          (recur tail))))
    (pp/pprint-indent :block (- (count prefix)))
    (pp/pprint-newline :mandatory)))

(defmacro pprint-entity-wrap
  "Expands into a series of pprint-family function calls to provide output
   wrapping for the given body."
  [node body]
  `(pp/pprint-logical-block :prefix (pprint-prefix ~node)
                            :suffix (string/join ["</" (name (:tag (:data ~node))) ">"])
                            ~body))

(defmulti dispatch
  "Pretty-print dispatch function for interoperation with the clojure.pprint
   family of functions."
  (comp :tag :data)
  :hierarchy ontology)

(defmethod dispatch :txt
  [node]
  (pp/pprint-logical-block :prefix "<txt>" :suffix "</txt>"
    (pprint-content node)))

(defmethod dispatch :block
  [{:as node :keys [data children]}]
  (pprint-entity-wrap node
    (pprint-multiple node)))

(defmethod dispatch :inline
  [{:as node :keys [children]}]
  (pprint-entity-wrap node
    (if (and (= 1 (count children))
             (= :txt (:tag (:data (first children)))))
      (pprint-content (first children))
      (pprint-multiple node))))

(defn pprint
  "Shorthand for binding the pretty-print dispatch function and calling
   clojure.pprint/pprint on ast."
  [ast]
  (pp/with-pprint-dispatch dispatch
    (pp/pprint ast)))

