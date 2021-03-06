(ns cljmd.ast.common
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]
            [squirrel.node :refer [add node]]))

(defn branch
  "Chain of single-child nodes starting with the root and proceeding to the
   leaf. If the children value is provided, it will be used on the leaf node."
  ([data children]
   (let [data (reverse data)
         root (node (first data) children)]
     (loop [data (rest data)
            root root]
       (if (empty? data)
         root
         (recur (rest data)
                (add (node (first data)) root))))))
  ([data]
   (branch data [])))

(defn fix
  "Applies f to the fields entries in node."
  [node f & fields]
  (reduce #(cond-> %1
             ((:data %1) %2) (update-in [:data %2] f))
          node
          fields))

(def ontology (-> (make-hierarchy)
                  (derive :a           :inline)
                  (derive :img         :inline)
                  (derive :em          :inline)
                  (derive :strong      :inline)
                  (derive :cs          :inline)
                  (derive :hbr         :inline)
                  (derive :sbr         :inline)
                  (derive :html-inline :inline)

                  (derive :em                         :emphasis)
                  (derive :strong                     :emphasis)
                  (derive :strong-in-em               :emphasis)
                  (derive :strong-in-strong           :emphasis)
                  (derive :strong-in-strong-in-em     :emphasis)
                  (derive :strong-in-strong-in-strong :emphasis)

                  (derive :strong-in-em               :deep-emphasis)
                  (derive :strong-in-strong           :deep-emphasis)
                  (derive :strong-in-strong-in-em     :deep-emphasis)
                  (derive :strong-in-strong-in-strong :deep-emphasis)

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
  [node]
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

