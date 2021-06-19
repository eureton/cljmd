(ns commonmark.ast
  (:require [clojure.string :as string]
            [commonmark.block :as block]
            [commonmark.blockrun :as blockrun]))

(defn node
  "Returns an AST node with the given data and children."
  ([data children]
   (cond-> {:data data}
     children (assoc :children children)))
  ([data]
   (node data nil)))

(comment
(def ontology (-> (make-hierarchy)
                  (derive :bq :container)
                  (derive :li :container)
                  atom))

(defmulti from-blockrun-entry
  "Parses a markdown AST from entry."
  first
  :hierarchy ontology)

(defmethod from-blockrun-entry :container
  [[_ lines]]
  (->> lines
       (map #(:content (block/tagger %) %))
       (string/join "\r\n")
       blockrun/parse
       from-blockrun))

(defmethod from-blockrun-entry :default
  [[tag lines]]
  (->> lines
       (string/join "\r\n")
       (node {:tag tag})))

(defn from-blockrun
  "Parses a markdown AST from blockrun."
  [blockrun]
  (reduce (fn [acc x]
            (prn "Acc:" acc)
            (prn "x:" x)
            (update acc
                    :children
                    (comp vec conj)
                    (->> x
                         second
                         (map (juxt (comp :content block/tagger)
                                    identity))
                         (map #(some identity %))
                         (string/join "\r\n")
                         blockrun/parse
                         from-blockrun
                         (node {:tag (first x)}))))
          (node {:tag :doc})
          blockrun))
)

(comment (let [x [:_ ["> xyz" "    abc" "- pqr" "# hdg" "nop"]]]
  (->> x
       second
       (map (juxt (comp :content block/tagger)
                  identity))
       (map #(some identity %))
       (string/join "\r\n")
       blockrun/parse
       from-blockrun
;      (node {:tag (first x)})
       )))

