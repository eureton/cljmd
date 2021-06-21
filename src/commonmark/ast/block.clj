(ns commonmark.ast.block
  (:require [clojure.string :as string]
            [commonmark.ast.common :as common]
            [commonmark.block :as block]
            [commonmark.blockrun :as blockrun]))

(defmulti from-blockrun-entry
  "Parses markdown AST from blockrun entry."
  first
  :hierarchy block/ontology
  :default :leaf)

(defn from-blockrun
  "Parses markdown AST from blockrun."
  [blockrun]
  (common/node {:tag :doc}
               (mapv from-blockrun-entry blockrun)))

(defmethod from-blockrun-entry :container
  [[_ lines]]
  (->> lines
       (map #(:content (block/tagger %) %))
       (string/join "\r\n")
       blockrun/parse
       from-blockrun))

(defmethod from-blockrun-entry :leaf
  [[tag lines]]
  (->> lines
       (string/join "\r\n")
       (hash-map :tag tag :content)
       common/node))

(comment
  (let [x [:_ ["> xyz" "    abc" "- pqr" "# hdg" "nop"]]
                 y ["> xyz" "    abc" "- pqr" "# hdg" "nop"]]
    (comment(->> x
         second
         (map (juxt (comp :content block/tagger)
                    identity))
         (map #(some identity %))
         (string/join "\r\n")
         blockrun/parse
         from-blockrun
         (common/node {:tag (first x)})
         ))
    (->> y
         (string/join "\r\n")
         blockrun/parse
         from-blockrun
         )))

