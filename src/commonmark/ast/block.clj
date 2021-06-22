(ns commonmark.ast.block
  (:require [clojure.string :as string]
            [commonmark.ast.common :as common]
            [commonmark.block :as block]
            [commonmark.blockrun :as blockrun]
            [commonmark.blockrun.entry :as blockrun.entry]))

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
  [entry]
  (->> entry
       blockrun.entry/content
       blockrun/parse
       (mapv from-blockrun-entry)
       (common/node {:tag (first entry)})))

(defmethod from-blockrun-entry :leaf
  [[tag lines]]
  (->> lines
       (string/join "\r\n")
       (hash-map :tag tag :content)
       common/node))

