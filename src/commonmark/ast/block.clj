(ns commonmark.ast.block
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.ast.common :as common]
            [commonmark.block :as block]
            [commonmark.inline :as inline]
            [commonmark.blockrun :as blockrun]
            [commonmark.blockrun.entry :as blockrun.entry]))

(defmulti from-blockrun-entry
  "Parses markdown AST from blockrun entry."
  first
  :hierarchy common/ontology
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
       blockrun/from-string
       blockrun/postprocess
       (mapv from-blockrun-entry)
       (common/node {:tag (first entry)})))

(defmethod from-blockrun-entry :leaf
  [[tag lines]]
  (->> lines
       (string/join "\r\n")
       (hash-map :tag tag :content)
       common/node))

(defmethod from-blockrun-entry :p
  [[_ lines]]
  (let [fetch (comp string/join
                    (juxt (comp :content block/paragraph-line)
                          (comp (ufn/to-fix some? #(string/replace % #"\\" ""))
                                #(re-find inline/hard-line-break-re %))))]
    (->> lines
         (map fetch)
         (string/join "\r\n")
         (hash-map :tag :p :content)
         common/node)))

(defmethod from-blockrun-entry :atxh
  [[_ lines]]
  (->> lines
       (map (comp :content block/atx-heading))
       (string/join "\r\n")
       (hash-map :tag :atxh :content)
       common/node))

(defmethod from-blockrun-entry :blank
  [[_ _]]
  (common/node {:tag :blank}))

(defmethod from-blockrun-entry :adef
  [[_ lines]]
  (->> lines
       (string/join " ")
       block/link-reference-definition
       common/node))

