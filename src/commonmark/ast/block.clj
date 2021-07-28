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
  (let [parsers {:atxh block/atx-heading
                 :icblk block/indented-chunk-line
                 :p block/paragraph-line}
        parsed (parsers tag #(hash-map :content %))]
    (->> lines
         (map (comp :content parsed))
         (string/join "\r\n")
         (hash-map :tag tag :content)
         common/node)))

(defmethod from-blockrun-entry :stxh
  [[tag lines]]
  (->> lines
       pop
       (map string/trim)
       (string/join "\r\n")
       (hash-map :tag tag
                 :level (-> lines peek block/setext-heading :level)
                 :content)
       common/node))

(defmethod from-blockrun-entry :ofcblk
  [[tag lines]]
  (let [info (->> lines first block/opening-code-fence :info)]
    (->> (subvec lines 1 (dec (count lines)))
         (string/join "\r\n")
         (hash-map :tag tag :content)
         (merge (cond-> {}
                  (not (string/blank? info)) (assoc :info info)))
         common/node)))

(defmethod from-blockrun-entry :blank
  [[_ _]]
  (common/node {:tag :blank}))

(defmethod from-blockrun-entry :adef
  [[_ lines]]
  (->> lines
       (string/join " ")
       block/link-reference-definition
       common/node))

