(ns commonmark.ast.block
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.ast.common :as common]
            [commonmark.block :as block]
            [commonmark.inline :as inline]
            [commonmark.blockrun :as blockrun]
            [commonmark.blockrun.entry :as blockrun.entry]
            [commonmark.util :as util]))

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

(defn from-container-blockrun-entry
  [entry]
  (->> entry
       blockrun.entry/content
       blockrun/from-string
       blockrun/postprocess
       (mapv from-blockrun-entry)
       (common/node {:tag (first entry)})))

(defmethod from-blockrun-entry :leaf
  [[tag lines]]
  (let [parsers {:p block/paragraph-line}
        parsed (parsers tag #(hash-map :content %))]
    (->> lines
         (map (comp :content parsed))
         (string/join "\r\n")
         (hash-map :tag tag :content)
         common/node)))

(defmethod from-blockrun-entry :bq
  [entry]
  (from-container-blockrun-entry entry))

(defmethod from-blockrun-entry :li
  [entry]
  (let [marker (->> entry
                    second
                    first
                    block/list-item-lead-line
                    :marker)
        raw (from-container-blockrun-entry entry)]
    (assoc-in raw [:data :marker] marker)))

(defmethod from-blockrun-entry :icblk
  [[tag lines]]
  (let [munch #(-> %
                   (string/replace #"(?<=^ {0,3})\t" "    ")
                   (util/trim-leading-whitespace 4))]
    (->> lines
         (split-with clojure.string/blank?)
         second
         reverse
         (split-with clojure.string/blank?)
         second
         reverse
         (map munch)
         (string/join "\r\n")
         (hash-map :tag tag :content)
         common/node)))

(defmethod from-blockrun-entry :atxh
  [[tag [line _]]]
  (-> line
      block/atx-heading
      (select-keys [:tag :level :content])
      common/node))

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
  (let [opener (->> lines first block/opening-code-fence)
        info (:info opener)
        munch #(util/trim-leading-whitespace % (-> opener :indent count))
        closed? (->> lines peek block/closing-code-fence some?)
        end-index (cond-> (count lines)
                    closed? dec)]
    (->> (subvec lines 1 (max end-index 1))
         (map munch)
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
       (string/join "\r\n")
       block/link-reference-definition
       common/node))

