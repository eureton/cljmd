(ns cljmd.ast.block
  (:require [clojure.string :as string]
            [squirrel.node :refer [node]]
            [flatland.useful.fn :as ufn]
            [cljmd.ast.common :as common]
            [cljmd.block :as block]
            [cljmd.inline :as inline]
            [cljmd.blockrun :as blockrun]
            [cljmd.blockrun.entry :as blockrun.entry]
            [cljmd.blockrun.setext :as blockrun.setext]
            [cljmd.util :as util]))

(defmulti from-blockrun-entry
  "Parses markdown AST from blockrun entry."
  first
  :hierarchy common/ontology
  :default :leaf)

(defn from-blockrun
  "Parses markdown AST from blockrun."
  [blockrun]
  (node {:tag :doc}
        (mapv from-blockrun-entry blockrun)))

(defn from-container-blockrun-entry
  [entry]
  (let [index-map (blockrun.setext/make-map entry)]
    (->> entry
         (blockrun.setext/redact index-map)
         blockrun.entry/content
         blockrun/from-string
         (blockrun.setext/insert index-map)
         blockrun/postprocess
         (mapv from-blockrun-entry)
         (node {:tag (first entry)}))))

(defmethod from-blockrun-entry :leaf
  [[tag lines]]
  (->> lines
       (string/join "\r\n")
       (hash-map :tag tag :content)
       node))

(defmethod from-blockrun-entry :p
  [[_ lines]]
  (->> lines
       (map string/triml)
       (string/join "\r\n")
       string/trim
       (hash-map :tag :p :content)
       node))

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
  (let [join (ufn/to-fix not-empty (comp #(str % "\r\n")
                                         #(string/join "\r\n" %))
                                   "")]
    (->> lines
         (map #(util/trim-leading-whitespace % 4))
         join
         (hash-map :tag tag :content)
         node)))

(defmethod from-blockrun-entry :atxh
  [[tag [line _]]]
  (-> line
      block/atx-heading
      (select-keys [:tag :level :content])
      node))

(defmethod from-blockrun-entry :stxh
  [[tag lines]]
  (->> lines
       pop
       (map string/trim)
       (string/join "\r\n")
       (hash-map :tag tag
                 :level (-> lines peek block/setext-heading :level)
                 :content)
       node))

(defmethod from-blockrun-entry :ofcblk
  [[tag lines]]
  (let [opener (->> lines first block/opening-code-fence)
        info (:info opener)
        munch #(util/trim-leading-whitespace % (-> opener :indent count))
        join (ufn/to-fix not-empty (comp #(str % "\r\n")
                                         #(string/join "\r\n" %))
                                   "")
        closed? (->> lines peek block/closing-code-fence some?)
        end-index (cond-> (count lines)
                    closed? dec)]
    (->> (subvec lines 1 (max end-index 1))
         (map munch)
         join
         (hash-map :tag tag :content)
         (merge (cond-> {}
                  (not (string/blank? info)) (assoc :info info)))
         node)))

(defmethod from-blockrun-entry :blank
  [[_ lines]]
  (node {:tag :blank
         :count (count lines)}))

(defmethod from-blockrun-entry :adef
  [[_ lines]]
  (->> lines
       (string/join "\r\n")
       block/link-reference-definition
       node))

