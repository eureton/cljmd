(ns commonmark.blockrun.entry
  (:require [clojure.string :as string]
            [commonmark.block :as block]
            [commonmark.re.link :as re.link]))

(def origin
  "Returns the first line."
  (comp first second))

(defmulti content
  "Returns the content of the blockrun entry as a single string. If its contents
   span multiple lines, the lines are joined. If the entry represents a leaf
   block, no other processing is performed. If the entry represents a container
   node, the container whitespace and markers are stripped before joining."
  first)

(defmethod content :li
  [[_ lines]]
  (let [origin (first lines)]
    (->> lines
         rest
         (map #(block/list-item-content % origin))
         (concat [(:content (block/list-item-lead-line origin))])
         (string/join "\r\n"))))

(defmethod content :bq
  [[_ lines]]
  (let [strip #(let [{:keys [tag content]} (block/tagger %)]
                 (if (= :bq tag) content %))]
    (->> lines
         (map strip)
         (string/join "\r\n"))))

(defmethod content :default
  [[_ lines]]
  (string/join "\r\n" lines))

(defn link-reference-definition-batch
  "Returns a sequence containing the lines which comprise adjacent link
   reference definitions. If no definitions exist, an empty sequence is
   returned."
  [[_ lines]]
  (->> lines
       (string/join "\n")
       (re-find re.link/reference-definition-batch)
       first
       string/split-lines
       (remove empty?)))

(defmulti promote
  "Hook for changing the tag of an entry after the blockrun has been compiled."
  first)

(defmethod promote :stxh
  [x]
  (cond-> x
    (-> x second count (= 1)) (assoc 0 :p)))

(defmethod promote :html-block-unpaired
  [x]
  (assoc x 0 (if (block/html-block-begin (origin x))
               :html-block
               :p)))

(defmethod promote :default [x] x)

