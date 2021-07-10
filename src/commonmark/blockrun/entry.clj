(ns commonmark.blockrun.entry
  (:require [clojure.string :as string]
            [commonmark.block :as block]))

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

(defmulti promote
  "Hook for performing transformations after the blockrun has been compiled."
  first)

(defmethod promote :html-block-unpaired
  [x]
  (assoc x 0 (if (block/html-block-begin (origin x))
               :html-block
               :p)))

(defmethod promote :default [x] x)

