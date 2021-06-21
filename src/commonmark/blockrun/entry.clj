(ns commonmark.blockrun.entry
  (:require [clojure.string :as string]
            [commonmark.block :as block]))

(defmulti content
  "Returns the content of the blockrun entry as a single string. If its contents
   span multiple lines, the lines are joined. If the entry represents a leaf
   block, no other processing is performed. If the entry represents a container
   node, the container whitespace and markers are stripped before joining."
  first
  :hierarchy block/ontology
  :default :leaf)

(defmethod content :li
  [[_ lines]]
  (let [origin (first lines)]
    (->> lines
         rest
         (map #(block/list-item-content % origin))
         (concat [(:content (block/list-item-lead-line origin))])
         (string/join "\r\n"))))

(defmethod content :leaf
  [[_ lines]]
  (string/join "\r\n" lines))

(comment
  (let [ls ["- phere"
            "lazy here"
            "    lazy ind"]
        e [:li ls]]
    (content e)))
