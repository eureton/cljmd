(ns commonmark.ast
  (:require [clojure.string :as string]
            [commonmark.block :as block]
            [commonmark.blockrun :as blockrun]))

(defn from-string
  "Parses markdown AST from string."
  [string]
  (->> string
       second
       (map (juxt (comp :content block/tagger)
                  identity))
       (map #(some identity %))
       (string/join "\r\n")
       blockrun/parse
       ))


(comment (let [x [:_ ["> xyz" "    abc" "- pqr" "# hdg" "nop"]]]
  ))

