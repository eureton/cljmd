(ns commonmark.ast
  (:require [clojure.string :as string]
            [clojure.core.incubator :refer [dissoc-in]]
            [flatland.useful.fn :as ufn]
            [treeduce.core :as tree]
            [commonmark.blockrun :as blockrun]
            [commonmark.ast.node :as node]
            [commonmark.ast.block :as block]
            [commonmark.ast.postprocessing :as postp]
            [commonmark.re.common :as re.common]
            [commonmark.ast.predicate :as pred]
            [commonmark.unicode :as unicode]))

(defn blockphase-context
  "Returns the context of the block phase of the parsing process."
  [ast]
  (let [labels (comp (ufn/ap conj)
                     (juxt unicode/unfold unicode/fold)
                     :label)
        put (fn [acc x]
              (let [payload (select-keys x [:destination :title])]
                (reduce (fn [acc label]
                          (update acc label #(or %1 %2) payload))
                        acc
                        (labels x))))]
    {:definitions (tree/reduce (fn [acc x]
                                 (cond-> acc
                                   (= :adef (:tag x)) (put x)))
                               {}
                               ast
                               :depth-first)}))

(defn expand-inline
  "Matches link references with link definitions and completes the former with
   the information of the latter. Unmatched references are transformed into :txt
   nodes with the source text as content."
  [ast]
  (let [contextful-expand #(node/expand-inline % (blockphase-context ast))]
    (tree/map (ufn/to-fix pred/has-inline? contextful-expand)
              ast)))

(defn normalize
  "Bring untrusted input to a standard format."
  [string]
  (string/replace string
                  (re-pattern (str re.common/line-ending #"\z"))
                  ""))

(defn from-string
  "Parses markdown AST from string."
  [string]
  (reduce #(%2 %1)
          (->> string
               normalize
               blockrun/from-string
               blockrun/postprocess
               block/from-blockrun
               expand-inline)
          postp/queue))

