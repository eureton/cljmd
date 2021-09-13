(ns cljmd.ast
  (:require [clojure.string :as string]
            [clojure.core.incubator :refer [dissoc-in]]
            [flatland.useful.fn :as ufn]
            [squirrel.core :as squirrel]
            [cljmd.blockrun :as blockrun]
            [cljmd.ast.node :as node]
            [cljmd.ast.block :as block]
            [cljmd.ast.postprocessing :as postp]
            [cljmd.re.common :as re.common]
            [cljmd.ast.predicate :as pred]
            [cljmd.unicode :as unicode]
            [cljmd.util :as util]))

(defn blockphase-context
  "Returns the context of the block phase of the parsing process."
  [ast]
  (let [labels (comp (ufn/ap conj)
                     (juxt unicode/unfold unicode/fold)
                     :label)
        put (fn [acc x]
              (let [payload (select-keys x [:destination :title])]
                (reduce (fn [acc label]
                          (update acc
                                  (util/collapse-whitespace label)
                                  #(or %1 %2)
                                  payload))
                        acc
                        (labels x))))]
    {:definitions (squirrel/reduce (fn [acc x]
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
    (squirrel/map (ufn/to-fix pred/has-inline? contextful-expand)
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

