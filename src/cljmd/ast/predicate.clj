(ns cljmd.ast.predicate
  (:refer-clojure :exclude [list?])
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]
            [cljmd.ast.common :refer [ontology]]))

(def tag (comp :tag :data))

(def txt? (comp #{:txt} tag))

(def p? (comp #{:p} tag))

(def li? (comp #{:li} tag))

(def hbr? (comp #{:hbr} tag))

(def sbr? (comp #{:sbr} tag))

(def blank? (comp #{:blank} tag))

(def adef? (comp #{:adef} tag))

(def fcblk? (comp #{:ofcblk} tag))

(def link? (comp #{:a :img} tag))

(def autolink? (comp #{:autolink} tag))

(def list? (comp #{:list} tag))

(def heading? (comp #{:atxh :stxh} tag))

(defn leaf?
  "True if node has no children, false otherwise."
  [node]
  (not (contains? node :children)))

(defn block?
  "True if node represents a block element, false otherwise."
  [node]
  (isa? (deref ontology) (tag node) :block))

(def has-inline?
  "True if the node has inline content which may be expanded into AST form,
   false otherwise."
  (let [leaf-tags #{:txt :tbr :hbr :sbr :html-inline :cs :blank :html-block
                    :icblk :ofcblk}]
    (every-pred leaf?
                (comp not leaf-tags tag))))

