(ns commonmark.ast.predicate
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]))

(def tag (comp :tag :data))

(def li? (comp #{:li} tag))

(def blank? (comp #{:blank} tag))

(def adef? (comp #{:adef} tag))

