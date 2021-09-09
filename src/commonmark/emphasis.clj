(ns commonmark.emphasis
  (:require [commonmark.emphasis.span :as span]))

(def from-string
  "Shorthand for emphasis.span/outermost."
  span/outermost)

