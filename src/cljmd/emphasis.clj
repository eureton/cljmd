(ns cljmd.emphasis
  (:require [cljmd.emphasis.span :as span]))

(def from-string
  "Shorthand for emphasis.span/outermost."
  span/outermost)

