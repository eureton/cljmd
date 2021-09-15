(ns cljmd.core
  (:require [cljmd.block]
            [cljmd.inline]
            [cljmd.ast.common]
            [cljmd.ast.inline]
            [cljmd.ast.block]
            [cljmd.ast]
            [cljmd.blockrun.entry]
            [cljmd.blockrun]
            [cljmd.render]))

(defmacro ppp
  "A convenience macro that pretty prints the last thing output. This is
   exactly equivalent to (pprint *1)."
  []
  `(cljmd.ast.common/pprint *1))

