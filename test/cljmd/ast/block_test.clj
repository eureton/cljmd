(ns cljmd.ast.block-test
  (:require [clojure.test :refer :all]
            [cljmd.ast.block :refer :all]))

(deftest from-blockrun-test
  (testing "pun nil"
    (is (nil? (from-blockrun nil)))))

