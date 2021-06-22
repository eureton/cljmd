(ns commonmark.ast-test
  (:require [clojure.test :refer :all]
            [commonmark.ast :refer :all]
            [commonmark.ast.common :refer [node]]))

(deftest from-string-test
  (testing "minimal"
    (is (= (from-string "abc")
           (node {:tag :doc}
                 [(node {:tag :p}
                        [(node {:tag :txt
                                :content "abc"})])]))))

  (testing "nested inline"
    (testing "text within emphasis"
      (is (= (from-string "*abc*")
             (node {:tag :doc}
                   [(node {:tag :p}
                          [(node {:tag :em}
                                 [(node {:tag :txt :content "abc"})])])]))))

    (testing "nested emphasis"
      (is (= (from-string "*(*abc*)* xyz *pqr*")
             (node {:tag :doc}
                   [(node {:tag :p}
                          [(node {:tag :em}
                                 [(node {:tag :txt :content "("})
                                  (node {:tag :em}
                                        [(node {:tag :txt :content "abc"})])
                                  (node {:tag :txt :content ")"})])
                           (node {:tag :txt :content " xyz "})
                           (node {:tag :em}
                                 [(node {:tag :txt :content "pqr"})])])]))))))

