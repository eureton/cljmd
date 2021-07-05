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

  (testing "link"
    (testing "inline"
      (testing "minimal"
        (is (= (from-string "[abc](xyz '123')")
               (node {:tag :doc}
                     [(node {:tag :p}
                            [(node {:tag :a
                                    :destination "xyz"
                                    :title "123"}
                                  [(node {:tag :txt
                                          :content "abc"})])])]))))

      (testing "preceded by literal '!'"
        (is (= (from-string "\\![abc](xyz)")
               (node {:tag :doc}
                     [(node {:tag :p}
                            [(node {:tag :txt
                                         :content "!"})
                             (node {:tag :a
                                    :destination "xyz"}
                                   [(node {:tag :txt
                                           :content "abc"})])])]))))))

  (testing "image"
    (testing "inline"
      (testing "description, destination and title"
        (is (= (from-string "![abc](/xyz \"123\")")
               (node {:tag :doc}
                     [(node {:tag :p}
                            [(node {:tag :img
                                    :destination "/xyz"
                                    :title "123"}
                                  [(node {:tag :txt
                                          :content "abc"})])])]))))

      (testing "image within image"
        (is (= (from-string "![txt ![abc](/xyz)](/123)")
               (node {:tag :doc}
                     [(node {:tag :p}
                            [(node {:tag :img
                                    :destination "/123"}
                                  [(node {:tag :txt
                                          :content "txt "})
                                   (node {:tag :img
                                          :destination "/xyz"}
                                         [(node {:tag :txt :content "abc"})])])])]))))

      (testing "link within image"
        (is (= (from-string "![txt [abc](/xyz)](/123)")
               (node {:tag :doc}
                     [(node {:tag :p}
                            [(node {:tag :img
                                    :destination "/123"}
                                  [(node {:tag :txt
                                          :content "txt "})
                                   (node {:tag :a
                                          :destination "/xyz"}
                                         [(node {:tag :txt :content "abc"})])])])]))))))

  (testing "nested inline"
    (testing "text within emphasis"
      (is (= (from-string "*abc*")
             (node {:tag :doc}
                   [(node {:tag :p}
                          [(node {:tag :em}
                                 [(node {:tag :txt :content "abc"})])])]))))

    (testing "nested __-strong emphasis"
      (is (= (from-string "__abc, __xyz__, pqr__")
             (node {:tag :doc}
                   [(node {:tag :p}
                          [(node {:tag :strong}
                                 [(node {:tag :txt :content "abc, "})
                                  (node {:tag :strong}
                                        [(node {:tag :txt :content "xyz"})])
                                  (node {:tag :txt :content ", pqr"})])])]))))

    (testing "nested *-emphasis"
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
                                 [(node {:tag :txt :content "pqr"})])])])))))

  (testing "post-processing"
    (testing "hard line break at end of block"
      (is (= (map (comp :tag :data)
                  (-> "`123` xyz *abc*  " from-string (get-in [:children 0 :children])))
             [:cs :txt :em])))

    (testing "block containing only a hard line break"
      (is (nil? (-> "  " from-string (get-in [:children 0 :children])))))))

