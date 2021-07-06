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

  (testing "hard line break"
    (testing "spaces at beginning of next line"
      (is (= (-> "abc  \n     xyz" from-string (get-in [:children 0 :children]))
             [(node {:tag :txt :content "abc"})
              (node {:tag :hbr :content "  \r\n"})
              (node {:tag :txt :content "xyz"})])))

    (testing "inside emphasis"
      (is (= (-> "*abc  \nxyz*" from-string (get-in [:children 0 :children]))
             [(node {:tag :em}
                    [(node {:tag :txt :content "abc"})
                     (node {:tag :hbr :content "  \r\n"})
                     (node {:tag :txt :content "xyz"})])])))

    (testing "inside code span"
      (are [s c] (= (-> s from-string (get-in [:children 0 :children]))
                    [(node {:tag :cs}
                           [(node {:tag :txt :content c})])])
           "`abc  \nxyz`" "abc    xyz"
           "`abc\\\nxyz`" "abc\\  xyz"))

    (testing "inside raw HTML"
      (are [s] (= (-> s from-string (get-in [:children 0 :children]))
                    [(node {:tag :html-inline}
                           [(node {:tag :txt :content s})])])
           "<a href=\"x  \r\nyz\">"
           "<a href=\"x\\\r\nyz\">"))

    (testing "inside inline link"
      (are [s c] (= (-> s from-string (get-in [:children 0 :children 0]))
                    (node {:tag :a :destination "123"}
                          [(node {:tag :txt :content "abc"})
                           (node {:tag :hbr :content c})
                           (node {:tag :txt :content "xyz"})]))
           "[abc  \nxyz](123)" "  \r\n"
           "[abc\\\nxyz](123)" "\\\r\n")))

  (testing "post-processing"
    (testing "hard line break at end of block"
      (are [s cs] (= cs (-> s from-string (get-in [:children 0 :children])))
           "*abc*  "    [(node {:tag :em}
                               [(node {:tag :txt :content "abc"})])]
           "abc  "      [(node {:tag :txt :content "abc"})]
           "  "         nil
           "*abc*\\"    [(node {:tag :em}
                               [(node {:tag :txt :content "abc"})])
                         (node {:tag :txt :content "\\"})]
           "abc\\"      [(node {:tag :txt :content "abc"})
                         (node {:tag :txt :content "\\"})]
           "\\"         [(node {:tag :txt :content "\\"})]
           "## *abc*  " [(node {:tag :em}
                               [(node {:tag :txt :content "abc"})])]
           "## abc  "   [(node {:tag :txt :content "abc"})]
           "##   "      nil
           "## *abc*\\" [(node {:tag :em}
                               [(node {:tag :txt :content "abc"})])
                         (node {:tag :txt :content "\\"})]
           "## abc\\"   [(node {:tag :txt :content "abc"})
                         (node {:tag :txt :content "\\"})]
           "## \\"      [(node {:tag :txt :content "\\"})]))

    (testing "block containing only a hard line break"
      (is (nil? (-> "  " from-string (get-in [:children 0 :children])))))

    (testing "empty paragraph"
      (is (= (map (comp :tag :data)
                  (-> "- abc\r\n\r\n  \r\n> xyz" from-string :children))
             [:li :bq])))))

