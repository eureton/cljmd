(ns commonmark.ast.inline-test
  (:require [clojure.test :refer :all]
            [commonmark.ast.inline :refer :all]
            [commonmark.ast.common :refer [node]]))

(deftest from-string-test
  (testing "pun nil"
    (is (nil? (from-string nil))))

  (testing "minimal"
    (let [txt-node (node {:tag :txt :content "xyz"})]
      (testing "text"
        (is (= (-> "xyz" from-string (get-in [:children 0]))
               txt-node)))

      (testing "code span"
        (is (= (-> "`xyz`" from-string (get-in [:children 0]))
               (node {:tag :cs} [txt-node]))))

      (testing "emphasis"
        (are [s] (= (-> s from-string (get-in [:children 0]))
                    (node {:tag :em} [txt-node]))
             "*xyz*"
             "_xyz_"))

      (testing "link"
        (is (= (-> "[xyz](abc)" from-string (get-in [:children 0]))
                    (node {:tag :a
                           :destination "abc"} [txt-node]))))))

  (testing "nested emphasis"
    (let [em-tree (node {:tag :em}
                        [(node {:tag :txt :content "("})
                         (node {:tag :em} [(node {:tag :txt :content "xyz"})])
                         (node {:tag :txt :content ")"})])]
      (are [s] (= (-> s from-string (get-in [:children 0]))
                  em-tree)
           "_(_xyz_)_"
           "*(*xyz*)*")))

  (testing "inlines within link text"
    (is (= (-> "[`xyz` 123 *abc*](qpr)" from-string (get-in [:children 0]))
                (node {:tag :a
                       :destination "qpr"}
                      [(node {:tag :cs} [(node {:tag :txt :content "xyz"})])
                       (node {:tag :txt :content " 123 "})
                       (node {:tag :em} [(node {:tag :txt :content "abc"})])])))))

