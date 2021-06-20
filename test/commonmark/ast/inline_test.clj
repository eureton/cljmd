(ns commonmark.ast.inline-test
  (:require [clojure.test :refer :all]
            [commonmark.ast.inline :refer :all]))

(deftest code-span-test
  (testing "minimal"
    (let [txt-node {:data {:tag :txt :content "xyz"}}]
      (testing "text"
        (is (= (-> "xyz" from-string (get-in [:children 0]))
               txt-node)))

      (testing "code span"
        (is (= (-> "`xyz`" from-string (get-in [:children 0]))
               {:data {:tag :cs}
                :children [txt-node]})))

      (testing "emphasis"
        (are [s] (= (-> s from-string (get-in [:children 0]))
                    {:data {:tag :em}
                     :children [txt-node]})
             "*xyz*"
             "_xyz_")))))

