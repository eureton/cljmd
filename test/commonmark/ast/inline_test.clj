(ns commonmark.ast.inline-test
  (:require [clojure.test :refer :all]
            [commonmark.ast.inline :refer :all]))

(deftest from-string-test
  (testing "minimal"
    (let [txt-node {:data {:tag :txt
                           :content "xyz"}}]
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
             "_xyz_"))))

  (testing "nested emphasis"
    (let [em-tree {:data {:tag :em}
                   :children [{:data {:tag :txt :content "("}}
                              {:data {:tag :em}
                               :children [{:data {:tag :txt
                                                  :content "xyz"}}]}
                              {:data {:tag :txt :content ")"}}]}]
      (are [s] (= (-> s from-string (get-in [:children 0]))
                  em-tree)
           "_(_xyz_)_"
           "*(*xyz*)*"))))

