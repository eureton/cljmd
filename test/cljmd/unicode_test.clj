(ns cljmd.unicode-test
  (:require [clojure.test :refer :all]
            [cljmd.unicode :refer :all]))

(deftest folding-mappings-test
  (testing "lowercase input"
    (is (= "ss" (folding-mappings \ß))))

  (testing "uppercase input"
    (is (= "ss" (folding-mappings \ẞ)))))

(deftest reverse-folding-mappings-test
  (testing "lowercase input"
    (is (= (reverse-folding-mappings "ss")
           #{"ß"  "ẞ"})))

  (testing "uppercase input"
    (is (nil? (reverse-folding-mappings "SS")))))

(deftest fold-test
  (testing "foldable input"
    (are [s] (= "muss" (fold s))
         "muß"
         "muẞ"))

  (testing "reverse-foldable input"
    (are [s] (= "muss" (fold s))
         "muss"
         "MUSS")))

(deftest unfold-test
  (testing "foldable input"
    (are [s] (= #{"muß"} (unfold s))
         "muß"
         "MUẞ"))

  (testing "reverse-foldable input"
    (are [s] (= #{"muß" "muẞ" "muss"} (unfold s))
         "muss"
         "MUSS")))

