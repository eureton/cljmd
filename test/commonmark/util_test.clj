(ns commonmark.util-test
  (:require [clojure.test :refer :all]
            [commonmark.util :refer :all]))

(deftest but-unescaped-re-test
  (testing "standard"
    (is (= (re-seq (but-unescaped-re \b \y) "abcxyz")
           ["a" "c" "x" "z"])))

  (testing "backslash escapes"
    (is (= (re-seq (but-unescaped-re \b \y) "a\\bcx\\yz")
           ["a" "\\b" "c" "x" "\\y" "z"])))

  (testing ":allow option"
    (testing "single"
      (is (= (re-seq (but-unescaped-re \b \y {:allow #"\w"}) "abc 123 xyz")
             ["a" "c" "1" "2" "3" "x" "z"])))

    (testing "multiple"
      (is (= (re-seq (but-unescaped-re \b \y {:allow [#"\w" "a-f"]}) "abc123xyz")
             ["a" "c"]))))

  (testing ":exclude option"
    (testing "single"
      (is (= (re-seq (but-unescaped-re \b \y {:exclude #"\d"}) "abc123xyz")
             ["a" "c" "x" "z"])))

    (testing "multiple"
      (is (= (re-seq (but-unescaped-re \b \y {:exclude [#"\d" "a-f"]}) "abc123xyz")
             ["x" "z"]))))

  (testing ":allow and :exclude options"
    (is (= (re-seq (but-unescaped-re \2 \5 {:allow #"\d\s" :exclude #"\t"}) "abc123 \txyz456")
           ["1" "3" " " "4" "6"]))))

