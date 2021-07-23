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

(deftest percent-encode-uri-test
  (testing "standard"
    (are [in out] (= out (percent-encode-uri in))
         "http://abc.com"                          "http://abc.com"
         "http://abc.com:1234"                     "http://abc.com:1234"
         "http://abc.com:1234/xyz"                 "http://abc.com:1234/xyz"
         "http://abc.com:1234/xyz?q=prs"           "http://abc.com:1234/xyz?q=prs"
         "http://abc.com:1234/xyz?q=prs&w=tuv"     "http://abc.com:1234/xyz?q=prs&w=tuv"
         "http://abc.com:1234/xyz?q=prs&w=tuv#123" "http://abc.com:1234/xyz?q=prs&w=tuv#123"))

  (testing "symbols"
    (are [in out] (= out (percent-encode-uri in))
         "http://a[b]c.c[o]m"                        "http://a%5Bb%5Dc.c%5Bo%5Dm"
         "http://abc.com:1[23]4"                     "http://abc.com:1%5B23%5D4"
         "http://abc.com:1234/x[y]z"                 "http://abc.com:1234/x%5By%5Dz"
         "http://abc.com:1234/xyz?q=p[r]s"           "http://abc.com:1234/xyz?q=p%5Br%5Ds"
         "http://abc.com:1234/xyz?q=p[r]s&w=t[u]v"   "http://abc.com:1234/xyz?q=p%5Br%5Ds&w=t%5Bu%5Dv"
         "http://abc.com:1234/xyz?q=prs&w=tuv#1[2]3" "http://abc.com:1234/xyz?q=prs&w=tuv#1%5B2%5D3"))

  (testing "percent-encoded"
    (are [s s] (= s (percent-encode-uri s))
         "http://%3C%20%3E.com"
         "http://abc.com:%3C%20%3E"
         "http://abc.com:1234/%3C%20%3E"
         "http://abc.com:1234/xyz?q=%3C%20%3E"
         "http://abc.com:1234/xyz?q=prs&w=%3C%20%3E"
         "http://abc.com:1234/xyz?q=prs&w=tuv#%3C%20%3E")))

