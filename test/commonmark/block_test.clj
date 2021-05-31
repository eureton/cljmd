(ns commonmark.block-test
  (:require [clojure.test :refer :all]
            [commonmark.block :refer :all]))

(deftest atx-heading-test
  (testing "minimal, 1-6"
    (are [s] (let [result (atx-heading s)]
               (and (some? result)
                    (= (:content result) "foo")))
         "# foo"
         "## foo"
         "### foo"
         "#### foo"
         "##### foo"
         "###### foo"))

  (testing "more than six # characters => nil"
    (is (nil? (atx-heading "####### foo"))))

  (testing "no space between # and contents => nil"
    (are [s] (nil? (atx-heading s))
         "#5 bolt"
         "#hashtag"))

  (testing "first # is escaped => nil"
    (is (nil? (atx-heading "\\## foo"))))
  
  (testing "leading and trailing whitespace ignored in parsing inline content"
    (is (= (-> "#                  foo  " atx-heading :content)
           "foo")))

  (testing "0-3 spaces indentation allowed"
    (are [s] (some? (atx-heading s))
         " ### foo"
         "  ## foo"
         "   # foo"))

  (testing "4 spaces => nil"
    (is (nil? (atx-heading "    # foo"))))

  (testing "closing sequence of #"
    (testing "optional"
      (is (some? (atx-heading "## foo ##"))))

    (testing "not the same length as opening sequence"
      (are [s] (some? (atx-heading s))
           "# foo ##################################"
           "##### foo ##"))

    (testing "trailing spaces allowed"
      (is (some? (atx-heading "### foo ###  "))))

    (testing "if non-spaces follow => part of contents"
      (is (= (-> "### foo ### b" atx-heading :content)
             "foo ### b")))

    (testing "must be preceded by a space"
      (is (= (-> "# foo#" atx-heading :content)
             "foo#")))

    (testing "backslash-escaped # characters => not part"
      (are [s c] (= (-> s atx-heading :content)
                    c)
           "### foo \\###" "foo ###"
           "## foo #\\##"  "foo ###"
           "# foo \\#"     "foo #")))

  (testing "can be empty"
    (are [s l] (= (-> s atx-heading :level)
                  l)
         "##"      2
         "#"       1
         "### ###" 3)))

(deftest thematic-break?-test
  (testing "minimal"
    (are [s] (true? (thematic-break? s))
         "***"
         "---"
         "___"))

  (testing "wrong characters"
    (are [s] (false? (thematic-break? s))
         "+++"
         "==="))

  (testing "not enough characters"
    (are [s] (false? (thematic-break? s))
         "--"
         "**"
         "__"))

  (testing "0-3 spaces indent => true"
    (are [s] (true? (thematic-break? s))
         " ***"
         "  ***"
         "   ***"))

  (testing "4 spaces indent => false"
    (is (false? (thematic-break? "    ***"))))

  (testing "> 3 characters used => true"
    (is (true? (thematic-break? "_____________________________________"))))

  (testing "spaces between characters => true"
    (are [s] (true? (thematic-break? s))
         " - - -"
         " **  * ** * ** * **"
         "-     -      -      -"))

  (testing "spaces at the end => true"
    (is (true? (thematic-break? "- - - -  "))))

  (testing "non-spaces => false"
    (are [s] (false? (thematic-break? s))
         "_ _ _ _ a"
         "a------"
         "---a---"))

  (testing "not all non-whitespace same => false"
    (is (false? (thematic-break? "*-*")))))

(deftest indented-chunk-line-test
  (testing "trim leading 4 spaces"
    (are [s c] (= (indented-chunk-line s)
                c)
         "    a simple"              "a simple"
         "      indented code block" "  indented code block"))

  (testing "trim leading tab"
    (are [s c] (= (indented-chunk-line s)
                c)
         "\ta simple"              "a simple"
         "\t  indented code block" "  indented code block"))

  (testing "contents not parsed as md"
    (are [s c] (= (indented-chunk-line s)
                c)
         "    <a/>"  "<a/>"
         "    *hi*"  "*hi*"
         "    - one" "- one"))

  (testing "leading spaces beyond 4 => included in content"
    (is (= (indented-chunk-line "      chunk2")
           "  chunk2")))

  (testing "trailing spaces => included in content"
    (is (= (indented-chunk-line "    foo ")
           "foo "))))

