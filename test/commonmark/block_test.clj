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

(deftest thematic-break-test
  (testing "minimal"
    (are [s] (some? (thematic-break s))
         "***"
         "---"
         "___"))

  (testing "wrong characters"
    (are [s] (nil? (thematic-break s))
         "+++"
         "==="))

  (testing "not enough characters"
    (are [s] (nil? (thematic-break s))
         "--"
         "**"
         "__"))

  (testing "0-3 spaces indent => true"
    (are [s] (some? (thematic-break s))
         " ***"
         "  ***"
         "   ***"))

  (testing "4 spaces indent => false"
    (is (nil? (thematic-break "    ***"))))

  (testing "> 3 characters used => true"
    (is (some? (thematic-break "_____________________________________"))))

  (testing "spaces between characters => true"
    (are [s] (some? (thematic-break s))
         " - - -"
         " **  * ** * ** * **"
         "-     -      -      -"))

  (testing "spaces at the end => true"
    (is (some? (thematic-break "- - - -  "))))

  (testing "non-spaces => false"
    (are [s] (nil? (thematic-break s))
         "_ _ _ _ a"
         "a------"
         "---a---"))

  (testing "not all non-whitespace same => false"
    (is (nil? (thematic-break "*-*")))))

(deftest indented-chunk-line-test
  (testing "trim leading 4 spaces"
    (are [s c] (= (-> s indented-chunk-line :content)
                c)
         "    a simple"              "a simple"
         "      indented code block" "  indented code block"))

  (testing "trim leading tab"
    (are [s c] (= (-> s indented-chunk-line :content)
                c)
         "\ta simple"              "a simple"
         "\t  indented code block" "  indented code block"))

  (testing "contents not parsed as md"
    (are [s c] (= (-> s indented-chunk-line :content)
                c)
         "    <a/>"  "<a/>"
         "    *hi*"  "*hi*"
         "    - one" "- one"))

  (testing "leading spaces beyond 4 => included in content"
    (is (= (-> "      chunk2" indented-chunk-line :content)
           "  chunk2")))

  (testing "trailing spaces => included in content"
    (is (= (-> "    foo " indented-chunk-line :content)
           "foo "))))

(deftest fenced-code-block-pair?-test
  (testing "minimal matching pairs"
    (are [l1 l2] (fenced-code-block-pair? l1 l2)
         "```" "```"
         "~~~" "~~~"))

  (testing "matching pairs with info string"
    (are [l1 l2] (fenced-code-block-pair? l1 l2)
         "``` clojure" "```"
         "~~~ clojure" "~~~"))

  (testing "opening fence longer than closing"
    (are [l1 l2] (fenced-code-block-pair? l1 l2)
         "```"  "````"
         "~~~"  "~~~~"
         "````" "`````"
         "~~~~" "~~~~~"))

  (testing "not code block fences => no match"
    (are [l1 l2] (false? (fenced-code-block-pair? l1 l2))
         "```" "`` `"
         "```" "    ```"
         "```" "abc"
         "`` `" "```"
         "    ```" "```"
         "abc" "```"))

  (testing "non-matching pairs"
    (are [l1 l2] (false? (fenced-code-block-pair? l1 l2))
         "```" "~~~"
         "~~~" "```"))

  (testing "info string on closing fence => no match"
    (are [l1 l2] (false? (fenced-code-block-pair? l1 l2))
         "```"         "``` clojure"
         "~~~"         "~~~ clojure"
         "``` clojure" "``` clojure"
         "~~~ clojure" "~~~ clojure"))

  (testing "non-matching pairs"
    (are [l1 l2] (false? (fenced-code-block-pair? l1 l2))
         "```" "~~~"
         "~~~"  "```")))

