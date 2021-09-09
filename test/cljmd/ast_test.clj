(ns cljmd.ast-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [cljmd.ast :refer :all]
            [cljmd.ast.common :refer [node branch]]))

(deftest from-string-test
  (testing "minimal"
    (is (= (-> "abc" from-string :children)
           [(branch [{:tag :p} {:tag :txt :content "abc"}])])))

  (testing "ATX heading"
    (testing "minimal"
      (is (= (-> "# abc" from-string :children)
             [(branch [{:tag :atxh :level 1} {:tag :txt :content "abc"}])])))

    (testing "level"
      (are [s l] (= (-> s from-string :children)
                    [(branch [{:tag :atxh :level l} {:tag :txt :content "abc"}])])
           "# abc"      1
           "## abc"     2
           "### abc"    3
           "#### abc"   4
           "##### abc"  5
           "###### abc" 6))

    (testing "coalesce"
      (is (= (-> "# abc\n# xyz" from-string :children)
             [(branch [{:tag :atxh :level 1} {:tag :txt :content "abc"}])
              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])])))

    (testing "nested inline"
      (are [s t] (= (-> s from-string :children)
                    [(node {:tag :atxh :level 1}
                           t)])
           "# *abc*"      [(branch [{:tag :em} {:tag :txt :content "abc"}])]
           "# **abc**"    [(branch [{:tag :strong} {:tag :txt :content "abc"}])]
           "# `abc`"      [(node {:tag :cs :content "abc"})]
           "# [abc](xyz)" [(branch [{:tag :a :destination "xyz"} {:tag :txt :content "abc"}])]))

    (testing "deeply nested inline"
      (is (= (-> "# qpr [*(**abc**)* `def`](xyz)" from-string :children)
             [(node {:tag :atxh :level 1}
                    [(node {:tag :txt :content "qpr "})
                     (node {:tag :a :destination "xyz"}
                           [(node {:tag :em}
                                  [(node {:tag :txt :content "("})
                                   (node {:tag :strong}
                                         [(node {:tag :txt :content "abc"})])
                                   (node {:tag :txt :content ")"})])
                            (node {:tag :txt :content " "})
                            (node {:tag :cs :content "def"})])])])))

    (testing "preceded by block"
      (are [b n] (= (-> (str b "\n# abc") from-string :children)
                    [n
                     (node {:tag :atxh :level 1}
                           [(node {:tag :txt :content "abc"})])])
           "---"                (node {:tag :tbr :content "---"})
           "- xyz"              (branch [{:tag :list :type "bullet" :tight "true"}
                                         {:tag :li}
                                         {:tag :p}
                                         {:tag :txt :content "xyz"}])
           "xyz\n---"           (branch [{:tag :stxh :level 2} {:tag :txt :content "xyz"}])
           "    xyz"            (node {:tag :icblk :content "xyz\r\n"})
           "```\nxyz\n```"      (node {:tag :ofcblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})
           "> xyz"              (branch [{:tag :bq} {:tag :p} {:tag :txt :content "xyz"}])
           "xyz"                (branch [{:tag :p} {:tag :txt :content "xyz"}])))

    (testing "followed by block"
      (are [b n] (= (-> (str "# abc\n" b) from-string :children)
                    [(node {:tag :atxh :level 1}
                           [(node {:tag :txt :content "abc"})])
                     n])
           "---"                (node {:tag :tbr :content "---"})
           "- xyz"              (branch [{:tag :list :type "bullet" :tight "true"}
                                         {:tag :li}
                                         {:tag :p}
                                         {:tag :txt :content "xyz"}])
           "xyz\n---"           (branch [{:tag :stxh :level 2} {:tag :txt :content "xyz"}])
           "    xyz"            (node {:tag :icblk :content "xyz\r\n"})
           "```\nxyz\n```"      (node {:tag :ofcblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})
           "> xyz"              (branch [{:tag :bq} {:tag :p} {:tag :txt :content "xyz"}])
           "xyz"                (branch [{:tag :p} {:tag :txt :content "xyz"}]))))

  (testing "Setext heading"
    (testing "minimal"
      (is (= (-> "abc\n===" from-string :children)
             [(branch [{:tag :stxh :level 1} {:tag :txt :content "abc"}])])))

    (testing "level"
      (are [s l] (= (-> s from-string :children)
                    [(branch [{:tag :stxh :level l} {:tag :txt :content "abc"}])])
           "abc\n===" 1
           "abc\n---" 2))

    (testing "coalesce"
      (is (= (-> "abc\n---\nxyz\n---" from-string :children)
             [(branch [{:tag :stxh :level 2} {:tag :txt :content "abc"}])
              (branch [{:tag :stxh :level 2} {:tag :txt :content "xyz"}])])))

    (testing "nested inline"
      (are [s t] (= (-> (str s "\n=====") from-string :children)
                    [(node {:tag :stxh :level 1}
                           t)])
           "*abc*"      [(branch [{:tag :em} {:tag :txt :content "abc"}])]
           "**abc**"    [(branch [{:tag :strong} {:tag :txt :content "abc"}])]
           "`abc`"      [(node {:tag :cs :content "abc"})]
           "[abc](xyz)" [(branch [{:tag :a :destination "xyz"} {:tag :txt :content "abc"}])]))

    (testing "deeply nested inline"
      (is (= (-> "qpr [*(**abc**)* `def`](xyz)\n============="
                 from-string
                 :children)
             [(node {:tag :stxh :level 1}
                    [(node {:tag :txt :content "qpr "})
                     (node {:tag :a :destination "xyz"}
                           [(node {:tag :em}
                                  [(node {:tag :txt :content "("})
                                   (node {:tag :strong}
                                         [(node {:tag :txt :content "abc"})])
                                   (node {:tag :txt :content ")"})])
                            (node {:tag :txt :content " "})
                            (node {:tag :cs :content "def"})])])])))

    (testing "multiline"
      (are [ls] (= (->> ls
                        (string/join "\n")
                        from-string
                        :children)
                   [(node {:tag :stxh :level 1}
                          [(node {:tag :txt :content "abc "})
                           (node {:tag :em}
                                 [(node {:tag :txt :content "xyz"})
                                  (node {:tag :sbr :content "\r\n"})
                                  (node {:tag :txt :content "123"})])])])
           ["abc *xyz" "123*"           "===="]
           ["abc *xyz" " 123*"          "===="]
           ["abc *xyz" "  123*"         "===="]
           ["abc *xyz" "   123*"        "===="]
           ["abc *xyz" "    123*"       "===="]
           ["abc *xyz" "     123*"      "===="]
           ["abc *xyz" "\t123*"         "===="]
           ["abc *xyz" "\t\t123*"       "===="]
           ["abc *xyz" "\t\t\t123*"     "===="]
           ["abc *xyz" "\t\t\t\t123*"   "===="]
           ["abc *xyz" "\t\t\t\t\t123*" "===="]))

    (testing "no text"
      (is (= (-> "===" from-string :children)
             [(branch [{:tag :p} {:tag :txt :content "==="}])])))

    (testing "leading whitespace alignment"
      (are [c u] (= (-> (str c \newline u) from-string :children)
                    [(branch [{:tag :stxh :level 1} {:tag :txt :content "abc"}])])
           "abc"    " ==="
           "abc"    "  ==="
           "abc"    "   ==="
           " abc"   " ==="
           " abc"   "  ==="
           " abc"   "   ==="
           "  abc"  " ==="
           "  abc"  "  ==="
           "  abc"  "   ==="
           "   abc" " ==="
           "   abc" "  ==="
           "   abc" "   ==="))

    (testing "hard line break at end of content line"
      (are [l c] (= (-> (str l "\n===") from-string :children)
                    [(branch [{:tag :stxh :level 1} {:tag :txt :content c}])])
           "abc  " "abc"
           "abc\\" "abc\\"))

    (testing "precedence vs inline markers"
      (testing "vs code span"
        (is (= (-> "`abc\n---\n`" from-string :children)
               [(branch [{:tag :stxh :level 2} {:tag :txt :content "`abc"}])
                (branch [{:tag :p} {:tag :txt :content "`"}])])))

      (testing "vs raw HTML"
        (is (= (-> "<a href=\"abc\n---\nxyz\"/>" from-string :children)
               [(branch [{:tag :stxh :level 2} {:tag :txt :content "<a href=\"abc"}])
                (branch [{:tag :p} {:tag :txt :content "xyz\"/>"}])]))))

    (testing "as lazy continuation line"
      (testing "in blockquote"
        (testing "level 1"
          (is (= (-> "> abc\n===" from-string :children)
                 [(branch [{:tag :bq} {:tag :p}]
                          [(node {:tag :txt :content "abc"})
                           (node {:tag :sbr :content "\r\n"})
                           (node {:tag :txt :content "==="})])])))

        (testing "level 2"
          (is (= (-> "> abc\n---" from-string :children)
                 [(branch [{:tag :bq} {:tag :p} {:tag :txt :content "abc"}])
                  (node {:tag :tbr :content "---"})]))))

      (testing "in multiline lazy blockquote"
        (testing "occurring once"
          (testing "level 1"
            (is (= (-> "> abc\nxyz\n===" from-string :children)
                   [(branch [{:tag :bq} {:tag :p}]
                            [(node {:tag :txt :content "abc"})
                             (node {:tag :sbr :content "\r\n"})
                             (node {:tag :txt :content "xyz"})
                             (node {:tag :sbr :content "\r\n"})
                             (node {:tag :txt :content "==="})])])))

          (testing "level 2"
            (is (= (-> "> abc\nxyz\n---" from-string :children)
                   [(branch [{:tag :bq} {:tag :p}]
                            [(node {:tag :txt :content "abc"})
                             (node {:tag :sbr :content "\r\n"})
                             (node {:tag :txt :content "xyz"})])
                    (node {:tag :tbr :content "---"})]))))

        (testing "occurring twice"
          (testing "both unmarked"
            (testing "level 1"
              (is (= (-> "> abc\n===\nxyz\n===" from-string :children)
                     [(branch [{:tag :bq} {:tag :p}]
                              [(node {:tag :txt :content "abc"})
                               (node {:tag :sbr :content "\r\n"})
                               (node {:tag :txt :content "==="})
                               (node {:tag :sbr :content "\r\n"})
                               (node {:tag :txt :content "xyz"})
                               (node {:tag :sbr :content "\r\n"})
                               (node {:tag :txt :content "==="})])])))

            (testing "level 2"
              (is (= (-> "> abc\n---\nxyz\n---" from-string :children)
                     [(branch [{:tag :bq} {:tag :p} {:tag :txt :content "abc"}])
                      (node {:tag :tbr :content "---"})
                      (branch [{:tag :stxh :level 2} {:tag :txt :content "xyz"}])]))))

          (testing "first marked, second unmarked"
            (testing "level 1"
              (is (= (-> "> abc\n> ===\nxyz\n===" from-string :children)
                     [(branch [{:tag :bq} {:tag :stxh :level 1} {:tag :txt :content "abc"}])
                      (branch [{:tag :stxh :level 1} {:tag :txt :content "xyz"}])])))

            (testing "level 2"
              (is (= (-> "> abc\n> ---\nxyz\n---" from-string :children)
                     [(branch [{:tag :bq} {:tag :stxh :level 2} {:tag :txt :content "abc"}])
                      (branch [{:tag :stxh :level 2} {:tag :txt :content "xyz"}])]))))

          (testing "first unmarked, second marked"
            (testing "level 1"
              (is (= (-> "> abc\n===\nxyz\n> ===" from-string :children)
                     [(branch [{:tag :bq} {:tag :stxh :level 1}]
                              [(node {:tag :txt :content "abc"})
                               (node {:tag :sbr :content "\r\n"})
                               (node {:tag :txt :content "==="})
                               (node {:tag :sbr :content "\r\n"})
                               (node {:tag :txt :content "xyz"})])])))

            (testing "level 2"
              (is (= (-> "> abc\n---\nxyz\n> ---" from-string :children)
                     [(branch [{:tag :bq} {:tag :p} {:tag :txt :content "abc"}])
                      (node {:tag :tbr :content "---"})
                      (branch [{:tag :p} {:tag :txt :content "xyz"}])
                      (branch [{:tag :bq} {:tag :tbr :content "---"}])]))))))

      (testing "in list item"
        (testing "level 1"
          (is (= (-> "- abc\n===" from-string :children)
                 [(branch [{:tag :list :type "bullet" :tight "true"} {:tag :li} {:tag :p}]
                          [(node {:tag :txt :content "abc"})
                           (node {:tag :sbr :content "\r\n"})
                           (node {:tag :txt :content "==="})])])))

        (testing "level 2"
          (is (= (-> "- abc\n---" from-string :children)
                 [(branch [{:tag :list :type "bullet" :tight "true"}
                           {:tag :li}
                           {:tag :p}
                           {:tag :txt :content "abc"}])
                  (node {:tag :tbr :content "---"})])))))

    (testing "preceded by block"
      (are [b n] (= (-> (str b "\nabc\n---") from-string :children)
                    [n
                     (node {:tag :stxh :level 2}
                           [(node {:tag :txt :content "abc"})])])
           "---"                (node {:tag :tbr :content "---"})
           "# xyz"              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])
           "    xyz"            (node {:tag :icblk :content "xyz\r\n"})
           "```\nxyz\n```"      (node {:tag :ofcblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})))

    (testing "followed by block"
      (are [b n] (= (-> (str "abc\n---\n" b) from-string :children)
                    [(node {:tag :stxh :level 2}
                           [(node {:tag :txt :content "abc"})])
                     n])
           "---"                (node {:tag :tbr :content "---"})
           "# xyz"              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])
           "    xyz"            (node {:tag :icblk :content "xyz\r\n"})
           "```\nxyz\n```"      (node {:tag :ofcblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})
           "> xyz"              (branch [{:tag :bq} {:tag :p} {:tag :txt :content "xyz"}])
           "- xyz"              (branch [{:tag :list :type "bullet" :tight "true"}
                                         {:tag :li}
                                         {:tag :p}
                                         {:tag :txt :content "xyz"}])))

    (testing "non-paragraph text line"
      (are [l n] (= (-> (str l "\n---") from-string :children)
                    [n
                     (node {:tag :tbr :content "---"})])
           "---"                (node {:tag :tbr :content "---"})
           "# xyz"              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])
           "    xyz"            (node {:tag :icblk :content "xyz\r\n"})
           "```\nxyz\n```"      (node {:tag :ofcblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})
           "> xyz"              (branch [{:tag :bq} {:tag :p} {:tag :txt :content "xyz"}])
           "- xyz"              (branch [{:tag :list :type "bullet" :tight "true"}
                                         {:tag :li}
                                         {:tag :p}
                                         {:tag :txt :content "xyz"}])))

    (testing "escaped block markers in text line"
      (are [b c] (= (-> (str b "\n---") from-string :children)
                    [(branch [{:tag :stxh :level 2} {:tag :txt :content c}])])
           "\\---"              "---"
           "\\# xyz"            "# xyz"
           "\\    xyz"          "\\    xyz"
           "\\<pre>xyz\\</pre>" "<pre>xyz</pre>"
           "\\> xyz"            "> xyz"
           "\\- xyz"            "- xyz"))

    (testing "text-before-text-after grey area"
      (is (= (-> "abc\nxyz\n---\n123" from-string :children)
             [(node {:tag :stxh :level 2}
                    [(node {:tag :txt :content "abc"})
                     (node {:tag :sbr :content "\r\n"})
                     (node {:tag :txt :content "xyz"})])
              (branch [{:tag :p} {:tag :txt :content "123"}])]))))

  (testing "indented code block"
    (testing "minimal"
      (are [s] (= (-> s from-string :children)
                  [(node {:tag :icblk :content "abc\r\n"})])
           "    abc"
           "\tabc"))

    (testing "content parsing"
      (is (= (-> "    *abc*\n    `xyz`\n    **123**\n    > def"
                 from-string
                 :children)
             [(node {:tag :icblk :content "*abc*\r\n`xyz`\r\n**123**\r\n> def\r\n"})])))

    (testing "blank lines within (less than 4 spaces of whitespace)"
      (are [l] (= (-> (string/join "\n" ["    abc" l "    xyz"])
                      from-string
                      :children)
                  [(node {:tag :icblk :content "abc\r\n\r\nxyz\r\n"})])
           ""
           " "
           "  "
           "   "
           "    "
           "\t"))

    (testing "spaces/tabs beyond 4"
      (testing "text"
        (are [s c] (= (-> s from-string :children)
                      [(node {:tag :icblk :content c})])
             "     abc"    " abc\r\n"
             "      abc"   "  abc\r\n"
             "       abc"  "   abc\r\n"
             "        abc" "    abc\r\n"
             "\t abc"      " abc\r\n"
             "\t  abc"     "  abc\r\n"
             "\t   abc"    "   abc\r\n"
             "\t    abc"   "    abc\r\n"
             "\t\tabc"     "\tabc\r\n"
             "\t\t\tabc"   "\t\tabc\r\n"
             "\t \tabc"    " \tabc\r\n"
             "\t\t abc"    "\t abc\r\n"))

      (testing "blank"
        (are [l c] (= (-> (str "\tabc\n" l "\n\txyz")
                          from-string
                          :children)
                      [(node {:tag :icblk
                              :content (str "abc\r\n" c "\r\nxyz\r\n")})])
             "     "   " "
             "      "  "  "
             "       " "   "
             "\t "     " "
             "\t  "    "  "
             "\t\t"    "\t"
             "\t \t"   " \t")))

    (testing "preceded by paragraph"
      (is (= (-> "abc\n    xyz" from-string :children)
             [(node {:tag :p}
                    [(node {:tag :txt :content "abc"})
                     (node {:tag :sbr :content "\r\n"})
                     (node {:tag :txt :content "xyz"})])])))

    (testing "followed by paragraph"
      (is (= (-> "    abc\nxyz" from-string :children)
             [(node {:tag :icblk :content "abc\r\n"})
              (branch [{:tag :p} {:tag :txt :content "xyz"}])])))

    (testing "preceded by block"
      (are [b n] (= (-> (str b "\n    abc") from-string :children)
                    [n
                     (node {:tag :icblk :content "abc\r\n"})])
           "---"                (node {:tag :tbr :content "---"})
           "# xyz"              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])
           "xyz\n---"           (branch [{:tag :stxh :level 2} {:tag :txt :content "xyz"}])
           "```\nxyz\n```"      (node {:tag :ofcblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})))

    (testing "followed by block"
      (are [b n] (= (-> (str "    abc\n" b) from-string :children)
                    [(node {:tag :icblk :content "abc\r\n"})
                     n])
           "---"                (node {:tag :tbr :content "---"})
           "# xyz"              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])
           "xyz\n---"           (branch [{:tag :stxh :level 2} {:tag :txt :content "xyz"}])
           "```\nxyz\n```"      (node {:tag :ofcblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})))

    (testing "preceded by indented blank line"
      (are [l] (= (-> (str l "\n    abc") from-string :children)
                  [(node {:tag :icblk :content "abc\r\n"})])
           "    "
           "     "
           "      "
           "\t"
           "\t\t"
           "\t\t\t"
           "\t "
           "\t  \t"))

    (testing "followed by indented blank line"
      (are [l] (= (-> (str "    abc\n" l) from-string :children)
                  [(node {:tag :icblk :content "abc\r\n"})])
           "    "
           "     "
           "      "
           "\t"
           "\t\t"
           "\t\t\t"
           "\t "
           "\t  \t"))

    (testing "trailing spaces/tabs"
      (are [s] (= (-> (str "    abc" s) from-string :children)
                  [(node {:tag :icblk :content (str "abc" s "\r\n")})])
           " "
           "  "
           "   "
           "\t"
           "\t\t"
           "\t\t\t"
           "\t "
           " \t"))

    (testing "list item ambiguity"
      (testing "first level"
        (is (= (-> "  - abc\n\n    xyz" from-string :children)
               [(branch [{:tag :list :type "bullet" :tight "false"} {:tag :li}]
                        [(branch [{:tag :p} {:tag :txt :content "abc"}])
                         (branch [{:tag :p} {:tag :txt :content "xyz"}])])])))

      (testing "second level"
        (is (= (-> "1.  abc\n\n    - xyz" from-string :children)
               [(branch [{:tag :list
                          :type "ordered"
                          :tight "false"
                          :delimiter "period"
                          :start "1"}
                         {:tag :li}]
                        [(branch [{:tag :p} {:tag :txt :content "abc"}])
                         (branch [{:tag :list :type "bullet" :tight "true"}
                                  {:tag :li}
                                  {:tag :p}
                                  {:tag :txt :content "xyz"}])])])))))

  (testing "fenced code block"
    (testing "minimal"
      (are [f] (= (-> (string/join "\n" [f "abc" f])
                      from-string
                      :children)
                  [(node {:tag :ofcblk :content "abc\r\n"})])
           "```"
           "````"
           "`````"
           "~~~"
           "~~~~"
           "~~~~~"))

    (testing "info string"
      (testing "without backtick"
        (are [i] (= (-> (str "``` " i "\nabc\n```")
                        from-string
                        :children)
               [(node {:tag :ofcblk :info i :content "abc\r\n"})])
             "xyz"
             "~xyz~"))

      (testing "with backtick"
        (testing "tilde fence"
          (is (= (-> "~~~ 12`34\nabc\n~~~" from-string :children)
                 [(node {:tag :ofcblk :info "12`34" :content "abc\r\n"})])))

        (testing "backtick fence"
          (is (= (-> "``` 12`34\nabc\n```" from-string :children)
                 [(node {:tag :p}
                        [(node {:tag :txt :content "``` 12`34"})
                         (node {:tag :sbr :content "\r\n"})
                         (node {:tag :txt :content "abc"})])
                  (node {:tag :ofcblk :content ""})])))))

    (testing "content parsing"
      (is (= (-> "```\n*abc*\n`xyz`\n**123**\n> def\n```"
                 from-string
                 :children)
             [(node {:tag :ofcblk :content "*abc*\r\n`xyz`\r\n**123**\r\n> def\r\n"})])))

    (testing "indentation removal"
      (is (= (-> " ```\n abc\n  xyz\n 123\n ```" from-string :children)
             [(node {:tag :ofcblk :content "abc\r\n xyz\r\n123\r\n"})])))

    (testing "unclosed"
      (testing "top-level"
        (is (= (-> "```\nabc" from-string :children)
               [(node {:tag :ofcblk :content "abc\r\n"})])))

      (testing "within a block"
        (is (= (-> "> ```\n> abc" from-string :children)
               [(branch [{:tag :bq} {:tag :ofcblk :content "abc\r\n"}])])))

      (testing "empty"
        (is (= (-> "```" from-string :children)
               [(node {:tag :ofcblk :content ""})]))))

    (testing "preceded by block"
      (are [b n] (= (-> (str b "\n```\nabc\n```") from-string :children)
                    [n
                     (node {:tag :ofcblk :content "abc\r\n"})])
           "---"                (node {:tag :tbr :content "---"})
           "# xyz"              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])
           "xyz\n---"           (branch [{:tag :stxh :level 2} {:tag :txt :content "xyz"}])
           "    xyz"            (node {:tag :icblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})
           "xyz"                (branch [{:tag :p} {:tag :txt :content "xyz"}])))

    (testing "followed by block"
      (are [b n] (= (-> (str "```\nabc\n```\n" b) from-string :children)
                    [(node {:tag :ofcblk :content "abc\r\n"})
                     n])
           "---"                (node {:tag :tbr :content "---"})
           "# xyz"              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])
           "xyz\n---"           (branch [{:tag :stxh :level 2} {:tag :txt :content "xyz"}])
           "    xyz"            (node {:tag :icblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})
           "xyz"                (branch [{:tag :p} {:tag :txt :content "xyz"}]))))

  (testing "blockquote"
    (testing "minimal"
      (are [s n] (= (-> s from-string :children)
                    [(node {:tag :bq}
                           n)])
           "> abc"               [(branch [{:tag :p} {:tag :txt :content "abc"}])]
           "> # abc"             [(branch [{:tag :atxh :level 1} {:tag :txt :content "abc"}])]
           "> abc\n> ---"        [(branch [{:tag :stxh :level 2} {:tag :txt :content "abc"}])]
           ">     abc"           [(node {:tag :icblk :content "abc\r\n"})]
           "> ```\n> abc\n> ```" [(node {:tag :ofcblk :content "abc\r\n"})]
           "> <pre>abc</pre>"    [(node {:tag :html-block :content "<pre>abc</pre>"})]
           "> - abc\n> - xyz"    [(node {:tag :list :type "bullet" :tight "true"}
                                        [(branch [{:tag :li} {:tag :p} {:tag :txt :content "abc"}])
                                         (branch [{:tag :li} {:tag :p} {:tag :txt :content "xyz"}])])]
           "> > abc"             [(branch [{:tag :bq} {:tag :p} {:tag :txt :content "abc"}])]))

    (testing "less 4 spaces of non-marker indentation"
      (is (= (-> ">    abc" from-string :children)
             [(branch [{:tag :bq} {:tag :p} {:tag :txt :content "abc"}])])))

    (testing "marker immediately followed by content"
      (is (= (-> ">abc" from-string :children)
             [(branch [{:tag :bq} {:tag :p} {:tag :txt :content "abc"}])])))

    (testing "lazy"
      (testing "paragraph continuation line"
        (are [s] (= (-> s from-string :children)
                    [(branch [{:tag :bq} {:tag :p}]
                             [(node {:tag :txt :content "abc"})
                              (node {:tag :sbr :content "\r\n"})
                              (node {:tag :txt :content "xyz"})
                              (node {:tag :sbr :content "\r\n"})
                              (node {:tag :txt :content "123"})])])
             "> abc\nxyz\n123"
             "> abc\n> xyz\n123"
             "> abc\nxyz\n> 123"))

      (testing "not paragraph continuation line"
        (is (= (-> "> abc\n---" from-string :children)
               [(branch [{:tag :bq} {:tag :p} {:tag :txt :content "abc"}])
                (node {:tag :tbr :content "---"})])))

      (testing "4 or more spaces of indentation"
        (are [s] (= (-> (str "> abc\n    " s)
                        from-string
                        :children)
                    [(branch [{:tag :bq} {:tag :p}]
                             [(node {:tag :txt :content "abc"})
                              (node {:tag :sbr :content "\r\n"})
                              (node {:tag :txt :content s})])])
             "- xyz"
             "# xyz"
             "> xyz"))

      (testing "blank line with marker, then paragraph"
        (is (= (-> "> abc\n>\nxyz" from-string :children)
               [(branch [{:tag :bq} {:tag :p} {:tag :txt :content "abc"}]
                      )
                (branch [{:tag :p} {:tag :txt :content "xyz"}])])))

      (testing "deep nesting"
        (are [s] (= (-> (str "> > > abc\n" s)
                        from-string
                        :children)
                    [(branch [{:tag :bq} {:tag :bq} {:tag :bq} {:tag :p}]
                             [(node {:tag :txt :content "abc"})
                              (node {:tag :sbr :content "\r\n"})
                              (node {:tag :txt :content "xyz"})])])
             "> > xyz"
             "> xyz"
             "xyz")))

    (testing "consecutive"
      (is (= (-> "> abc\n>\n> xyz" from-string :children)
             [(node {:tag :bq}
                    [(branch [{:tag :p} {:tag :txt :content "abc"}])
                     (branch [{:tag :p} {:tag :txt :content "xyz"}])])])))

    (testing "empty"
      (are [s] (= (-> s from-string :children)
                  [(node {:tag :bq})])
           ">"
           ">\n>  "
           ">\n>  \n> "))

    (testing "blank lines"
      (testing "inside"
        (is (= (-> ">\n> abc\n>" from-string :children)
               [(branch [{:tag :bq} {:tag :p} {:tag :txt :content "abc"}])])))

      (testing "outside"
        (is (= (-> "> abc\n\n> xyz" from-string :children)
               [(branch [{:tag :bq} {:tag :p} {:tag :txt :content "abc"}])
                (branch [{:tag :bq} {:tag :p} {:tag :txt :content "xyz"}])]))))

    (testing "preceded by block"
      (are [b n] (= (-> (str b "\n> abc") from-string :children)
                    [n
                     (branch [{:tag :bq} {:tag :p} {:tag :txt :content "abc"}])])
           "---"                (node {:tag :tbr :content "---"})
           "# xyz"              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])
           "xyz\n---"           (branch [{:tag :stxh :level 2} {:tag :txt :content "xyz"}])
           "    xyz"            (node {:tag :icblk :content "xyz\r\n"})
           "```\nxyz\n```"      (node {:tag :ofcblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})
           "- xyz"              (branch [{:tag :list :type "bullet" :tight "true"}
                                         {:tag :li}
                                         {:tag :p}
                                         {:tag :txt :content "xyz"}])
           "xyz"                (branch [{:tag :p} {:tag :txt :content "xyz"}])))

    (testing "followed by block"
      (are [b n] (= (-> (str "> abc\n" b) from-string :children)
                    [(branch [{:tag :bq} {:tag :p} {:tag :txt :content "abc"}])
                     n])
           "---"                (node {:tag :tbr :content "---"})
           "# xyz"              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])
           "```\nxyz\n```"      (node {:tag :ofcblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})
           "- xyz"              (branch [{:tag :list :type "bullet" :tight "true"}
                                          {:tag :li}
                                          {:tag :p}
                                          {:tag :txt :content "xyz"}]))))

  (testing "list"
    (testing "minimal"
      (are [s n] (= (-> s from-string :children)
                    [(branch [{:tag :list :type "bullet" :tight "true"} {:tag :li}]
                             n)])
           "- abc"               [(branch [{:tag :p} {:tag :txt :content "abc"}])]
           "- # abc"             [(branch [{:tag :atxh :level 1} {:tag :txt :content "abc"}])]
           "- abc\n  ---"        [(branch [{:tag :stxh :level 2} {:tag :txt :content "abc"}])]
           "-     abc"           [(node {:tag :icblk :content "abc\r\n"})]
           "- ```\n  abc\n  ```" [(node {:tag :ofcblk :content "abc\r\n"})]
           "- <pre>abc</pre>"    [(node {:tag :html-block :content "<pre>abc</pre>"})]
           "- > abc\n  > xyz"    [(branch [{:tag :bq} {:tag :p}]
                                          [(node {:tag :txt :content "abc"})
                                           (node {:tag :sbr :content "\r\n"})
                                           (node {:tag :txt :content "xyz"})])]
           "- - abc"             [(branch [{:tag :list :type "bullet" :tight "true"}
                                           {:tag :li}
                                           {:tag :p}
                                           {:tag :txt :content "abc"}])]))

    (testing "asymmetrical embedding"
      (is (= (-> "   > > 1. abc\n>>     xyz"
                 from-string
                 :children)
             [(branch [{:tag :bq}
                       {:tag :bq}
                       {:tag :list :type "ordered" :tight "true" :delimiter "period" :start "1"}
                       {:tag :li}
                       {:tag :p}]
                      [(node {:tag :txt :content "abc"})
                       (node {:tag :sbr :content "\r\n"})
                       (node {:tag :txt :content "xyz"})])])))

    (testing "tightness"
      (testing "no blank lines"
        (is (= (-> "- abc\n- xyz" from-string :children)
               [(node {:tag :list :type "bullet" :tight "true"}
                      [(branch [{:tag :li} {:tag :p} {:tag :txt :content "abc"}])
                       (branch [{:tag :li} {:tag :p} {:tag :txt :content "xyz"}])])])))

      (testing "first item begins with blank line"
        (is (= (-> "-\n  abc\n- xyz" from-string :children)
               [(node {:tag :list :type "bullet" :tight "true"}
                      [(branch [{:tag :li} {:tag :p} {:tag :txt :content "abc"}])
                       (branch [{:tag :li} {:tag :p} {:tag :txt :content "xyz"}])])])))

      (testing "last item ends with blank line"
        (is (= (-> "- abc\n- xyz\n\n" from-string :children)
               [(node {:tag :list :type "bullet" :tight "true"}
                      [(branch [{:tag :li} {:tag :p} {:tag :txt :content "abc"}])
                       (branch [{:tag :li} {:tag :p} {:tag :txt :content "xyz"}])])])))

      (testing "blank lines between items"
        (are [n] (= (-> (str "- abc" (string/join (repeat n "\n")) "- xyz")
                        from-string
                        :children)
                    [(node {:tag :list :type "bullet" :tight "false"}
                           [(branch [{:tag :li} {:tag :p} {:tag :txt :content "abc"}])
                            (branch [{:tag :li} {:tag :p} {:tag :txt :content "xyz"}])])])
             2 3 4))

      (testing "blank lines between block-level elements directly within item"
        (are [n] (= (-> (str "- abc" (string/join (repeat n "\n")) "  xyz")
                        from-string
                        :children)
                    [(branch [{:tag :list :type "bullet" :tight "false"} {:tag :li}]
                             [(branch [{:tag :p} {:tag :txt :content "abc"}])
                              (branch [{:tag :p} {:tag :txt :content "xyz"}])])])
             2 3 4))

      (testing "blank lines within block-level elements directly within item"
        (are [n] (= (-> (str "- ```\n  abc" (string/join (repeat n "\n")) "  xyz\n  ```")
                        from-string
                        :children)
                    [(branch [{:tag :list :type "bullet" :tight "true"}
                              {:tag :li}
                              {:tag :ofcblk :content (str "abc"
                                                          (string/join (repeat n "\r\n"))
                                                          "xyz\r\n")}])])
             2 3 4))

      (testing "blank lines between block-level elements indirectly within item"
        (are [n] (= (-> (str "- - abc" (string/join (repeat n "\n")) "    # xyz")
                        from-string
                        :children)
                    [(branch [{:tag :list :type "bullet" :tight "true"}
                              {:tag :li}
                              {:tag :list :type "bullet" :tight "false"}
                              {:tag :li}]
                             [(branch [{:tag :p} {:tag :txt :content "abc"}])
                              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])])])
             2 3 4))

      (testing "blank line when block-level elements directly within item are less than 3"
        (is (= (-> "-\n  abc" from-string :children)
               [(branch [{:tag :list :type "bullet" :tight "true"}
                         {:tag :li}
                         {:tag :p}
                         {:tag :txt :content "abc"}])])))

      (testing "blank line not between block-level elements directly within item"
        (is (= (-> "- abc\n" from-string :children)
               [(branch [{:tag :list :type "bullet" :tight "true"}
                         {:tag :li}
                         {:tag :p}
                         {:tag :txt :content "abc"}])]))))

    (testing "item indentation"
      (are [s] (= (->> s
                       from-string
                       :children
                       first
                       :children
                       (map (comp :tag :data)))
                  [:li :li :li :li :li :li :li])
           "- a\n - b\n  - c\n   - d\n  - e\n - f\n- g"
           "1. a\n 1. b\n  1. c\n   1. d\n  1. e\n 1. f\n1. g"))

    (testing "bullet"
      (testing "grouping"
        (testing "positive"
          (are [s] (= (-> s from-string :children)
                      [(node {:tag :list :type "bullet" :tight "true"}
                             [(branch [{:tag :li} {:tag :p} {:tag :txt :content "abc"}])
                              (branch [{:tag :li} {:tag :p} {:tag :txt :content "xyz"}])])])
               "- abc\n- xyz"
               "+ abc\n+ xyz"
               "* abc\n* xyz"))

        (testing "negative"
          (are [s] (= (->> s
                           from-string
                           :children
                           (map (comp #(select-keys % [:tag :type]) :data)))
                      [{:tag :list :type "bullet"}
                       {:tag :list :type "bullet"}])
               "- abc\n* xyz"
               "- abc\n+ xyz"
               "* abc\n- xyz"
               "* abc\n+ xyz"
               "+ abc\n- xyz"
               "+ abc\n* xyz"))))

    (testing "ordered"
      (testing "start"
        (are [s n] (= (-> s
                          from-string
                          (get-in [:children 0 :data])
                          (select-keys [:tag :type :start]))
                      {:tag :list
                       :type "ordered"
                       :start n})
             "1. abc"         "1"
             "3. abc"         "3"
             "7. abc\n1. xyz" "7"))

      (testing "delimiter"
        (testing "attribute"
          (are [s d] (= (-> s
                            from-string
                            (get-in [:children 0 :data])
                            (select-keys [:tag :type :delimiter]))
                        {:tag :list
                         :type "ordered"
                         :delimiter d})
               "1. abc" "period"
               "1) abc" "paren"))

        (testing "grouping"
          (is (= (->> "1. abc\n1) xyz"
                      from-string
                      :children
                      (map (comp #(select-keys % [:tag :type :delimiter]) :data)))
                 [{:tag :list :type "ordered" :delimiter "period"}
                  {:tag :list :type "ordered" :delimiter "paren"}]))))

      (testing "interrupt paragraph"
        (testing "start is 1"
          (are [s d] (= (-> s from-string :children)
                        [(branch [{:tag :p} {:tag :txt :content "abc"}])
                         (branch [{:tag :list
                                   :type "ordered"
                                   :tight "true"
                                   :delimiter d
                                   :start "1"}
                                  {:tag :li}
                                  {:tag :p}
                                  {:tag :txt :content "xyz"}])])
               "abc\n1. xyz" "period"
               "abc\n1) xyz" "paren"))

        (testing "start is not 1"
          (are [s t] (= (-> s from-string :children)
                        [(node {:tag :p}
                               [(node {:tag :txt :content "abc"})
                                (node {:tag :sbr :content "\r\n"})
                                (node {:tag :txt :content t})])])
               "abc\n2. xyz" "2. xyz"
               "abc\n2) xyz" "2) xyz"))))

    (testing "preceded by block"
      (are [b n] (= (-> (str b "\n- abc") from-string :children)
                    [n
                     (branch [{:tag :list :type "bullet" :tight "true"}
                              {:tag :li}
                              {:tag :p}
                              {:tag :txt :content "abc"}])])
           "---"                (node {:tag :tbr :content "---"})
           "# xyz"              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])
           "xyz\n---"           (branch [{:tag :stxh :level 2} {:tag :txt :content "xyz"}])
           "    xyz"            (node {:tag :icblk :content "xyz\r\n"})
           "```\nxyz\n```"      (node {:tag :ofcblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})
           "> xyz"              (branch [{:tag :bq} {:tag :p} {:tag :txt :content "xyz"}])
           "xyz"                (branch [{:tag :p} {:tag :txt :content "xyz"}])))

    (testing "followed by block"
      (are [b n] (= (-> (str "- abc\n" b) from-string :children)
                    [(branch [{:tag :list :type "bullet" :tight "true"}
                              {:tag :li}
                              {:tag :p}
                              {:tag :txt :content "abc"}])
                     n])
           "---"                (node {:tag :tbr :content "---"})
           "# xyz"              (branch [{:tag :atxh :level 1} {:tag :txt :content "xyz"}])
           "```\nxyz\n```"      (node {:tag :ofcblk :content "xyz\r\n"})
           "<pre>\nxyz\n</pre>" (node {:tag :html-block :content "<pre>\r\nxyz\r\n</pre>"})
           "> xyz"              (branch [{:tag :bq} {:tag :p} {:tag :txt :content "xyz"}]))))

  (testing "link"
    (testing "inline"
      (testing "minimal"
        (is (= (-> "[abc](xyz '123')"
                   from-string
                   (get-in [:children 0 :children 0]))
               (branch [{:tag :a :destination "xyz" :title "123"} {:tag :txt :content "abc"}]))))

      (testing "preceded by literal '!'"
        (is (= (-> "\\![abc](xyz)"
                   from-string
                   (get-in [:children 0]))
               (node {:tag :p}
                     [(node {:tag :txt :content "!"})
                      (branch [{:tag :a :destination "xyz"} {:tag :txt :content "abc"}])])))))

    (testing "reference"
      (testing "full"
        (testing "standard"
          (is (= (-> "[abc][qpr]\n\n[qpr]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0]))
                   (branch [{:tag :a :destination "xyz" :title "123"}
                            {:tag :txt :content "abc"}]))))

        (testing "inline content"
          (are [s t] (= (-> (str "[" s "][qpr]\n\n[qpr]: xyz '123'")
                            from-string
                            (get-in [:children 0 :children]))
                        [(node {:tag :a :destination "xyz" :title "123"}
                               [t])])
               "*abc*"   (branch [{:tag :em} {:tag :txt :content "abc"}])
               "`abc`"   (node {:tag :cs :content "abc"})
               "**abc**" (branch [{:tag :strong} {:tag :txt :content "abc"}])))

        (testing "inline image"
          (is (= (-> "[![abc](abc.png)][qpr]\n\n[qpr]: xyz '123'"
                     from-string
                     (get-in [:children 0 :children]))
                 [(branch [{:tag :a :destination "xyz" :title "123"}
                           {:tag :img :destination "abc.png"}
                           {:tag :txt :content "abc"}])])))

        (testing "match"
          (testing "case-insensitive"
            (is (= (-> "[abc][QpR]\n\n[qpr]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

          (testing "Unicode case folding"
            (testing "simple"
              (is (= (-> "[abc][ÄÖÕ]\n\n[äöõ]: xyz '123'"
                         from-string
                         (get-in [:children 0 :children 0 :data]))
                     {:tag :a :destination "xyz" :title "123"})))

            (testing "full"
              (are [lref ldef] (= (-> (str "[abc][" lref "]\n\n[" ldef "]: xyz '123'")
                              from-string
                              (get-in [:children 0 :children 0 :data]))
                          {:tag :a :destination "xyz" :title "123"})
                   "müß"  "müß"
                   "müss" "müß"
                   "MÜẞ"  "müß"
                   "MÜSS" "müß"
                   "müß"  "müss"
                   "müss" "müss"
                   "MÜẞ"  "müss"
                   "MÜSS" "müss"
                   "müß"  "MÜẞ"
                   "müss" "MÜẞ"
                   "MÜẞ"  "MÜẞ"
                   "MÜSS" "MÜẞ"
                   "müß"  "MÜSS"
                   "müss" "MÜSS"
                   "MÜẞ"  "MÜSS"
                   "MÜSS" "MÜSS")))

          (testing "whitespace"
            (is (= (-> "[abc][q \t\r\n p \t\r\n r]\n\n[q p r]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

          (testing "multiple"
            (is (= (-> "[abc][qpr]\n\n[qpr]: xyz '123'\n[qpr]: zyx '321'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

          (testing "backslash escapes"
            (is (= (-> "[abc][qpr\\!]\n\n[qpr!]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :txt :content "[abc][qpr!]"}))))

        (testing "image"
          (is (= (-> "![def][abc *xyz*]\n\n[abc *xyz*]: qpr '123'"
                     from-string
                     (get-in [:children 0 :children 0]))
                 (branch [{:tag :img :destination "qpr" :title "123"} {:tag :txt :content "def"}])))))

      (testing "collapsed"
        (testing "standard"
          (is (= (-> "[abc][]\n\n[abc]: xyz '123'"
                     from-string
                     (get-in [:children 0 :children 0]))
                 (branch [{:tag :a :destination "xyz" :title "123"} {:tag :txt :content "abc"}]))))

        (testing "inline content"
          (are [s t] (= (-> (str "[" s "][]\n\n[" s "]: xyz '123'")
                            from-string
                            (get-in [:children 0 :children]))
                        [(node {:tag :a :destination "xyz" :title "123"}
                               [t])])
               "*abc*"   (branch [{:tag :em} {:tag :txt :content "abc"}])
               "`abc`"   (node {:tag :cs :content "abc"})
               "**abc**" (branch [{:tag :strong} {:tag :txt :content "abc"}])))

        (testing "match"
          (testing "case-insensitive"
            (is (= (-> "[QpR][]\n\n[qpr]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

          (testing "Unicode"
            (is (= (-> "[ÄÖÕ][]\n\n[äöõ]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

          (testing "whitespace"
            (is (= (-> "[q \t\r\n p \t\r\n r][]\n\n[q p r]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

          (testing "multiple"
            (is (= (-> "[qpr][]\n\n[qpr]: xyz '123'\n[qpr]: zyx '321'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

          (testing "backslash escapes"
            (is (= (-> "[qpr\\!][]\n\n[qpr!]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :txt :content "[qpr!][]"}))))

        (testing "image"
          (is (= (-> "![abc *xyz*][]\n\n[abc *xyz*]: qpr '123'"
                     from-string
                     (get-in [:children 0 :children 0]))
                 (node {:tag :img :destination "qpr" :title "123"}
                       [(node {:tag :txt :content "abc "})
                        (branch [{:tag :em} {:tag :txt :content "xyz"}])])))))

      (testing "shortcut"
        (testing "standard"
          (is (= (-> "[abc]\n\n[abc]: xyz '123'"
                     from-string
                     (get-in [:children 0 :children 0]))
                 (branch [{:tag :a :destination "xyz" :title "123"} {:tag :txt :content "abc"}]))))

        (testing "inline content"
          (are [s t] (= (-> (str "[" s "]\n\n[" s "]: xyz '123'")
                            from-string
                            (get-in [:children 0 :children]))
                        [(node {:tag :a :destination "xyz" :title "123"}
                               [t])])
               "*abc*"   (branch [{:tag :em} {:tag :txt :content "abc"}])
               "`abc`"   (node {:tag :cs :content "abc"})
               "**abc**" (branch [{:tag :strong} {:tag :txt :content "abc"}])))

        (testing "match"
          (testing "case-insensitive"
            (is (= (-> "[QpR][]\n\n[qpr]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

          (testing "Unicode"
            (is (= (-> "[ÄÖÕ][]\n\n[äöõ]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

          (testing "whitespace"
            (is (= (-> "[q \t\r\n p \t\r\n r]\n\n[q p r]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

          (testing "multiple"
            (is (= (-> "[qpr]\n\n[qpr]: xyz '123'\n[qpr]: zyx '321'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

          (testing "backslash escapes"
            (is (= (-> "[qpr\\!]\n\n[qpr!]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :txt :content "[qpr!]"}))))

        (testing "image"
          (is (= (-> "![abc *xyz*]\n\n[abc *xyz*]: qpr '123'"
                     from-string
                     (get-in [:children 0 :children 0]))
                 (node {:tag :img :destination "qpr" :title "123"}
                       [(node {:tag :txt :content "abc "})
                        (branch [{:tag :em} {:tag :txt :content "xyz"}])]))))

        (testing "followed by label"
          (is (= (-> "[abc][xyz][123]\n\n[123]: dest-123 'title-123'\n[abc]: dest-abc 'title-abc'"
                     from-string
                     (get-in [:children 0 :children]))
                 [(node {:tag :txt :content "[abc]"})
                  (branch [{:tag :a :destination "dest-123" :title "title-123"}
                           {:tag :txt :content "xyz"}])]))))

      (testing "no match"
        (are [s] (= (-> s from-string (get-in [:children 0 :children]))
                    [(node {:tag :txt :content s})])
             "[abc][xyz]"
             "[abc][]"
             "[abc]"))

      (testing "surrounding inline content"
        (are [r l] (= (-> (str "before " r " after\n\n[" l "]: xyz '123'")
                          from-string
                          (get-in [:children 0 :children]))
                    [(node {:tag :txt :content "before "})
                     (branch [{:tag :a :destination "xyz" :title "123"}
                              {:tag :txt :content "abc"}])
                     (node {:tag :txt :content " after"})])
             "[abc][lbl]" "lbl"
             "[abc][]"    "abc"
             "[abc]"      "abc"))))

  (testing "image"
    (testing "inline"
      (testing "description, destination and title"
        (is (= (-> "![abc](/xyz \"123\")"
                   from-string
                   (get-in [:children 0 :children]))
               [(branch [{:tag :img :destination "/xyz" :title "123"} {:tag :txt :content "abc"}])])))

      (testing "image within image"
        (is (= (-> "![txt ![abc](/xyz)](/123)"
                   from-string
                   (get-in [:children 0 :children]))
               [(node {:tag :img :destination "/123"}
                      [(node {:tag :txt :content "txt "})
                       (branch [{:tag :img :destination "/xyz"} {:tag :txt :content "abc"}])])])))

      (testing "link within image"
        (is (= (-> "![txt [abc](/xyz)](/123)"
                   from-string
                   (get-in [:children 0 :children]))
               [(node {:tag :img :destination "/123"}
                      [(node {:tag :txt :content "txt "})
                       (branch [{:tag :a :destination "/xyz"} {:tag :txt :content "abc"}])])])))))

  (testing "nested inline"
    (testing "text within emphasis"
      (is (= (-> "*abc*" from-string (get-in [:children 0 :children]))
             [(branch [{:tag :em} {:tag :txt :content "abc"}])])))

    (testing "deep nesting, broad nesting"
      (testing "2 levels"
        (is (= (-> "*a1 *b1* *b2* a2*"
                   from-string
                   (get-in [:children 0 :children]))
               [(node {:tag :em}
                      [(node {:tag :txt :content "a1 "})
                       (branch [{:tag :em} {:tag :txt :content "b1"}])
                       (node {:tag :txt :content " "})
                       (branch [{:tag :em} {:tag :txt :content "b2"}])
                       (node {:tag :txt :content " a2"})])])))

      (testing "3 levels"
        (is (= (-> "*a1 *b1 *c1* *c2* b2* *b3 *c3* *c4* b4* a2*"
                   from-string
                   (get-in [:children 0 :children]))
               [(node {:tag :em}
                      [(node {:tag :txt :content "a1 "})
                       (node {:tag :em}
                             [(node {:tag :txt :content "b1 "})
                              (branch [{:tag :em} {:tag :txt :content "c1"}])
                              (node {:tag :txt :content " "})
                              (branch [{:tag :em} {:tag :txt :content "c2"}])
                              (node {:tag :txt :content " b2"})])
                       (node {:tag :txt :content " "})
                       (node {:tag :em}
                             [(node {:tag :txt :content "b3 "})
                              (branch [{:tag :em} {:tag :txt :content "c3"}])
                              (node {:tag :txt :content " "})
                              (branch [{:tag :em} {:tag :txt :content "c4"}])
                              (node {:tag :txt :content " b4"})])
                       (node {:tag :txt :content " a2"})])]))))

    (testing "nested __-strong emphasis"
      (is (= (-> "__abc, __xyz__, pqr__"
                 from-string
                 (get-in [:children 0 :children]))
             [(node {:tag :strong}
                    [(node {:tag :txt :content "abc, "})
                     (branch [{:tag :strong} {:tag :txt :content "xyz"}])
                     (node {:tag :txt :content ", pqr"})])])))

    (testing "nested *-emphasis"
      (is (= (-> "*(*abc*)* xyz *pqr*"
                 from-string
                 (get-in [:children 0 :children]))
             [(node {:tag :em}
                    [(node {:tag :txt :content "("})
                     (branch [{:tag :em} {:tag :txt :content "abc"}])
                     (node {:tag :txt :content ")"})])
              (node {:tag :txt :content " xyz "})
              (branch [{:tag :em} {:tag :txt :content "pqr"}])])))

    (testing "deeply nested emphasis"
      (testing "delimiter run size: 3"
        (are [s] (= (-> s from-string (get-in [:children 0 :children]))
                    [(branch [{:tag :em} {:tag :strong} {:tag :txt :content "abc"}])])
             "***abc***"
             "___abc___"))

      (testing "delimiter run size: 4"
        (are [s] (= (-> s from-string (get-in [:children 0 :children]))
                    [(branch [{:tag :strong} {:tag :strong} {:tag :txt :content "abc"}])])
             "****abc****"
             "____abc____"))

      (testing "delimiter run size: 5"
        (are [s] (= (-> s from-string (get-in [:children 0 :children]))
                    [(branch [{:tag :em} {:tag :strong} {:tag :strong} {:tag :txt :content "abc"}])])
             "*****abc*****"
             "_____abc_____"))

      (testing "delimiter run size: 6"
        (are [s] (= (-> s from-string (get-in [:children 0 :children]))
                    [(branch [{:tag :strong} {:tag :strong} {:tag :strong} {:tag :txt :content "abc"}])])
             "******abc******"
             "______abc______")))

    (testing "emphasis delimiter runs: multiples of 3"
      (testing "sum is, parts aren't"
        (are [s c] (= c (-> s from-string (get-in [:children 0 :children])))
             "*abc**123**xyz*" [(node {:tag :em}
                                      [(node {:tag :txt :content "abc"})
                                       (branch [{:tag :strong} {:tag :txt :content "123"}])
                                       (node {:tag :txt :content "xyz"})])]
             "_abc__123__xyz_" [(branch [{:tag :em} {:tag :txt :content "abc__123__xyz"}])]))

      (testing "sum is, parts are"
        (are [s c] (= c (-> s from-string (get-in [:children 0 :children])))
             "***abc***123***xyz***" [(branch [{:tag :em} {:tag :strong} {:tag :txt :content "abc"}])
                                      (node {:tag :txt :content "123"})
                                      (branch [{:tag :em} {:tag :strong} {:tag :txt :content "xyz"}])]
             "___abc___123___xyz___" [(branch [{:tag :em} {:tag :strong} {:tag :txt :content "abc___123___xyz"}])]))))

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
                    [(node {:tag :cs :content c})])
           "`abc  \nxyz`" "abc   xyz"
           "`abc\\\nxyz`" "abc\\ xyz"))

    (testing "inside raw HTML"
      (are [s] (= (-> s from-string (get-in [:children 0 :children]))
                    [(node {:tag :html-inline :content s})])
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

  (testing "soft line break"
    (testing "standard"
      (is (= (-> "abc\nxyz" from-string (get-in [:children 0 :children]))
             [(node {:tag :txt :content "abc"})
              (node {:tag :sbr :content "\r\n"})
              (node {:tag :txt :content "xyz"})])))

    (testing "spaces at beginning of next line"
      (is (= (-> "abc\n xyz" from-string (get-in [:children 0 :children]))
             [(node {:tag :txt :content "abc"})
              (node {:tag :sbr :content "\r\n"})
              (node {:tag :txt :content "xyz"})])))

    (testing "inside emphasis"
      (is (= (-> "*abc\nxyz*" from-string (get-in [:children 0 :children]))
             [(node {:tag :em}
                    [(node {:tag :txt :content "abc"})
                     (node {:tag :sbr :content "\r\n"})
                     (node {:tag :txt :content "xyz"})])])))

    (testing "inside code span"
      (is (= (-> "`abc\nxyz`" from-string (get-in [:children 0 :children]))
             [(node {:tag :cs :content "abc xyz"})])))

    (testing "inside raw HTML"
      (are [e] (= (-> (str "<a href=\"xyz" e "abc\">")
                      from-string
                      (get-in [:children 0 :children 0]))
                  (node {:tag :html-inline :content "<a href=\"xyz\r\nabc\">"}))
           "\n"
;          "\r"
           "\r\n"))

    (testing "inside inline link"
      (are [e] (= (-> (str "[abc" e "xyz](123)")
                      from-string
                      (get-in [:children 0 :children 0]))
                  (node {:tag :a :destination "123"}
                        [(node {:tag :txt :content "abc"})
                         (node {:tag :sbr :content "\r\n"})
                         (node {:tag :txt :content "xyz"})]))
           "\n"
           "\r"
           "\r\n")))

  (testing "markdown between HTML tags"
    (testing "blank lines inbetween"
      (is (= (-> "<del>\n\n*abc*\n\n</del>" from-string :children)
             [(node {:tag :html-block :content "<del>"})
              (branch [{:tag :p} {:tag :em} {:tag :txt :content "abc"}])
              (node {:tag :html-block :content "</del>"})])))

    (testing "on adjacent lines"
      (is (= (-> "<del>\n*abc*\n</del>" from-string :children)
             [(node {:tag :html-block :content "<del>\r\n*abc*\r\n</del>"})])))

    (testing "on the same line"
      (is (= (-> "<del>*abc*</del>" from-string :children)
             [(node {:tag :p}
                    [(node {:tag :html-inline :content "<del>"})
                     (branch [{:tag :em} {:tag :txt :content "abc"}])
                     (node {:tag :html-inline :content "</del>"})])]))))

  (testing "post-processing"
    (testing "hard line break at end of block"
      (are [s cs] (= cs (-> s from-string (get-in [:children 0 :children])))
           "*abc*  "    [(branch [{:tag :em} {:tag :txt :content "abc"}])]
           "abc  "      [(node {:tag :txt :content "abc"})]
           "  "         nil
           "*abc*\\"    [(branch [{:tag :em} {:tag :txt :content "abc"}])
                         (node {:tag :txt :content "\\"})]
           "abc\\"      [(node {:tag :txt :content "abc\\"})]
           "\\"         [(node {:tag :txt :content "\\"})]
           "## *abc*  " [(branch [{:tag :em} {:tag :txt :content "abc"}])]
           "## abc  "   [(node {:tag :txt :content "abc"})]
           "##   "      nil
           "## *abc*\\" [(branch [{:tag :em} {:tag :txt :content "abc"}])
                         (node {:tag :txt :content "\\"})]
           "## abc\\"   [(node {:tag :txt :content "abc\\"})]
           "## \\"      [(node {:tag :txt :content "\\"})]))

    (testing "block containing only a hard line break"
      (is (nil? (-> "  " from-string (get-in [:children 0 :children])))))

    (testing "empty paragraph"
      (is (= (map (comp :tag :data)
                  (-> "# abc\r\n\r\n  \r\n> xyz" from-string :children))
             [:atxh :bq]))))

  (testing "backslash escapes"
    (testing "ASCII punctuation"
      (are [c] (= (-> (str "abc\\" c "xyz") from-string :children)
                  [(branch [{:tag :p} {:tag :txt :content (str "abc" c "xyz")}])])
           \!
           \"
           \#
           \$
           \%
           \&
           \'
           \(
           \)
           \*
           \+
           \,
           \-
           \.
           \/
           \:
           \;
           \<
           \=
           \>
           \?
           \@
           \[
           \\
           \]
           \^
           \_
           \`
           \{
           \|
           \}
           \~))

    (testing "not ASCII punctuation"
      (are [c] (= (-> (str "abc\\" c "xyz") from-string :children)
                  [(branch [{:tag :p} {:tag :txt :content (str "abc\\" c "xyz")}])])
           \→
           \A
           \a
           \space
           \3
           \φ
           \«))

    (testing "entity markers"
      (are [m] (= (-> (str \\ m) from-string :children)
                  [(branch [{:tag :p} {:tag :txt :content m}])])
           "*abc*"
           "<br/>abc"
           "[abc](xyz)"
           "`abc`"
           "- abc"
           "# abc"
           "[abc]: xyz '123'"
           "&ouml;"))

    (testing "in code span"
      (is (= (-> "`` \\[\\` ``" from-string :children)
             [(branch [{:tag :p} {:tag :cs :content "\\[\\`"}])])))

    (testing "in indented code block"
      (is (= (-> "    \\[\\]" from-string :children)
             [(node {:tag :icblk :content "\\[\\]\r\n"})])))

    (testing "in fenced code block"
      (is (= (-> "~~~\n\\[\\]\n~~~" from-string :children)
             [(node {:tag :ofcblk :content "\\[\\]\r\n"})])))

    (testing "in autolink"
      (is (= (-> "<http://abc.com?q=\\*>" from-string :children)
             [(branch [{:tag :p}
                       {:tag :a :destination "http://abc.com?q=%5C*"}
                       {:tag :txt :content "http://abc.com?q=\\*"}])])))

    (testing "in raw HTML"
      (is (= (-> "<a href=\"abc\\/)\">" from-string :children)
             [(node {:tag :html-block :content "<a href=\"abc\\/)\">"})])))

    (testing "in link destination"
      (are [s] (= (-> s from-string :children)
                  [(branch [{:tag :p}
                            {:tag :a :destination "xyz*"}
                            {:tag :txt :content "abc"}])])
           "[abc](xyz\\*)"
           "[abc]\n\n[abc]: xyz\\*"))

    (testing "in link title"
      (are [s] (= (-> s from-string :children)
                  [(branch [{:tag :p}
                            {:tag :a :destination "xyz" :title "12*3"}
                            {:tag :txt :content "abc"}])])
           "[abc](xyz '12\\*3')"
           "[abc]\n\n[abc]: xyz '12\\*3'"))

    (testing "in fenced code block info string"
      (is (= (-> "``` abc\\+xyz\n123\n```" from-string :children)
             [(node {:tag :ofcblk :info "abc+xyz" :content "123\r\n"})])))

    (testing "backslash-escaped backslash escape"
      (are [m t] (= (-> (str \\ \\ m)
                        from-string
                        (get-in [:children 0 :children]))
                    t)
           "*abc*"      [(node {:tag :txt :content "\\"})
                         (branch [{:tag :em} {:tag :txt :content "abc"}])]
           "<abc/>"     [(node {:tag :txt :content "\\"})
                         (node {:tag :html-inline, :content "<abc/>"})]
           "[abc](xyz)" [(node {:tag :txt :content "\\"})
                         (branch [{:tag :a :destination "xyz"} {:tag :txt :content "abc"}])]
           "`abc`"      [(node {:tag :txt :content "\\"})
                         (node {:tag :cs :content "abc"})]
           "- abc"      [(node {:tag :txt :content "\\- abc"})]
           "# abc"      [(node {:tag :txt :content "\\# abc"})]
           "&ouml;"     [(node {:tag :txt :content "\\ö"})]))

    (testing "ordered list"
      (is (= (-> "1\\. abc" from-string :children)
             [(branch [{:tag :p} {:tag :txt :content "1. abc"}])])))))

