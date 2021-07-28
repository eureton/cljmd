(ns commonmark.ast-test
  (:require [clojure.test :refer :all]
            [commonmark.ast :refer :all]
            [commonmark.ast.common :refer [node]]))

(deftest from-string-test
  (testing "minimal"
    (is (= (-> "abc" from-string :children)
           [(node {:tag :p}
                  [(node {:tag :txt :content "abc"})])])))

  (testing "link"
    (testing "inline"
      (testing "minimal"
        (is (= (-> "[abc](xyz '123')"
                   from-string
                   (get-in [:children 0 :children 0]))
               (node {:tag :a :destination "xyz" :title "123"}
                     [(node {:tag :txt :content "abc"})]))))

      (testing "preceded by literal '!'"
        (is (= (-> "\\![abc](xyz)"
                   from-string
                   (get-in [:children 0]))
               (node {:tag :p}
                     [(node {:tag :txt :content "!"})
                      (node {:tag :a :destination "xyz"}
                            [(node {:tag :txt :content "abc"})])])))))

    (testing "reference"
      (testing "full"
        (testing "standard"
          (is (= (-> "[abc][qpr]\n\n[qpr]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0]))
                   (node {:tag :a :destination "xyz" :title "123"}
                         [(node {:tag :txt :content "abc"})]))))

        (testing "inline content"
          (are [s t] (= (-> (str "[" s "][qpr]\n\n[qpr]: xyz '123'")
                            from-string
                            (get-in [:children 0 :children]))
                        [(node {:tag :a :destination "xyz" :title "123"}
                               [t])])
               "*abc*"   (node {:tag :em}
                               [(node {:tag :txt :content "abc"})])
               "`abc`"   (node {:tag :cs :content "abc"})
               "**abc**" (node {:tag :strong}
                               [(node {:tag :txt :content "abc"})])))

        (testing "inline image"
          (is (= (-> "[![abc](abc.png)][qpr]\n\n[qpr]: xyz '123'"
                     from-string
                     (get-in [:children 0 :children]))
                 [(node {:tag :a :destination "xyz" :title "123"}
                        [(node {:tag :img :destination "abc.png"}
                               [(node {:tag :txt :content "abc"})])])])))

        (testing "match"
          (testing "case-insensitive"
            (is (= (-> "[abc][QpR]\n\n[qpr]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

          (testing "Unicode"
            (is (= (-> "[abc][ÄÖÕ]\n\n[äöõ]: xyz '123'"
                       from-string
                       (get-in [:children 0 :children 0 :data]))
                   {:tag :a :destination "xyz" :title "123"})))

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
                 (node {:tag :img :destination "qpr" :title "123"}
                       [(node {:tag :txt :content "def"})])))))

      (testing "collapsed"
        (testing "standard"
          (is (= (-> "[abc][]\n\n[abc]: xyz '123'"
                     from-string
                     (get-in [:children 0 :children 0]))
                 (node {:tag :a :destination "xyz" :title "123"}
                       [(node {:tag :txt :content "abc"})]))))

        (testing "inline content"
          (are [s t] (= (-> (str "[" s "][]\n\n[" s "]: xyz '123'")
                            from-string
                            (get-in [:children 0 :children]))
                        [(node {:tag :a :destination "xyz" :title "123"}
                               [t])])
               "*abc*"   (node {:tag :em}
                               [(node {:tag :txt :content "abc"})])
               "`abc`"   (node {:tag :cs :content "abc"})
               "**abc**" (node {:tag :strong}
                               [(node {:tag :txt :content "abc"})])))

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
                        (node {:tag :em}
                              [(node {:tag :txt :content "xyz"})])])))))

      (testing "shortcut"
        (testing "standard"
          (is (= (-> "[abc]\n\n[abc]: xyz '123'"
                     from-string
                     (get-in [:children 0 :children 0]))
                 (node {:tag :a :destination "xyz" :title "123"}
                       [(node {:tag :txt :content "abc"})]))))

        (testing "inline content"
          (are [s t] (= (-> (str "[" s "]\n\n[" s "]: xyz '123'")
                            from-string
                            (get-in [:children 0 :children]))
                        [(node {:tag :a :destination "xyz" :title "123"}
                               [t])])
               "*abc*"   (node {:tag :em}
                               [(node {:tag :txt :content "abc"})])
               "`abc`"   (node {:tag :cs :content "abc"})
               "**abc**" (node {:tag :strong}
                               [(node {:tag :txt :content "abc"})])))

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
                        (node {:tag :em}
                              [(node {:tag :txt :content "xyz"})])]))))

        (testing "followed by label"
          (is (= (-> "[abc][xyz][123]\n\n[123]: dest-123 'title-123'\n[abc]: dest-abc 'title-abc'"
                     from-string
                     (get-in [:children 0 :children]))
                 [(node {:tag :txt :content "[abc]"})
                  (node {:tag :a :destination "dest-123" :title "title-123"}
                        [(node {:tag :txt :content "xyz"})])]))))

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
                     (node {:tag :a :destination "xyz" :title "123"}
                           [(node {:tag :txt :content "abc"})])
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
               [(node {:tag :img :destination "/xyz" :title "123"}
                      [(node {:tag :txt :content "abc"})])])))

      (testing "image within image"
        (is (= (-> "![txt ![abc](/xyz)](/123)"
                   from-string
                   (get-in [:children 0 :children]))
               [(node {:tag :img :destination "/123"}
                      [(node {:tag :txt :content "txt "})
                       (node {:tag :img :destination "/xyz"}
                             [(node {:tag :txt :content "abc"})])])])))

      (testing "link within image"
        (is (= (-> "![txt [abc](/xyz)](/123)"
                   from-string
                   (get-in [:children 0 :children]))
               [(node {:tag :img :destination "/123"}
                      [(node {:tag :txt :content "txt "})
                       (node {:tag :a :destination "/xyz"}
                             [(node {:tag :txt :content "abc"})])])])))))

  (testing "nested inline"
    (testing "text within emphasis"
      (is (= (-> "*abc*" from-string (get-in [:children 0 :children]))
             [(node {:tag :em}
                    [(node {:tag :txt :content "abc"})])])))

    (testing "nested __-strong emphasis"
      (is (= (-> "__abc, __xyz__, pqr__"
                 from-string
                 (get-in [:children 0 :children]))
             [(node {:tag :strong}
                    [(node {:tag :txt :content "abc, "})
                     (node {:tag :strong}
                           [(node {:tag :txt :content "xyz"})])
                     (node {:tag :txt :content ", pqr"})])])))

    (testing "nested *-emphasis"
      (is (= (-> "*(*abc*)* xyz *pqr*"
                 from-string
                 (get-in [:children 0 :children]))
             [(node {:tag :em}
                    [(node {:tag :txt :content "("})
                     (node {:tag :em}
                           [(node {:tag :txt :content "abc"})])
                     (node {:tag :txt :content ")"})])
              (node {:tag :txt :content " xyz "})
              (node {:tag :em}
                    [(node {:tag :txt :content "pqr"})])]))))

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
             [(node {:tag :html-block
                     :content "<del>"})
              (node {:tag :p}
                    [(node {:tag :em}
                           [(node {:tag :txt :content "abc"})])])
              (node {:tag :html-block
                     :content "</del>"})])))

    (testing "on adjacent lines"
      (is (= (-> "<del>\n*abc*\n</del>" from-string :children)
             [(node {:tag :html-block
                     :content "<del>\r\n*abc*\r\n</del>"})])))

    (testing "on the same line"
      (is (= (-> "<del>*abc*</del>" from-string :children)
             [(node {:tag :p}
                    [(node {:tag :html-inline :content "<del>"})
                     (node {:tag :em}
                           [(node {:tag :txt :content "abc"})])
                     (node {:tag :html-inline :content "</del>"})])]))))

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
             [:li :bq]))))

  (testing "backslash escapes"
    (testing "ASCII punctuation"
      (are [c] (= (-> (str "abc\\" c "xyz") from-string :children)
                  [(node {:tag :p}
                         [(node {:tag :txt :content (str "abc" c "xyz")})])])
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
                  [(node {:tag :p}
                         [(node {:tag :txt :content (str "abc\\" c "xyz")})])])
           \→
           \A
           \a
           \space
           \3
           \φ
           \«))

    (testing "entity markers"
      (are [m] (= (-> (str \\ m) from-string :children)
                  [(node {:tag :p}
                         [(node {:tag :txt :content m})])])
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
             [(node {:tag :p}
                    [(node {:tag :cs :content "\\[\\`"})])])))

    (testing "in indented code block"
      (is (= (-> "    \\[\\]" from-string :children)
             [(node {:tag :icblk :content "\\[\\]"})])))

    (testing "in fenced code block"
      (is (= (-> "~~~\n\\[\\]\n~~~" from-string :children)
             [(node {:tag :ofcblk :content "\\[\\]"})])))

    (testing "in autolink"
      (is (= (-> "<http://abc.com?q=\\*>" from-string :children)
             [(node {:tag :p}
                    [(node {:tag :a :destination "http://abc.com?q=%5C*"}
                           [(node {:tag :txt :content "http://abc.com?q=\\*"})])])])))

    (testing "in raw HTML"
      (is (= (-> "<a href=\"abc\\/)\">" from-string :children)
             [(node {:tag :html-block :content "<a href=\"abc\\/)\">"})])))

    (testing "in link destination"
      (are [s] (= (-> s from-string :children)
                  [(node {:tag :p}
                         [(node {:tag :a :destination "xyz*"}
                                [(node {:tag :txt :content "abc"})])])])
           "[abc](xyz\\*)"
           "[abc]\n\n[abc]: xyz\\*"))

    (testing "in link title"
      (are [s] (= (-> s from-string :children)
                  [(node {:tag :p}
                         [(node {:tag :a :destination "xyz" :title "12*3"}
                                [(node {:tag :txt :content "abc"})])])])
           "[abc](xyz '12\\*3')"
           "[abc]\n\n[abc]: xyz '12\\*3'"))

    (testing "in fenced code block info string"
      (is (= (-> "``` abc\\+xyz\n123\n```" from-string :children)
             [(node {:tag :ofcblk :info "abc+xyz" :content "123"})])))

    (testing "backslash-escaped backslash escape"
      (are [m t] (= (-> (str \\ \\ m)
                        from-string
                        (get-in [:children 0 :children]))
                    t)
           "*abc*"      [(node {:tag :txt :content "\\"})
                         (node {:tag :em}
                               [(node {:tag :txt :content "abc"})])]
           "<abc/>"     [(node {:tag :txt :content "\\"})
                         (node {:tag :html-inline, :content "<abc/>"})]
           "[abc](xyz)" [(node {:tag :txt :content "\\"})
                         (node {:tag :a :destination "xyz"}
                               [(node {:tag :txt :content "abc"})])]
           "`abc`"      [(node {:tag :txt :content "\\"})
                         (node {:tag :cs :content "abc"})]
           "- abc"      [(node {:tag :txt :content "\\- abc"})]
           "# abc"      [(node {:tag :txt :content "\\# abc"})]
           "&ouml;"     [(node {:tag :txt :content "\\&ouml;"})]))

    (testing "ordered list"
      (is (= (-> "1\\. abc" from-string :children)
             [(node {:tag :p}
                    [(node {:tag :txt :content "1. abc"})])])))))

