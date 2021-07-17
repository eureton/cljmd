(ns commonmark.ast-test
  (:require [clojure.test :refer :all]
            [commonmark.ast :refer :all]
            [commonmark.ast.common :refer [node]]))

(deftest from-string-test
  (testing "minimal"
    (is (= (from-string "abc")
           (node {:tag :doc}
                 [(node {:tag :p}
                        [(node {:tag :txt
                                :content "abc"})])]))))

  (testing "link"
    (testing "inline"
      (testing "minimal"
        (is (= (from-string "[abc](xyz '123')")
               (node {:tag :doc}
                     [(node {:tag :p}
                            [(node {:tag :a
                                    :destination "xyz"
                                    :title "123"}
                                  [(node {:tag :txt
                                          :content "abc"})])])]))))

      (testing "preceded by literal '!'"
        (is (= (from-string "\\![abc](xyz)")
               (node {:tag :doc}
                     [(node {:tag :p}
                            [(node {:tag :txt
                                         :content "!"})
                             (node {:tag :a
                                    :destination "xyz"}
                                   [(node {:tag :txt
                                           :content "abc"})])])])))))

    (testing "reference"
      (testing "standard"
        (let [p-tree (node {:tag :p}
                           [(node {:tag :a
                                   :destination "xyz"
                                   :title "123"}
                                  [(node {:tag :txt
                                          :content "abc"})])])]
          (testing "full"
            (is (= (from-string "[abc][qpr]\n\n[qpr]: xyz '123'")
                   (node {:tag :doc}
                         [p-tree]))))

          (testing "collapsed"
            (is (= (from-string "[abc][]\n\n[abc]: xyz '123'")
                   (node {:tag :doc}
                         [p-tree]))))

          (testing "shortcut"
            (is (= (from-string "[abc]\n\n[abc]: xyz '123'")
                   (node {:tag :doc}
                         [p-tree]))))))

      (testing "full"
        (testing "inline content"
          (are [s t] (= (-> (str "[" s "][qpr]\n\n[qpr]: xyz '123'")
                            from-string
                            (get-in [:children 0 :children]))
                        [(node {:tag :a :destination "xyz" :title "123"}
                               [(node {:tag t}
                                      [(node {:tag :txt :content "abc"})])])])
               "*abc*"   :em
               "`abc`"   :cs
               "**abc**" :strong))

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
                   {:tag :txt :content "[abc][qpr!]"})))))

      (testing "collapsed"
        (testing "inline content"
          (are [s t] (= (-> (str "[" s "][]\n\n[" s "]: xyz '123'")
                            from-string
                            (get-in [:children 0 :children]))
                        [(node {:tag :a :destination "xyz" :title "123"}
                               [(node {:tag t}
                                      [(node {:tag :txt :content "abc"})])])])
               "*abc*"   :em
               "`abc`"   :cs
               "**abc**" :strong))

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
                   {:tag :txt :content "[qpr!][]"})))))

      (testing "shortcut"
        (testing "inline content"
          (are [s t] (= (-> (str "[" s "]\n\n[" s "]: xyz '123'")
                            from-string
                            (get-in [:children 0 :children]))
                        [(node {:tag :a :destination "xyz" :title "123"}
                               [(node {:tag t}
                                      [(node {:tag :txt :content "abc"})])])])
               "*abc*"   :em
               "`abc`"   :cs
               "**abc**" :strong))

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

        (testing "followed by label"
          (is (= (-> "[abc][xyz][123]\n\n[123]: dest-123 'title-123'\n[abc]: dest-abc 'title-abc'"
                     from-string
                     (get-in [:children 0 :children]))
                 {:tag :txt :content "[qpr!]"}))))

      (testing "no match"
        (are [s] (= (from-string s)
                    (node {:tag :doc}
                          [(node {:tag :p}
                                 [(node {:tag :txt
                                         :content s})])]))
             "[abc][xyz]"
             "[abc][]"
             "[abc]"))

      (testing "surrounding inline content"
        (are [r l] (= (from-string (str "before " r " after\n\n[" l "]: xyz '123'"))
                    (node {:tag :doc}
                          [(node {:tag :p}
                                 [(node {:tag :txt :content "before "})
                                  (node {:tag :a :destination "xyz" :title "123"}
                                        [(node {:tag :txt :content "abc"})])
                                  (node {:tag :txt :content " after"})])]))
             "[abc][lbl]" "lbl"
             "[abc][]"    "abc"
             "[abc]"      "abc"))))

  (testing "image"
    (testing "inline"
      (testing "description, destination and title"
        (is (= (from-string "![abc](/xyz \"123\")")
               (node {:tag :doc}
                     [(node {:tag :p}
                            [(node {:tag :img
                                    :destination "/xyz"
                                    :title "123"}
                                  [(node {:tag :txt
                                          :content "abc"})])])]))))

      (testing "image within image"
        (is (= (from-string "![txt ![abc](/xyz)](/123)")
               (node {:tag :doc}
                     [(node {:tag :p}
                            [(node {:tag :img
                                    :destination "/123"}
                                  [(node {:tag :txt
                                          :content "txt "})
                                   (node {:tag :img
                                          :destination "/xyz"}
                                         [(node {:tag :txt :content "abc"})])])])]))))

      (testing "link within image"
        (is (= (from-string "![txt [abc](/xyz)](/123)")
               (node {:tag :doc}
                     [(node {:tag :p}
                            [(node {:tag :img
                                    :destination "/123"}
                                  [(node {:tag :txt
                                          :content "txt "})
                                   (node {:tag :a
                                          :destination "/xyz"}
                                         [(node {:tag :txt :content "abc"})])])])]))))))

  (testing "nested inline"
    (testing "text within emphasis"
      (is (= (from-string "*abc*")
             (node {:tag :doc}
                   [(node {:tag :p}
                          [(node {:tag :em}
                                 [(node {:tag :txt :content "abc"})])])]))))

    (testing "nested __-strong emphasis"
      (is (= (from-string "__abc, __xyz__, pqr__")
             (node {:tag :doc}
                   [(node {:tag :p}
                          [(node {:tag :strong}
                                 [(node {:tag :txt :content "abc, "})
                                  (node {:tag :strong}
                                        [(node {:tag :txt :content "xyz"})])
                                  (node {:tag :txt :content ", pqr"})])])]))))

    (testing "nested *-emphasis"
      (is (= (from-string "*(*abc*)* xyz *pqr*")
             (node {:tag :doc}
                   [(node {:tag :p}
                          [(node {:tag :em}
                                 [(node {:tag :txt :content "("})
                                  (node {:tag :em}
                                        [(node {:tag :txt :content "abc"})])
                                  (node {:tag :txt :content ")"})])
                           (node {:tag :txt :content " xyz "})
                           (node {:tag :em}
                                 [(node {:tag :txt :content "pqr"})])])])))))

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
                    [(node {:tag :cs}
                           [(node {:tag :txt :content c})])])
           "`abc  \nxyz`" "abc   xyz"
           "`abc\\\nxyz`" "abc\\ xyz"))

    (testing "inside raw HTML"
      (are [s] (= (-> s from-string (get-in [:children 0 :children]))
                    [(node {:tag :html-inline}
                           [(node {:tag :txt :content s})])])
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
             [(node {:tag :cs}
                    [(node {:tag :txt :content "abc xyz"})])])))

    (testing "inside raw HTML"
      (are [e] (= (-> (str "<a href=\"xyz" e "abc\">")
                      from-string
                      (get-in [:children 0 :children 0]))
                  (node {:tag :html-inline}
                        [(node {:tag :txt :content "<a href=\"xyz\r\nabc\">"})]))
           "\n"
;          "\r"
           "\r\n"))

    (testing "inside inline link"
      (are [e] (let [res (from-string (str "[abc" e "xyz](123)"))]
                 (and (= (get-in res [:children 0 :children 0 :data])
                         {:tag :a :destination "123"})
                      (= (map (comp :tag :data)
                              (get-in res [:children 0 :children 0 :children]))
                         [:txt :sbr :txt])
                      (= (get-in res [:children 0 :children 0 :children 0 :data :content])
                         "abc")
                      (= (get-in res [:children 0 :children 0 :children 2 :data :content])
                         "xyz")))
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
                    [(node {:tag :html-inline}
                           [(node {:tag :txt :content "<del>"})])
                     (node {:tag :em}
                           [(node {:tag :txt :content "abc"})])
                     (node {:tag :html-inline}
                           [(node {:tag :txt :content "</del>"})])])]))))

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
             [:li :bq])))))

