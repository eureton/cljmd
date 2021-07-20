(ns commonmark.ast.inline-test
  (:require [clojure.test :refer :all]
            [commonmark.ast.inline :refer :all]
            [commonmark.ast.common :refer [node]]))

(deftest from-string-test
  (testing "pun nil"
    (is (nil? (from-string nil))))

  (testing "minimal"
    (let [txt-node (node {:tag :txt :content "xyz"})]
      (testing "text"
        (is (= (-> "xyz" from-string (get-in [:children 0]))
               txt-node)))

      (testing "code span"
        (is (= (-> "`xyz`" from-string (get-in [:children 0]))
               (node {:tag :cs} [txt-node]))))

      (testing "emphasis"
        (are [s] (= (-> s from-string (get-in [:children 0]))
                    (node {:tag :em} [txt-node]))
             "*xyz*"
             "_xyz_"))

      (testing "link"
        (is (= (-> "[xyz](abc)" from-string (get-in [:children 0]))
                    (node {:tag :a
                           :destination "abc"} [txt-node]))))

      (testing "image"
        (is (= (-> "![xyz](abc)" from-string (get-in [:children 0]))
                    (node {:tag :img
                           :destination "abc"} [txt-node]))))))

  (testing "removes backslash escapes from ASCII punctuation"
    (are [in out] (= (-> (str "abc" in "xyz")
                         from-string
                         (get-in [:children 0]))
                     (node {:tag :txt :content (str "abc" out "xyz")}))
         "\\!" "!"
         "\\\"" "\""
         "\\#" "#"
         "\\$" "$"
         "\\%" "%"
         "\\&" "&"
         "\\'" "'"
         "\\(" "("
         "\\)" ")"
         "\\*" "*"
         "\\+" "+"
         "\\," ","
         "\\-" "-"
         "\\." "."
         "\\/" "/"
         "\\:" ":"
         "\\;" ";"
         "\\<" "<"
         "\\=" "="
         "\\>" ">"
         "\\?" "?"
         "\\@" "@"
         "\\[" "["
         "\\\\" "\\"
         "\\]" "]"
         "\\^" "^"
         "\\_" "_"
         "\\`" "`"
         "\\{" "{"
         "\\|" "|"
         "\\}" "}"
         "\\~" "~"))

  (testing "nested emphasis"
    (let [em-tree (node {:tag :em}
                        [(node {:tag :txt :content "("})
                         (node {:tag :em} [(node {:tag :txt :content "xyz"})])
                         (node {:tag :txt :content ")"})])]
      (are [s] (= (-> s from-string (get-in [:children 0]))
                  em-tree)
           "_(_xyz_)_"
           "*(*xyz*)*")))

  (testing "inlines within link text"
    (is (= (-> "[`xyz` 123 *abc*](qpr)" from-string (get-in [:children 0]))
                (node {:tag :a
                       :destination "qpr"}
                      [(node {:tag :cs} [(node {:tag :txt :content "xyz"})])
                       (node {:tag :txt :content " 123 "})
                       (node {:tag :em} [(node {:tag :txt :content "abc"})])]))))

  (testing "link within image"
    (is (= (-> "![[txt](txt.com)](img.com)" from-string (get-in [:children 0]))
                (node {:tag :img
                       :destination "img.com"}
                     [(node {:tag :a
                             :destination "txt.com"}
                            [(node {:tag :txt :content "txt"})])]))))

  (testing "inlines within image description"
    (is (= (-> "![`xyz` [*emphasis* on `code`](txt.com) *abc*](img.com)"
               from-string
               (get-in [:children 0]))
                (node {:tag :img
                       :destination "img.com"}
                     [(node {:tag :cs} [(node {:tag :txt :content "xyz"})])
                      (node {:tag :txt :content " "})
                      (node {:tag :a
                             :destination "txt.com"}
                            [(node {:tag :em} [(node {:tag :txt :content "emphasis"})])
                             (node {:tag :txt :content " on "})
                             (node {:tag :cs} [(node {:tag :txt :content "code"})])])
                      (node {:tag :txt :content " "})
                      (node {:tag :em} [(node {:tag :txt :content "abc"})])]))))

  (testing "precedence"
    (testing "inline link"
      (testing "text brackets"
        (testing "vs codespan backticks"
          (are [s t] (= (-> s from-string :children)
                        t)
               "`[abc`](xyz)" [(node {:tag :cs}
                                     [(node {:tag :txt :content "[abc"})])
                               (node {:tag :txt :content "](xyz)"})]
               "[ab`c](x`yz)" [(node {:tag :txt :content "[ab"})
                               (node {:tag :cs}
                                     [(node {:tag :txt :content "c](x"})])
                               (node {:tag :txt :content "yz)"})]))

        (testing "vs emphasis markers"
          (are [s t] (= (-> s from-string :children)
                        t)
               "*[abc*](xyz)" [(node {:tag :txt :content "*"})
                               (node {:tag :a
                                      :destination "xyz"}
                                     [(node {:tag :txt :content "abc*"})])]
               "[ab*c](xyz*)" [(node {:tag :a
                                      :destination "xyz*"}
                                     [(node {:tag :txt :content "ab*c"})])]
               "[a*bc](xyz)*" [(node {:tag :a
                                      :destination "xyz"}
                                     [(node {:tag :txt :content "a*bc"})])
                               (node {:tag :txt :content "*"})]))))
    (testing "link reference"
      (testing "text brackets"
        (testing "vs codespan backticks"
          (are [s t] (= (-> s from-string :children)
                        t)
               "`[abc`][xyz]" [(node {:tag :cs}
                                     [(node {:tag :txt :content "[abc"})])
                               (node {:tag :txt :content "][xyz]"})]
               "[ab`c][x`yz]" [(node {:tag :txt :content "[ab"})
                               (node {:tag :cs}
                                     [(node {:tag :txt :content "c][x"})])
                               (node {:tag :txt :content "yz]"})]))

        (testing "vs HTML"
          (are [s t] (= (-> s from-string :children)
                        t)
               "[<abc attr=\"][xyz]\">"  [(node {:tag :txt :content "["})
                                          (node {:tag :html-inline
                                                 :content "<abc attr=\"][xyz]\">"})]
               "<a attr=\"[abc][\">xyz]" [(node {:tag :html-inline
                                                 :content "<a attr=\"[abc][\">"})
                                          (node {:tag :txt :content "xyz]"})]))

        (testing "vs autolinks"
          (are [s t] (= (-> s from-string :children)
                        t)
               "[abc<http://123.com?q=\"][xyz]\">" [(node {:tag :txt :content "[abc"})
                                                    (node {:tag :auto
                                                           :uri "http://123.com?q=\"][xyz]\""
                                                           :label "http://123.com?q=\"][xyz]\""})]
               "<http://123.com?q=\"[abc\">][xyz]" [(node {:tag :auto
                                                           :uri "http://123.com?q=\"[abc\""
                                                           :label "http://123.com?q=\"[abc\""})
                                                    (node {:tag :txt :content "][xyz]"})]))

        (testing "vs emphasis markers"
          (are [s t] (= (-> (str s "\n\n[xyz]: 123") from-string :children)
                        t)
               "*[abc*][xyz]" [(node {:tag :txt :content "*"})
                               (node {:tag :a
                                      :destination "123"}
                                     [(node {:tag :txt :content "abc*"})])]
               "[a*bc][xyz]*" [(node {:tag :a
                                      :destination "123"}
                                     [(node {:tag :txt :content "a*bc"})])
                               (node {:tag :txt :content "*"})]))))))

