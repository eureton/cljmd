(ns commonmark.ast.inline-test
  (:require [clojure.test :refer :all]
            [commonmark.ast.inline :refer :all]
            [commonmark.ast.common :refer [node]]))

(deftest from-string-test
  (defn txt [c]
    (node {:tag :txt :content c}))

  (defn code [c]
    (node {:tag :cs :content c}))

  (defn em [c]
    (node {:tag :em} [(txt c)]))

  (defn html [c]
    (node {:tag :html-inline :content c}))

  (testing "pun nil"
    (is (nil? (from-string nil))))

  (testing "minimal"
    (testing "text"
      (is (= (-> "xyz" from-string (get-in [:children 0]))
             (txt "xyz"))))

    (testing "code span"
      (is (= (-> "`xyz`" from-string (get-in [:children 0]))
             (code "xyz"))))

    (testing "emphasis"
      (are [s] (= (-> s from-string (get-in [:children 0]))
                  (em "xyz"))
           "*xyz*"
           "_xyz_"))

    (testing "link"
      (is (= (-> "[xyz](abc)" from-string (get-in [:children 0]))
                  (node {:tag :a :destination "abc"}
                        [(txt "xyz")]))))

    (testing "image"
      (is (= (-> "![xyz](abc)" from-string (get-in [:children 0]))
                  (node {:tag :img :destination "abc"}
                        [(txt "xyz")])))))

  (testing "removes backslash escapes from ASCII punctuation"
    (are [in out] (= (-> (str "abc" in "xyz")
                         from-string
                         (get-in [:children 0]))
                     (txt (str "abc" out "xyz")))
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
                         (em "xyz")
                         (node {:tag :txt :content ")"})])]
      (are [s] (= (-> s from-string (get-in [:children 0]))
                  em-tree)
           "_(_xyz_)_"
           "*(*xyz*)*")))

  (testing "inlines within link text"
    (is (= (-> "[`xyz` 123 *abc*](qpr)" from-string (get-in [:children 0]))
                (node {:tag :a
                       :destination "qpr"}
                      [(code "xyz")
                       (txt " 123 ")
                       (em "abc")]))))

  (testing "link within image"
    (is (= (-> "![[txt](txt.com)](img.com)" from-string (get-in [:children 0]))
                (node {:tag :img :destination "img.com"}
                     [(node {:tag :a :destination "txt.com"}
                            [(txt "txt")])]))))

  (testing "inlines within image description"
    (is (= (-> "![`xyz` [*emphasis* on `code`](txt.com) *abc*](img.com)"
               from-string
               (get-in [:children 0]))
                (node {:tag :img :destination "img.com"}
                     [(code "xyz")
                      (txt " ")
                      (node {:tag :a :destination "txt.com"}
                            [(em "emphasis")
                             (txt " on ")
                             (code "code")])
                      (txt " ")
                      (em "abc")]))))

  (testing "precedence"
    (testing "inline link"
      (testing "vs codespan"
        (are [s t] (= (-> s from-string :children)
                      t)
             "`[ab`c](xyz)" [(code "[ab")
                             (txt "c](xyz)")]
             "[ab`c](x`yz)" [(txt "[ab")
                             (code "c](x")
                             (txt "yz)")]
             "[abc](x`yz)`" [(txt "[abc](x")
                             (code "yz)")]))

      (testing "vs emphasis"
        (are [s t] (= (-> s from-string :children)
                      t)
             "*[ab*c](xyz)" [(txt "*")
                             (node {:tag :a :destination "xyz"}
                                   [(txt "ab*c")])]
             "[ab*c](x*yz)" [(node {:tag :a :destination "x*yz"}
                                   [(txt "ab*c")])]
             "[abc](x*yz)*" [(node {:tag :a :destination "x*yz"}
                                   [(txt "abc")])
                             (txt "*")])))

    (testing "link reference"
      (def linkdef {:destination "123"})

      (testing "vs codespan"
        (are [s t] (= (-> s
                          (from-string {:definitions {"xyz" linkdef
                                                      "xy`z" linkdef}})
                          :children)
                      t)
             "`[ab`c][xyz]" [(code "[ab")
                             (txt "c][xyz]")]
             "[ab`c][xy`z]" [(txt "[ab")
                             (code "c][xy")
                             (txt "z]")]
             "[abc][xy`z]`" [(txt "[abc][xy")
                             (code "z]")]
             "`[xy`z][]"    [(code "[xy")
                             (txt "z][]")]
             "[xy`z][`]"    [(txt "[xy")
                             (code "z][")
                             (txt "]")]
             "[xyz][`]`"    [(txt "[xyz][")
                             (code "]")]
             "`[xy`z]"      [(code "[xy")
                             (txt "z]")]
             "[xy`z]`"      [(txt "[xy")
                             (code "z]")]))

      (testing "vs HTML"
        (are [s t] (= (-> s
                          (from-string {:definitions {"xyz" linkdef
                                                      "\">xyz" linkdef
                                                      "xy\">z" linkdef
                                                      "xyz<tag attr=\"" linkdef
                                                      "xy<tag attr=\"z" linkdef}})
                          :children)
                      t)
             "[abc<tag attr=\"][xyz]\">" [(txt "[abc")
                                          (html "<tag attr=\"][xyz]\">")]
             "<tag attr=\"[abc][\">xyz]" [(html "<tag attr=\"[abc][\">")
                                          (txt "xyz]")]
             "[abc<tag attr=\"][xy\">z]" [(txt "[abc")
                                          (html "<tag attr=\"][xy\">")
                                          (txt "z]")]
             "[xyz<tag attr=\"][]\">"    [(txt "[xyz")
                                          (html "<tag attr=\"][]\">")]
             "<tag attr=\"[xyz][\">]"    [(html "<tag attr=\"[xyz][\">")
                                          (txt "]")]
             "[xyz<tag attr=\"][\">]"    [(txt "[xyz")
                                          (html "<tag attr=\"][\">")
                                          (txt "]")]
             "[xy<tag attr=\"z]\">"      [(txt "[xy")
                                          (html "<tag attr=\"z]\">")]
             "<tag attr=\"[xy\">z]"      [(html "<tag attr=\"[xy\">")
                                          (txt "z]")]))

      (comment "TODO fix as soon as autolinks generate :a nodes"
      (testing "vs autolinks"
        (are [s t] (= (-> s from-string :children)
                      t)
             "[abc<http://123.com?q=\"][xyz]\">" [(txt "[abc")
                                                  (node {:tag :auto
                                                         :uri "http://123.com?q=\"][xyz]\""
                                                         :label "http://123.com?q=\"][xyz]\""})]
             "<http://123.com?q=\"[abc\">][xyz]" [(node {:tag :auto
                                                         :uri "http://123.com?q=\"[abc\""
                                                         :label "http://123.com?q=\"[abc\""})
                                                  (txt :content "][xyz]")]
             "[abc<http://123.com?q=\"][]\">"    [(txt "[abc")
                                                  (node {:tag :auto
                                                         :uri "http://123.com?q=\"][]\""
                                                         :label "http://123.com?q=\"][]\""})])))

      (testing "vs emphasis markers"
        (are [s t] (= (-> s
                          (from-string {:definitions {"xyz" linkdef
                                                      "xy*z" linkdef}})
                          :children)
                      t)
             "*[ab*c][xyz]" [(txt "*")
                             (node {:tag :a :destination "123"}
                                   [(txt "ab*c")])]
             "[ab*c][xyz]*" [(node {:tag :a :destination "123"}
                                   [(txt "ab*c")])
                             (txt "*")]
             "*[xy*z][]"    [(txt "*")
                             (node {:tag :a :destination "123"}
                                   [(txt "xy*z")])]
             "[xy*z][]*"    [(node {:tag :a :destination "123"}
                                   [(txt "xy*z")])
                             (txt "*")]
             "*[xy*z]"      [(txt "*")
                             (node {:tag :a :destination "123"}
                                   [(txt "xy*z")])]
             "[xy*z]*"      [(node {:tag :a :destination "123"}
                                   [(txt "xy*z")])
                             (txt "*")])))))

