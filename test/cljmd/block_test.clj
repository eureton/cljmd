(ns cljmd.block-test
  (:require [clojure.test :refer :all]
            [cljmd.block :refer :all]
            [cljmd.re.html :as re.html]))

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
         "###### foo"
         "#\tfoo"
         "##\tfoo"
         "###\tfoo"
         "####\tfoo"
         "#####\tfoo"
         "######\tfoo"
         ))

  (testing "more than six # characters => nil"
    (is (nil? (atx-heading "####### foo"))))

  (testing "no space between # and contents => nil"
    (are [s] (nil? (atx-heading s))
         "#5 bolt"
         "#hashtag"))

  (testing "first # is escaped => nil"
    (is (nil? (atx-heading "\\## foo"))))
  
  (testing "leading and trailing whitespace ignored in parsing inline content"
    (are [s] (= (-> s atx-heading :content)
                "foo")
         "#                  foo  "
         "#\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tfoo\t\t"))

  (testing "0-3 spaces indentation allowed"
    (are [s] (some? (atx-heading s))
         " ### foo"
         "  ## foo"
         "   # foo"))

  (testing "4 spaces => nil"
    (is (nil? (atx-heading "    # foo"))))

  (testing "closing sequence of #"
    (testing "preceded by spaces"
      (are [s] (= (-> s atx-heading :content)
                  "foo")
           "## foo ##"
           "## foo  ##"
           "## foo   ##"))

    (testing "preceded by tabs"
      (are [s] (= (-> s atx-heading :content)
                  "foo")
           "## foo\t##"
           "## foo\t\t##"
           "## foo\t\t\t##"))

    (testing "not the same length as opening sequence"
      (are [s] (some? (atx-heading s))
           "# foo ##################################"
           "##### foo ##"))

    (testing "followed by spaces"
      (is (= (-> "### foo ###  " atx-heading :content)
             "foo")))

    (testing "followed by tabs"
      (is (= (-> "### foo ###\t\t" atx-heading :content)
             "foo")))

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

(deftest setext-heading-test
  (testing "minimal"
    (is (some? (setext-heading "==="))))

  (testing "length"
    (are [s] (some? (setext-heading s))
         "="
         "-"
         "=="
         "--"
         "==="
         "---"
         "===="
         "----"
         "====="
         "-----"
         "======"
         "------"))

  (testing "character"
    (testing "="
      (is (= (-> "===" setext-heading :level)
             1)))

    (testing "-"
      (is (= (-> "---" setext-heading :level)
             2)))

    (testing "both - and ="
      (is (nil? (setext-heading "=-=")))))

  (testing "internal space"
    (are [s] (nil? (setext-heading "=-="))
         "= ="
         "- -"
         "=\t="
         "-\t-"))

  (testing "indentation"
    (testing "valid"
      (are [s] (some? (setext-heading s))
           " ==="
           " ---"
           "  ==="
           "  ---"
           "   ==="
           "   ---"))

    (testing "invalid"
      (are [s] (nil? (setext-heading s))
           "\t==="
           "\t---"
           "    ==="
           "    ---"))))

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

  (testing "tabs between characters => true"
    (are [s] (some? (thematic-break s))
         " -\t-\t-"
         " **\t\t*\t**\t*\t**\t*\t**"
         "-\t\t\t\t\t-\t\t\t\t\t\t-\t\t\t\t\t\t-"))

  (testing "spaces at the end => true"
    (is (some? (thematic-break "- - - -  "))))

  (testing "tabs at the end => true"
    (is (some? (thematic-break "- - - -\t\t"))))

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

(deftest belongs-to-list-item?-test
  (testing "adequate leading whitespace"
    (are [l p] (belongs-to-list-item? l p)
         "    xyz" [" 1. abc"]
         "    xyz" [" 1. abc" "    opqr"]
         "    xyz" [" 1. abc" "    # opqr"]
         "    xyz" [" 1. abc" ""]))

  (testing "previous is nil"
    (is (nil? (belongs-to-list-item? "abc" nil))))

  (testing "previous is empty"
    (is (nil? (belongs-to-list-item? "abc" []))))

  (testing "origin not a line item"
    (are [l p] (nil? (belongs-to-list-item? l p))
         "  xyz" ["abc" "- abc"]
         "  xyz" ["abc" "  abc"]
         "  xyz" ["abc" "  # abc"]
         "  xyz" ["abc" ""]))

  (testing "lazy continuation line after blank origin"
    (testing "sufficiently indented"
      (are [l p] (belongs-to-list-item? l p)
           "  xyz"  ["-"]
           "   xyz" ["-"]))

    (testing "insufficiently indented"
      (are [l p] (not (belongs-to-list-item? l p))
           " xyz" ["-"]
           "xyz"  ["-"]))))

(deftest paragraph-continuation-text?-test
  (testing "degenerate"
    (is (false? (paragraph-continuation-text? "abc" []))))

  (testing ":p -> :p"
    (is (paragraph-continuation-text? "xyz" ["> abc"])))

  (testing ":icblk -> :p"
    (is (paragraph-continuation-text? "    xyz" ["> abc"])))

  (testing ":p -> :p -> :p"
    (is (paragraph-continuation-text? "123" ["> abc" "> xyz"])))

  (testing ":p -> :icblk -> :p"
    (is (paragraph-continuation-text? "123" ["> abc" ">     xyz"])))

  (testing ":icblk -> :p -> :p"
    (is (paragraph-continuation-text? "    123" ["> abc" "> xyz"])))

  (testing ":icblk -> :icblk -> :p"
    (is (paragraph-continuation-text? "    123" ["> abc" ">     xyz"])))

  (testing "setext in blockquote"
    (testing "underline inside"
      (is (paragraph-continuation-text? "xyz" ["> abc" "==="]))))

    (testing "underline outside"
      (is (false? (paragraph-continuation-text? "xyz" ["> abc" "> ==="])))))

(deftest html-block-begin-test
  (testing "pun nil"
    (is (nil? (html-block-begin nil))))

  (testing "variant 1"
    (testing "tags"
      (testing "valid"
        (are [t] (contains? (-> (str t "xyz") html-block-begin :variant)
                            1)
             "<pre>"
             "<script>"
             "<style>"
             "<textarea>"))

      (testing "invalid"
        (are [t] (not (contains? (-> (str t "xyz") html-block-begin :variant)
                                 1))
             "<abc>"
             "<xyz>"
             "<>"
             "\\<pre>"
             "\\<script>"
             "\\<style>"
             "\\<textarea>")))

    (testing "capture"
      (are [s] (let [res (html-block-begin s)]
                 (and (contains? (:variant res) 1)
                      (= s (:content res))))
           "<pre"
           "<script"
           "<style"
           "<textarea"
           "<pre>xyz"
           "<script>xyz"
           "<style>xyz"
           "<textarea>xyz"
           "<pre xyz"
           "<script xyz"
           "<style xyz"
           "<textarea xyz"))

    (testing "indentation"
      (testing "valid"
        (are [s] (some? (html-block-begin s))
             " <pre"
             "  <pre"
             "   <pre"))

      (testing "invalid"
        (are [s] (nil? (html-block-begin s))
             "    <pre"
             "     <pre"
             "      <pre"))))

  (testing "variant 2"
    (testing "tags"
      (testing "valid"
        (is (contains? (-> "<!--xyz" html-block-begin :variant)
                       2)))

      (testing "invalid"
        (are [t] (not (contains? (-> (str t "xyz") html-block-begin :variant)
                                 2))
             "< !--"
             "<! --"
             "<!- -"
             "<!-"
             "\\<!--")))

    (testing "capture"
      (are [s] (let [res (html-block-begin s)]
                 (and (contains? (:variant res) 2)
                      (= s (:content res))))
           "<!--"
           "<!--xyz"
           "<!-- xyz"))

    (testing "indentation"
      (testing "valid"
        (are [s] (some? (html-block-begin s))
             " <!--"
             "  <!--"
             "   <!--"))

      (testing "invalid"
        (are [s] (nil? (html-block-begin s))
             "    <!--"
             "     <!--"
             "      <!--"))))

  (testing "variant 3"
    (testing "tags"
      (testing "valid"
        (is (contains? (-> "<?xyz" html-block-begin :variant)
                       3)))

      (testing "invalid"
        (are [t] (not (contains? (-> (str t "xyz") html-block-begin :variant)
                                 3))
             "< ?"
             "\\<?"
             "<\\?")))

    (testing "capture"
      (are [s] (let [res (html-block-begin s)]
                 (and (contains? (:variant res) 3)
                      (= s (:content res))))
           "<?"
           "<?xyz"
           "<? xyz"))

    (testing "indentation"
      (testing "valid"
        (are [s] (some? (html-block-begin s))
             " <?"
             "  <?"
             "   <?"))

      (testing "invalid"
        (are [s] (nil? (html-block-begin s))
             "    <?"
             "     <?"
             "      <?"))))

  (testing "variant 4"
    (testing "tags"
      (testing "valid"
        (is (contains? (-> "<!Wxyz" html-block-begin :variant)
                       4)))

      (testing "invalid"
        (are [t] (not (contains? (-> (str t "xyz") html-block-begin :variant)
                                 4))
             "< !W"
             "<! W"
             "<! "
             "\\<!W"
             "<\\!W"
             "<!\\W")))

    (testing "capture"
      (are [s] (let [res (html-block-begin s)]
                 (and (contains? (:variant res) 4)
                      (= s (:content res))))
           "<!W"
           "<!Wxyz"
           "<!W xyz"))

    (testing "indentation"
      (testing "valid"
        (are [s] (some? (html-block-begin s))
             " <!W"
             "  <!W"
             "   <!W"))

      (testing "invalid"
        (are [s] (nil? (html-block-begin s))
             "    <!W"
             "     <!W"
             "      <!W"))))

  (testing "variant 5"
    (testing "tags"
      (testing "valid"
        (is (contains? (-> "<![CDATA[xyz" html-block-begin :variant)
                       5)))

      (testing "invalid"
        (are [t] (not (contains? (-> (str t "xyz") html-block-begin :variant)
                                 5))
             "< ![CDATA["
             "<! [CDATA["
             "<![ CDATA["
             "<![C DATA["
             "<![CD ATA["
             "<![CDA TA["
             "<![CDAT A["
             "<![CDATA ["
             "\\<![CDATA["
             "<\\![CDATA["
             "<!\\[CDATA["
             "<![\\CDATA["
             "<![C\\DATA["
             "<![CD\\ATA["
             "<![CDA\\TA["
             "<![CDAT\\A["
             "<![CDATA\\[")))

    (testing "capture"
      (are [s] (let [res (html-block-begin s)]
                 (and (contains? (:variant res) 5)
                      (= s (:content res))))
           "<![CDATA["
           "<![CDATA[xyz"
           "<![CDATA[ xyz"))

    (testing "indentation"
      (testing "valid"
        (are [s] (some? (html-block-begin s))
             " <![CDATA["
             "  <![CDATA["
             "   <![CDATA["))

      (testing "invalid"
        (are [s] (nil? (html-block-begin s))
             "    <![CDATA["
             "     <![CDATA["
             "      <![CDATA["))))

  (testing "variant 6"
    (testing "tags"
      (testing "valid"
        (let [v6? #(contains? (:variant %) 6)]
          (are [pre suf] (every? #(->> (str pre % suf)
                                       html-block-begin
                                       v6?)
                                 re.html/block-variant-6-tags)
               "<"  " xyz"
               "<"  ">"
               "<"  "/>"
               "<"  ""
               "</" " xyz"
               "</" ">"
               "</" "/>"
               "</" "")))

      (testing "invalid"
        (are [s] (not (contains? (-> s html-block-begin :variant)
                                 6))
             "< p"
             "<px"
             "< p>"
             "< p/ >"
             "< /p"
             "</ p"
             "</px"
             "< /p x"
             "</ p x"
             "< /p>"
             "</ p>"
             "< /p/>"
             "</ p/>"
             "</p/ >")))

    (testing "capture"
      (are [s] (let [res (html-block-begin s)]
                 (and (contains? (:variant res) 6)
                      (= s (:content res))))
           "<p>"
           "<p>xyz"
           "<p> xyz"
           "<p xyz"
           "<p>"
           "<p/>"
           "<p"
           "</p xyz"
           "</p>"
           "</p/>"
           "</p"))

    (testing "indentation"
      (testing "valid"
        (are [s] (some? (html-block-begin s))
             " <p"
             "  <p"
             "   <p"))

      (testing "invalid"
        (are [s] (nil? (html-block-begin s))
             "    <p"
             "     <p"
             "      <p"))))

  (testing "variant 7"
    (testing "tags"
      (testing "valid"
        (are [s] (contains? (-> s html-block-begin :variant)
                            7)
             "<a>"
             "<a >"
             "<a href>"
             "<a href=\"xyz\">"
             "<a href=\"xyz\" >"
             "<a/>"
             "<a />"
             "<a href/>"
             "<a href=\"xyz\"/>"
             "<a href=\"xyz\" />"
             "</a>"
             "</a >"
             "</textarea>"
             "</script>"
             "</style>"
             "</pre>"
             "</textarea >"
             "</script >"
             "</style >"
             "</pre >"))

      (testing "invalid"
        (are [s] (not (contains? (-> s html-block-begin :variant)
                                 7))
             "<textarea>"
             "<script>"
             "<style>"
             "<pre>"
             "<textarea >"
             "<script >"
             "<style >"
             "<pre >"
             "<textarea abc>"
             "<script abc>"
             "<style abc>"
             "<pre abc>"
             "<textarea abc=\"xyz\">"
             "<script abc=\"xyz\">"
             "<style abc=\"xyz\">"
             "<pre abc=\"xyz\">"
             "<textarea/>"
             "<script/>"
             "<style/>"
             "<pre/>"
             "<textarea />"
             "<script />"
             "<style />"
             "<pre />"
             "<textarea abc/>"
             "<script abc/>"
             "<style abc/>"
             "<pre abc/>"
             "<textarea abc=\"xyz\"/>"
             "<script abc=\"xyz\"/>"
             "<style abc=\"xyz\"/>"
             "<pre abc=\"xyz\"/>"
             "<a>xyz"
             "<a> xyz")))

    (testing "capture"
      (are [s] (let [res (html-block-begin s)]
                 (and (contains? (:variant res) 7)
                      (= s (:content res))))
           "<p>"
           "<p class=\"xyz\">"
           "<p> \t "))

    (testing "indentation"
      (testing "valid"
        (are [s] (some? (html-block-begin s))
             " <p>"
             "  <p>"
             "   <p>"))

      (testing "invalid"
        (are [s] (nil? (html-block-begin s))
             "    <p>"
             "     <p>"
             "      <p>")))))

(deftest html-block-end-test
  (testing "pun nil"
    (is (nil? (html-block-end nil))))

  (testing "variant 1"
    (testing "tags"
      (testing "valid"
        (are [t] (contains? (-> t html-block-end :variant)
                            1)
             "</pre>"
             "</textarea>"
             "</script>"
             "</style>"))

      (testing "invalid"
        (are [t] (not (contains? (-> (str t "xyz") html-block-end :variant)
                                 1))
             "</abc>"
             "</xyz>"
             "</>"
             "\\</pre>"
             "\\</textarea>"
             "\\</script>"
             "\\</style>")))

    (testing "capture"
      (are [s] (let [res (html-block-end s)]
                 (and (contains? (:variant res) 1)
                      (= s (:content res))))
           "</pre>"
           "</textarea>"
           "</script>"
           "</style>"
           "</pre> xyz"
           "</textarea> xyz"
           "</script> xyz"
           "</style> xyz"
           "abc </pre> xyz"
           "abc </textarea> xyz"
           "abc </script> xyz"
           "abc </style> xyz"))

    (testing "indentation"
      (testing "valid"
        (are [s] (some? (html-block-end s))
             " </pre>"
             "  </pre>"
             "   </pre>"))

      (testing "invalid"
        (are [s] (nil? (html-block-end s))
             "    </pre>"
             "     </pre>"
             "      </pre>"))))

  (testing "variant 2"
    (testing "tags"
      (testing "valid"
        (is (contains? (-> "-->" html-block-end :variant)
                       2)))

      (testing "invalid"
        (are [t] (not (contains? (-> (str t "xyz") html-block-end :variant)
                                 2))
             "- ->"
             "-- >"
             "\\-->")))

    (testing "capture"
      (are [s] (let [res (html-block-end s)]
                 (and (contains? (:variant res) 2)
                      (= s (:content res))))
           "-->"
           "--> xyz"
           "abc --> xyz"))

    (testing "indentation"
      (testing "valid"
        (are [s] (some? (html-block-end s))
             " -->"
             "  -->"
             "   -->"))

      (testing "invalid"
        (are [s] (nil? (html-block-end s))
             "    -->"
             "     -->"
             "      -->"))))

  (testing "variant 3"
    (testing "tags"
      (testing "valid"
        (is (contains? (-> "?>" html-block-end :variant)
                       3)))

      (testing "invalid"
        (are [t] (not (contains? (-> (str t "xyz") html-block-end :variant)
                                 3))
             "? >"
             "?\\>"
             "\\?>")))

    (testing "capture"
      (are [s] (let [res (html-block-end s)]
                 (and (contains? (:variant res) 3)
                      (= s (:content res))))
           "?>"
           "?> xyz"
           "abc ?> xyz"))

    (testing "indentation"
      (testing "valid"
        (are [s] (some? (html-block-end s))
             " ?>"
             "  ?>"
             "   ?>"))

      (testing "invalid"
        (are [s] (nil? (html-block-end s))
             "    ?>"
             "     ?>"
             "      ?>"))))

  (testing "variant 4"
    (testing "tags"
      (testing "valid"
        (is (contains? (-> ">" html-block-end :variant)
                       4)))

      (testing "invalid"
        (is (not (contains? (-> "\\>xyz" html-block-end :variant)
                            4)))))

    (testing "capture"
      (are [s] (let [res (html-block-end s)]
                 (and (contains? (:variant res) 4)
                      (= s (:content res))))
           ">"
           "> xyz"
           "abc > xyz"))

    (testing "indentation"
      (testing "valid"
        (are [s] (some? (html-block-end s))
             " >"
             "  >"
             "   >"))

      (testing "invalid"
        (are [s] (nil? (html-block-end s))
             "    >"
             "     >"
             "      >"))))

  (testing "variant 5"
    (testing "tags"
      (testing "valid"
        (is (contains? (-> "]]>" html-block-end :variant)
                       5)))

      (testing "invalid"
        (are [s] (not (contains? (-> s html-block-end :variant)
                                 5))
             "] ]>xyz"
             "]] >xyz"
             "\\]]>xyz"
             "]\\]>xyz"
             "]]\\>xyz")))

    (testing "capture"
      (are [s] (let [res (html-block-end s)]
                 (and (contains? (:variant res) 5)
                      (= s (:content res))))
           "]]>"
           "]]> xyz"
           "abc ]]> xyz"))

    (testing "indentation"
      (testing "valid"
        (are [s] (some? (html-block-end s))
             " ]]>"
             "  ]]>"
             "   ]]>"))

      (testing "invalid"
        (are [s] (nil? (html-block-end s))
             "    ]]>"
             "     ]]>"
             "      ]]>"))))

  (testing "variant 6"
    (testing "valid"
      (are [s] (contains? (-> s html-block-end :variant)
                          6)
           ""
           " "
           "  "
           "   "
           "\t"
           "\t\t"
           "\t\t\t"
           "\n"
           "\n\n"
           "\n\n\n"
           "\r"
           "\r\r"
           "\r\r\r"
           "\r\n"
           "\r\n\r\n"
           "\r\n\r\n\r\n"
           " \t\n\r \t\n\r \t\n\r"))

    (testing "invalid"
      (are [s] (not (contains? (-> s html-block-end :variant)
                               6))
           "x"
           ">"
           "\\"))

    (testing "capture"
      (are [s] (let [res (html-block-end s)]
                 (and (contains? (:variant res) 6)
                      (= s (:content res))))
           ""
           "   "
           "   \t\t\t   ")))

  (testing "variant 7"
    (testing "valid"
      (are [s] (contains? (-> s html-block-end :variant)
                          7)
           ""
           " "
           "  "
           "   "
           "\t"
           "\t\t"
           "\t\t\t"
           "\n"
           "\n\n"
           "\n\n\n"
           "\r"
           "\r\r"
           "\r\r\r"
           "\r\n"
           "\r\n\r\n"
           "\r\n\r\n\r\n"
           " \t\n\r \t\n\r \t\n\r"))

    (testing "invalid"
      (are [s] (not (contains? (-> s html-block-end :variant)
                               7))
           "x"
           ">"
           "\\"))

    (testing "capture"
      (are [s] (let [res (html-block-end s)]
                 (and (contains? (:variant res) 7)
                      (= s (:content res))))
           ""
           "   "
           "   \t\t\t   "))))

(deftest link-reference-definition-test
  (testing "complete"
    (let [{:keys [label destination title]} (link-reference-definition "[abc]: xyz '123'")]
      (is (and (= "abc" label)
               (= "xyz" destination)
               (= "123" title)))))

  (testing "no title"
    (let [{:keys [label destination title]} (link-reference-definition "[abc]: xyz")]
      (is (and (= "abc" label)
               (= "xyz" destination)
               (nil? title)))))

  (testing "no destination, no title"
    (is (nil? (link-reference-definition "[abc]:"))))

  (testing "whitespace"
    (are [s] (some? (link-reference-definition s))
         "[abc]:xyz"
         "[abc]: xyz"
         "[abc]: xyz "
         "[abc]: xyz '123'"
         "[abc]: xyz '123' "))

  (testing "non-whitespace character after title"
    (is (nil? (link-reference-definition "[abc]: xyz '123' q"))))

  (testing "valid"
    (are [s] (some? (link-reference-definition s))
         "[abc\\\\]: xyz"))

  (testing "invalid"
    (are [s] (nil? (link-reference-definition s))
         "\\[abc]: xyz"
         "[abc] xyz"
         "[abc] : xyz"
         "[]: xyz"
         "[ ]: xyz"
         "[\t]: xyz"
         "[\n]: xyz"
         "xyz"
         "<xyz>"
         "\"123\""
         "'123'"
         "(123)"
         "((123))"))

  (testing "indentation"
    (testing "valid"
      (are [s] (some? (link-reference-definition s))
           " [abc]: xyz"
           "  [abc]: xyz"
           "   [abc]: xyz"))

    (testing "invalid"
      (are [s] (nil? (link-reference-definition s))
           "    [abc]: xyz"
           "     [abc]: xyz"
           "      [abc]: xyz"))))

