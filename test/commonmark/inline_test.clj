(ns commonmark.inline-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [commonmark.inline :refer :all]
            [commonmark.emphasis :as emphasis]
            [commonmark.re.inline :as re.inline]
            [commonmark.re.link :as re.link]
            [commonmark.re.html :as re.html]))

(deftest code-span-test
  (defn match [s]
    (re-find re.inline/code-span s))

  (defn content [s]
    (some->> (match s) (hash-map :re/match) code-span :content))

  (testing "minimal"
    (is (= "foo" (content "`foo`"))))

  (testing "multiple"
    (is (= "foo" (content "`foo` bar `baz`"))))

  (testing "backtick in contents"
    (is (= "foo`bar" (content "``foo`bar``"))))

  (testing "wrap backtick pairs"
    (is (= "`ls -l`" (content "`` `ls -l` ``"))))

  (testing "strip"
    (testing "single space"
      (are [s c] (= c (content s))
           "` foo `"   "foo"
           "`  foo  `" " foo "
           "` `` `"    "``"))

    (testing "space not on both ends"
      (is (= " foo" (content "` foo`"))))

    (testing "ASCII whitespace"
      (are [c] (= (count (content (str "`" c "foo" c "`")))
                  5)
           \u0009
           \u000C))

    (testing "Unicode whitespace"
      (are [c] (= (count (content (str "`" c "foo" c "`")))
                  5)
           \u00A0
           \u1680
           \u2000
           \u2001
           \u2002
           \u2003
           \u2004
           \u2005
           \u2006
           \u2007
           \u2008
           \u2009
           \u200A
           \u202F
           \u205F
           \u3000))

    (testing "contains only spaces"
      (are [s c] (= c (content s))
           "` `"  " "
           "`  `" "  ")))

  (testing "line endings"
    (are [s c] (= c (content s))
         "``\nfoo\nbar  \nbaz\n``" "foo bar   baz"
         "``\nfoo \n``" "foo "))

  (testing "interior spaces"
    (is (= (content "`foo   bar \nbaz`")
           "foo   bar  baz")))

  (testing "backtick strings unequal length"
    (testing "no match"
      (are [s] (nil? (match s))
           "```foo``"
           "`foo"
           "`foo``"))

    (testing "match"
      (are [s c] (= c (content s))
           "`foo``bar``" "bar")))

  (testing "line endings"
    (are [s c] (= c (content s))
         "`abc\r\nxyz`"     "abc xyz"
         "`abc\nxyz`"       "abc xyz"
         "`abc\rxyz`"       "abc xyz"
         "`abc\r\n\r\nxyz`" "abc  xyz"
         "`abc\n\nxyz`"     "abc  xyz"
         "`abc\r\rxyz`"     "abc  xyz")))

(deftest delimeter-run-test
  (let [star (re.inline/delimiter-run "[*]")
        lobar (re.inline/delimiter-run "[_]")
        l-star (re.inline/lfdr star)
        l-lobar (re.inline/lfdr lobar)
        r-star (re.inline/rfdr star)
        r-lobar (re.inline/rfdr lobar)
        match? (fn [s re] (->> s (re-find re) some?))]
    (testing "flanking"
      (are [s left? right?]
           (and (= left?  (or (match? s l-star)
                              (match? s l-lobar)))
                (= right? (or (match? s r-star)
                              (match? s r-lobar))))
           ; left yes, right no
           "***abc"             true false
           "*abc"               true false
           "**\"abc \""         true false
           "*\"abc \""          true false
           "_\"abc \""          true false
           "_abc"               true false
           ; left no, right yes
           "abc***"            false  true
           "abc_"              false  true
           "\"abc \"**"        false  true
           "\"abc \"_"         false  true
           "\\**"              false  true
           ; left yes, right yes
           "abc***def"          true  true
           "\"abc \"_\"def \""  true  true
           ; left no, right no
           "abc *** def"       false false
           "a _ b"             false false))))

(deftest tokenize-test
  (testing "inline link"
    (defn match [s]
      (->> s tokenize (filter (comp #{:a} :tag))))

    (defn text [s]
      (->> s match (keep :text) set))

    (defn destination [s]
      (->> s match (keep :destination) set))

    (defn title [s]
      (->> s match (keep :title) set))

    (testing "invalid input"
      (is (empty? (match "not-a-valid-inline-link"))))

    (testing "omit destination"
      (is (not-empty (match "[abc]()"))))

    (testing "omit title"
      (testing "text"
        (is (not-empty (text "[abc](xyz)"))))

      (testing "destination"
        (is (not-empty (destination "[abc](xyz)"))))

      (testing "title"
        (is (empty? (title "[abc](xyz)")))))

    (testing "text"
      (testing "backslash-escaped brackets"
        (are [s] (= #{s} (text (str "[" s "](xyz)")))
             "abc\\]123"
             "abc\\[123"))

      (testing "balanced brackets"
        (are [s] (= #{s} (text (str "[" s "](xyz)")))
             ""
             "[]"
             "[][]"
             "[[]]"
             "[[[]]]"
             "[[][]]"
             "[[]][]"
             "[][][]"
             "[abc]"
             "[[abc]]"
             "abc [123] pqr"
             "[abc [123] pqr]"))

      (testing "unbalanced brackets"
        (are [s] (empty? (match s))
             "[]]()"
             "[[]]]()"
             "[[[]]]]()"))

      (testing "contains line breaks"
        (testing "single"
          (is (= #{"ab\ncd"} (text "[ab\ncd](xyz)"))))

        (testing "multiple"
          (is (= #{"ab\ncd\nef"} (text "[ab\ncd\nef](xyz)")))))

      (testing "contains blank line"
        (is (empty? (match "[ab\n\ncd](xyz)"))))

      (testing "nested links"
        (testing "text"
          (is (= #{"in"} (text "[[in](in.com)](out.com)"))))

        (testing "destination"
          (is (= #{"in.com"} (destination "[[in](in.com)](out.com)"))))))

    (testing "destination"
      (testing "wrapped in <>"
        (testing "minimal"
          (is (= #{"xyz"} (destination "[abc](<xyz>)"))))

        (testing "spaces"
          (is (= #{"xyz 123 qpr"} (destination "[abc](<xyz 123 qpr>)"))))

        (testing "line breaks"
          (is (empty? (match "[abc](<123\nxyz>)"))))

        (testing "parentheses"
          (are [s d] (= #{d} (destination s))
               "[abc](<123)xyz>)" "123)xyz"
               "[abc](<123(xyz>)" "123(xyz"))

        (testing "with title"
          (are [t] (= #{"xyz"} (destination (str "[abc](<xyz> " t ")")))
               "'123'"
               "\"123\""))

        (testing "escaped delimeters"
          (are [s] (= #{s} (destination (str "[abc](<" s ">)")))
               "123\\<xyz"
               "123\\>xyz"
               "123\\<qpr\\>xyz"))

        (testing "unescaped delimeters"
          (are [s] (empty? (match (str "[abc](<" s ">)")))
               "123<xyz"
               "123>xyz"
               "123<qpr>xyz"))

        (testing "improperly matched opening delimiters"
          (are [s] (empty? (match s))
               "[a] (<b)c"
               "[a] (<b)c>"
               "[a] (<b>c)")))

      (testing "not wrapped in <>"
        (testing "minimal"
          (is (= #{"xyz"} (destination "[abc](xyz)"))))

        (testing "begins with <"
          (is (empty? (match "[abc](<xyz)"))))

        (testing "contains <"
          (is (= #{"x<yz"} (destination "[abc](x<yz)"))))

        (testing "spaces"
          (is (empty? (match "[abc](xyz 123)"))))

        (testing "with title"
          (are [t] (= #{"xyz"} (destination (str "[abc](xyz " t ")")))
               "'123'"
               "\"123\""))

        (testing "control characters"
          (are [s] (empty? (match (str "[abc](" s ")")))
               "123\rxyz"
               "123\nxyz"))

        (testing "parentheses"
          (testing "unescaped, unbalanced"
            (are [s] (empty? (match (str "[abc](" s ")")))
                 "123(xyz"
                 "123()xyz("
                 "123((qpr))xyz("))

          (testing "unescaped, balanced"
            (are [s] (= #{s} (destination (str "[abc](" s ")")))
                 "()"
                 "123()xyz"
                 "123(!(qpr)!)xyz"))

          (testing "escaped"
            (are [s] (= #{s} (destination (str "[abc](" s ")")))
                 "123\\(xyz"
                 "123\\)xyz"
                 "123\\)q\\(pr\\)xyz"))))

      (testing "fragments and queries"
        (are [d] (= #{d} (destination (str "[abc](" d ")")))
             "#fragment"
             "http://example.com#fragment"
             "http://example.com?foo=3#frag")))

    (testing "title"
      (testing "'-delimited"
        (testing "minimal"
          (is (= #{"123"} (title "[abc](xyz '123')"))))

        (testing "double quotes"
          (is (= #{"12 \"34\" 56"} (title "[abc](xyz '12 \"34\" 56')"))))

        (testing "escaped delimeters"
          (are [t] (= #{t} (title (str "[abc](xyz '" t "')")))
               "1\\'23"
               "12\\'3"
               "1\\'2\\'3"))

        (testing "unescaped delimeters"
          (are [t] (empty? (match (str "[abc](xyz '" t "')")))
               "1'23"
               "12'3"
               "1'2'3"))

        (testing "line breaks"
          (testing "single"
            (is (= #{"12\n34\n56"} (title "[abc](xyz '12\n34\n56')"))))

          (testing "multiple"
            (is (empty? (match "[abc](xyz '12\n\n34')"))))))

      (testing "\"-delimited"
        (testing "minimal"
          (is (= #{"123"} (title "[abc](xyz \"123\")"))))

        (testing "single quotes"
          (is (= #{"12 '34' 56"} (title "[abc](xyz \"12 '34' 56\")"))))

        (testing "escaped delimeters"
          (are [t] (= #{t} (title (str "[abc](xyz \"" t "\")")))
               "1\\\"23"
               "12\\\"3"
               "1\\\"2\\\"3"))

        (testing "unescaped delimeters"
          (are [t] (empty? (match (str "[abc](xyz \"" t "\")")))
               "1\"23"
               "12\"3"
               "1\"2\"3"))

        (testing "line breaks"
          (testing "single"
            (is (= #{"12\n34\n56"} (title "[abc](xyz \"12\n34\n56\")"))))

          (testing "multiple"
            (is (empty? (match "[abc](xyz \"12\n\n34\")"))))))

      (testing "()-delimited"
        (testing "minimal"
          (is (= #{"123"} (title (str "[abc](xyz (123))")))))

        (testing "escaped delimeters"
          (are [t] (= #{t} (title (str "[abc](xyz (" t "))")))
               "1\\(23"
               "12\\(3"
               "1\\(2\\(3"
               "1\\)23"
               "12\\)3"
               "1\\)2\\)3"
               "1\\(2\\)3"
               "1\\)2\\(3"))

        (testing "unescaped delimeters"
          (are [t] (empty? (match (str "[abc](xyz (" t "))")))
               "1(23"
               "12(3"
               "1(2(3"
               "1)23"
               "12)3"
               "1)2)3"
               "1(2)3"
               "1)2(3"))

        (testing "line breaks"
          (testing "single"
            (is (= #{"12\n34\n56"} (title "[abc](xyz (12\n34\n56))"))))

          (testing "multiple"
            (is (empty? (match "[abc](xyz (12\n\n34))")))))

        (testing "backslash escapes"
          (is (= #{"be there in 5\\\""} (title "[abc](xyz \"be there in 5\\\"\")"))))

        (testing "entity"
          (is (= #{"be there in 5&quot;"} (title "[abc](xyz \"be there in 5&quot;\")"))))))

    (testing "separating destination from title with non-unicode whitespace"
      (are [c] (= #{(str "xyz" c "123")} (destination (str "[abc](xyz" c "123)")))
           \u00A0
           \u1680
           \u2000
           \u2001
           \u2002
           \u2003
           \u2004
           \u2005
           \u2006
           \u2007
           \u2008
           \u2009
           \u200A
           \u202F
           \u205F
           \u3000))

    (testing "whitespace around destination and title"
      (testing "destination"
        (is (= #{"xyz"} (destination "[abc]( \t\nxyz \t\n'12 34' \t\n)"))))

      (testing "title"
        (is (= #{"12 34"} (title "[abc]( \t\nxyz \t\n'12 34' \t\n)")))))

    (testing "whitespace between text and destination"
      (are [s] (empty? (match (str "[abc]" s "(xyz)")))
           \space
           \newline
           \tab
           " \n\t"))

    (testing "opening bracket is escaped"
      (is (empty? (match (str "\\[abc](xyz)")))))

    (testing "all in one"
      (let [s "[p `code` *em*](http://example.com 'The title')"]
        (testing "text"
          (is (= #{"p `code` *em*"} (text s))))

        (testing "destination"
          (is (= #{"http://example.com"} (destination s))))

        (testing "title"
          (is (= #{"The title"} (title s)))))))

  (testing "inline image"
    (defn match [s]
      (->> s tokenize (filter (comp #{:img} :tag))))

    (defn text [s]
      (->> s match (keep :text) set))

    (defn destination [s]
      (->> s match (keep :destination) set))

    (defn title [s]
      (->> s match (keep :title) set))

    (testing "whitespace"
      (let [s "My ![abc def](/xyz \"123\"   )"]
        (testing "tag"
          (is (not-empty (match s))))

        (testing "text"
          (is (= #{"abc def"} (text s))))

        (testing "destination"
          (is (= #{"/xyz"} (destination s))))

        (testing "title"
          (is (= #{"123"} (title s))))))

    (testing "description"
      (testing "empty"
        (is (= #{""} (text "![](xyz)")))))

    (testing "destination"
      (testing "<>-delimited"
        (is (= #{"xyz"} (destination "![abc](<xyz>)"))))))

  (testing "link reference"
    (defn context [label]
      {:definitions {label {:title "t"
                            :destination "d"
                            :label label}}})

    (defn match [label s]
      (->> (tokenize s (context label)) (filter (comp #{:a} :tag))))

    (defn text [label s]
      (->> s (match label) (keep :text) set))

    (defn destination [label s]
      (->> s (match label) (keep :destination) set))

    (defn title [label s]
      (->> s (match label) (keep :title) set))

    (testing "full"
      (testing "text"
        (testing "minimal"
          (is (= #{"abc"} (text "xyz" "[abc][xyz]"))))

        (testing "empty"
          (is (= #{""} (text "xyz" "[][xyz]"))))

        (testing "inline elements"
          (is (= (text "xyz" "[*abc* `def` **ghi**][xyz]")
                 #{"*abc* `def` **ghi**"})))

        (testing "brackets"
          (testing "backslash-escaped"
            (is (= (text "xyz" "[a\\]b\\[c][xyz]")
                   #{"a\\]b\\[c"})))

          (testing "matched"
            (is (= (text "xyz" "[1[2[3]4]5][xyz]")
                   #{"1[2[3]4]5"})))))

      (testing "destination"
        (testing "minimal"
          (is (= #{"d"} (destination "xyz" "[abc][xyz]"))))

        (testing "brackets"
          (testing "backslash-escaped"
            (is (not-empty (match "x\\]y\\[z" "[abc][x\\]y\\[z]"))))))

      (testing "title"
        (testing "minimal"
          (is (= #{"t"} (title "xyz" "[abc][xyz]")))))

      (testing "label"
        (testing "brackets"
          (testing "backslash-escaped"
            (is (not-empty (match "x\\]y\\[z" "[abc][x\\]y\\[z]")))))))

    (testing "collapsed"
      (testing "minimal"
        (is (not-empty (match "abc" "[abc][]"))))

      (testing "text"
        (is (= #{"abc"} (text "abc" "[abc][]"))))

      (testing "destination"
        (is (= #{"d"} (destination "abc" "[abc][]"))))

      (testing "title"
        (is (= #{"t"} (title "abc" "[abc][]"))))

      (testing "brackets"
        (testing "backslash-escaped"
          (is (not-empty (match "a\\]b\\[c" "[a\\]b\\[c][]"))))))

    (testing "shortcut"
      (testing "minimal"
        (is (not-empty (match "abc" "[abc]"))))

      (testing "text"
        (is (= #{"abc"} (text "abc" "[abc]"))))

      (testing "destination"
        (is (= #{"d"} (destination "abc" "[abc]"))))

      (testing "title"
        (is (= #{"t"} (title "abc" "[abc]"))))

      (testing "brackets"
        (testing "backslash-escaped"
          (is (not-empty (match "a\\]b\\[c" "[a\\]b\\[c]"))))))))

(deftest autolink-test
  (defn match [s]
    (re-find re.inline/autolink s))

  (defn destination [s]
    (some->> (match s) (hash-map :re/match) autolink :destination))

  (defn text [s]
    (some->> (match s) (hash-map :re/match) autolink :text))

  (testing "URI"
    (testing "destination"
      (testing "valid URIs"
        (are [s] (= s (destination (str "<" s ">")))
             "http://abc.xyz.123"
             "http://abc.xyz.123/test?q=hello&id=22&boolean"
             "irc://abc.xyz:2233/123"
             "MAILTO:ABC@XYZ.123"
             "a+b+c:d"
             "made-up-scheme://abc,xyz"
             "http://../"
             "localhost:5001/abc"))

      (testing "invalid URIs"
        (are [s] (nil? (match s))
             "<>"
             "< http://abc.xyz >"
             "<http://ab\tc.xyz>"
             "<http://ab\nc.xyz>"
             "<http://ab\rc.xyz>"
             "<http://ab\r\nc.xyz>"
             "<m:abc>"
             "<abc.xyz.123>"))

      (testing "not wrapped in <>"
        (are [s] (nil? (match s))
             "http://xyz.com"
             "abc@qpr.xyz.com"))

      (testing "contains space"
        (is (nil? (match "<http://abc.xyz/qpr jkl>"))))

      (testing "backslash"
        (is (= (destination "<http://abc.com/\\xyz>")
               "http://abc.com/%5Cxyz")))

      (testing "percent encoding"
        (are [c e] (= (destination (str "<http://abc.com/" c ">"))
                      (str "http://abc.com/" e))
             \" "%22"
             \[ "%5B"
             \\ "%5C"
             \] "%5D"
             \` "%60"
             \{ "%7B"
             \| "%7C"
             \} "%7D"))

      (testing "percent decoding"
        (are [uri] (= (destination (str "<" uri ">"))
                      uri)
             "http://abc.com/%3C"
             "http://abc.com/%3E"
             "http://abc.com/%20")))

    (testing "text"
      (testing "backslash escape"
        (is (= (text "<http://abc.com/\\xyz>")
               "http://abc.com/\\xyz")))

      (testing "percent decoding"
        (are [e] (let [uri (str "http://abc.com/" e)]
                   (= (text (str "<" uri ">"))
                      uri))
             "%3C"
             "%3E"
             "%20"))

      (testing "percent encoding"
        (are [c] (= (text (str "<http://abc.com/" c ">"))
                    (str "http://abc.com/" c))
             \"
             \[
             \\
             \]
             \`
             \{
             \|
             \}))))

  (testing "email address"
    (testing "destination"
      (testing "valid addresses"
        (are [s] (= (destination (str "<" s ">"))
                    (str "mailto:" s))
             "abc@xyz.example.com"
             "abc+special@Xyz.123-xyz0.com")))

    (testing "text"
      (testing "valid addresses"
        (are [s] (= s (text (str "<" s ">")))
             "abc@xyz.example.com"
             "abc+special@Xyz.123-xyz0.com")))

    (testing "backslash escape"
      (is (nil? (match "<foo\\+@bar.example.com>"))))))

(deftest html-test
  (defn match [s]
    (re-find re.html/tag s))

  (defn content [s]
    (some->> (match s) (hash-map :re/match) html :content))

  (testing "open tags"
    (testing "simple"
      (are [s] (= s (content s))
           "<a>"
           "<p>"
           "<bab>"
           "<c2c>"
           "<a/>"
           "<b2/>"))

    (testing "backslash escaping"
      (are [s] (nil? (match s))
           "\\<p>"
           "\\<p class=\"xyz\">"
           "\\<p/>"))

    (testing "whitespace"
      (are [s] (= s (content s))
           "<a  />"
           "<b2\ndata=\"xyz\" >"))

    (testing "tag names"
      (testing "valid"
        (are [s] (= s (content s))
             "<p>"
             "<p2>"
             "<p->"
             "<pP>"))

      (testing "invalid"
        (are [s] (nil? (match s))
             "<_>"
             "<p_>"
             "<-p>"
             "<2p>")))

    (testing "attributes"
      (testing "name"
        (testing "valid"
          (are [s] (= s (content s))
               "<p abc=xyz>"
               "<p _bc=xyz>"
               "<p :bc=xyz>"
               "<p a2c=xyz>"
               "<p a.c=xyz>"
               "<p a:c=xyz>"
               "<p a-c=xyz>"))

        (testing "invalid"
          (are [s] (nil? (match s))
               "<p 2bc=xyz>"
               "<p .bc=xyz>"
               "<p -bc=xyz>"
               "<p a#c=xyz>"
               "<p a*c=xyz>")))

      (testing "value"
        (testing "unquoted"
          (testing "valid"
            (are [s] (= s (content s))
                 "<p abc=xyz>"
                 "<p abc = xyz>"
                 "<p abc = xyz123>"
                 "<p abc=@#$%^&*()_+->"
                 "<p abc=&quot;>"
                 "<p abc=\\*>"))

          (testing "invalid"
            (are [s] (not= s (content s))
                 "<p abc=xy'z>"
                 "<p abc=xy\"z>"
                 "<p abc=xy=z>"
                 "<p abc=xy<z>"
                 "<p abc=xy>z>"
                 "<p abc=xy`z>")))

        (testing "single-quoted"
          (are [s] (= s (content s))
               "<p abc='xyz'>"
               "<p abc = 'xyz'>"
               "<p abc = 'xyz \"123\"'>"
               "<p abc = 'xyz \"123\"=123'>"
               "<p abc = 'xyz \"123\"=123 <em>'>"
               "<p abc = 'xyz \"123\"=123 <em> `ls`'>"
               "<p abc = '&quot;'>"
               "<p abc = '\\*'>"))

        (testing "double-quoted"
          (are [s] (= s (content s))
               "<p abc=\"xyz\">"
               "<p abc = \"xyz\">"
               "<p abc = \"xyz '123'=123\">"
               "<p abc = \"xyz '123'=123 <em>\">"
               "<p abc = \"xyz '123'=123 <em> `ls`\">"
               "<p abc = \"&quot;\">"
               "<p abc = \"\\*\">"))))

    (testing "whitespace"
      (testing "misplaced"
        (are [s] (nil? (match s))
                 "< p>"
                 "<\np>"
                 "<a/ >"))

      (testing "missing"
        (is (nil? (match "<p a='b'c='d'>"))))))

  (testing "closing tags"
    (testing "tag names"
      (testing "valid"
        (are [s] (= s (content s))
             "</p>"
             "</p2>"
             "</p->"
             "</pP>"))

      (testing "invalid"
        (are [s] (nil? (match s))
             "\\</p>"
             "</_>"
             "</p_>"
             "</-p>"
             "</2p>")))

    (testing "attributes"
      (is (nil? (match "</p abc=xyz>")))))

  (testing "comments"
    (testing "valid"
      (are [s] (= s (content s))
           "<!--x-->"
           "<!---->"
           "<!-- x -->"
           "<!-- x y -->"
           "<!-- x\ny -->"
           "<!-- x-y -->"
           "<!-- x<!->y -->"))

    (testing "invalid"
      (are [s] (nil? (match s))
           "\\<!--x-->"
           "<!--x--->"
           "<!--x--y-->"
           "<!-->x-->"
           "<!--->x-->")))

  (testing "processing instructions"
    (testing "valid"
      (are [s] (= s (content s))
           "<?x?>"
           "<? x ?>"
           "<??>"
           "<? x y ?>"
           "<? x\ny ?>"
           "<? x<y ?>"
           "<? x?y ?>"
           "<? x<?y ?>"
           "<? x? >y ?>"
           "<? x<??y ?>"))

    (testing "invalid"
      (are [s] (nil? (match s))
           "\\<? x ?>"
           "<\\? x ?>")))

  (testing "declarations"
    (testing "valid"
      (are [s] (= s (content s))
           "<!X >"
           "<!X x>"
           "<!X x >"
           "<!X x y>"
           "<!X x\ny>"
           "<!X x <!X y>"))

    (testing "followed by >"
      (is (= (content "<!X x>>")
             "<!X x>")))

    (testing "invalid"
      (are [s] (nil? (match s))
           "<!X>"
           "\\<!X x>"
           "<\\!X x>")))

  (testing "cdata sections"
    (testing "valid"
      (are [s] (= s (content s))
           "<![CDATA[xyz]]>"
           "<![CDATA[xy z]]>"
           "<![CDATA[x y z]]>"
           "<![CDATA[]]>"
           "<![CDATA[xy\nz]]>"
           "<![CDATA[xy<![CDATA[z]]>"
           "<![CDATA[>&<]]>"))

    (testing "followed by >"
      (is (= (content "<![CDATA[xyz]]>>")
             "<![CDATA[xyz]]>")))

    (testing "invalid"
      (are [s] (nil? (match s))
           "\\<![CDATA[X x]]>"
           "<\\![CDATA[X x]]>"))))

(deftest hard-line-break-test
  (defn match [s]
    (re-find re.inline/hard-line-break s))

  (defn content [s]
    (some->> (match s) (hash-map :re/match) hard-line-break :content))

  (testing "standard"
    (are [s c] (= c (content s))
         "abc  \nxyz"   "  \n"
         "abc  \rxyz"   "  \r"
         "abc  \r\nxyz" "  \r\n"
         "abc\\\nxyz"   "\\\n"
         "abc\\\rxyz"   "\\\r"
         "abc\\\r\nxyz" "\\\r\n"))

  (testing "alone on a line"
    (testing "space"
      (is (nil? (match "  \nabc"))))

    (testing "backslash"
      (is (some? (match "\\\nabc"))))))

(deftest soft-line-break-test
  (defn match [s]
    (re-find re.inline/soft-line-break s))

  (defn content [s]
    (some->> (match s) (hash-map :re/match) soft-line-break :content))

  (testing "preceded by spaces"
    (are [s] (nil? (match s))
         "  \nxyz"
         "   \nxyz"
         "    \nxyz"))

  (testing "preceded by backslash"
    (is (nil? (match "\\\nxyz"))))

  (testing "standard"
    (are [s c] (= c (content s))
         "abc \nxyz"   "\n"
         "abc \rxyz"   "\r"
         "abc \r\nxyz" "\r\n"
         "abc\nxyz"    "\n"
         "abc\rxyz"    "\r"
         "abc\r\nxyz"  "\r\n"))

  (testing "by itself in a line"
    (is (nil? (match "\nabc")))))


