(ns commonmark.inline-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [commonmark.inline :refer :all]
            [commonmark.re.link :as re.link]
            [commonmark.re.html :as re.html]))

(deftest code-span-test
  (defn match [s]
    (re-find code-span-re s))

  (defn content [s]
    (some->> (match s) code-span :content))

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

    (testing "not unicode whitespace"
      (are [c] (= (count (content (str "`" c "foo" c "`")))
                  5)
           \u0009
;          \u000A
           \u000C
           \u000D
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
  (let [e* (emphasis-delimeter-re \* 1)
        e_ (emphasis-delimeter-re \_ 1)
        s* (emphasis-delimeter-re \* 2)
        s_ (emphasis-delimeter-re \_ 2)
        le* (lfdr-re e*)
        le_ (lfdr-re e_)
        ls* (lfdr-re s*)
        ls_ (lfdr-re s_)
        re* (rfdr-re e*)
        re_ (rfdr-re e_)
        rs* (rfdr-re s*)
        rs_ (rfdr-re s_)
        match? (fn [s re] (->> s (re-find re) some?))]
    (testing "left-flanking"
      (are [s le*? le_? ls*? ls_? re*? re_? rs*? rs_?]
           (and (= (match? s le*) le*?)
                (= (match? s le_) le_?)
                (= (match? s ls*) ls*?)
                (= (match? s ls_) ls_?)
                (= (match? s re*) re*?)
                (= (match? s re_) re_?)
                (= (match? s rs*) rs*?)
                (= (match? s rs_) rs_?))
           ; left yes, right no
;          "***abc"             true false  true false false false false false
           "*abc"               true false false false false false false false
           "**\"abc \""        false false  true false false false false false
           "*\"abc \""          true false false false false false false false
           "_\"abc \""         false  true false false false false false false
           "_abc"              false  true false false false false false false
           ; left no, right yes
;          "abc***"            false false false false  true false false false
           "abc_"              false false false false false  true false false
           "\"abc \"**"        false false false false false false  true false
           "\"abc \"_"         false false false false false  true false false
           ; left yes, right yes
;          "abc***def"
;          "\"abc \"_\"def \""
           ; left no, right no
           "abc *** def"       false false false false false false false false
           "a _ b"             false false false false false false false false))))

(deftest emphasis-test
  (defn match [s]
    (re-find (emphasis-re 1) s))

  (defn content [s]
    (some->> (match s) emphasis :content))

  (testing "opening with *"
    (testing "minimal"
      (is (= "foo bar" (content "*foo bar*"))))

    (testing "multiple"
      (is (= "abc" (content "*abc* xyz *def*"))))

    (testing "opening * followed by whitespace"
      (is (nil? (match "a * foo bar*"))))

    (testing "opening * preceded by alphanumeric and followed by punctuation"
      (is (nil? (match "a*\"foo\"*"))))

    (testing "unicode nonbreaking spaces"
      (are [c] (nil? (match (str "*" c "a *")))
           \u0009
           \u000A
           \u000C
           \u000D
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

    (testing "intraword"
      (are [s c] (= c (content s))
           "foo*bar*" "bar"
           "5*6*78"   "6"))

    (testing "multiple"
      (are [s] (= "xyz" (content s))
               "*xyz* *abc*"
               "*xyz* qpr *abc*"
           "def *xyz* qpr *abc*"
           "def *xyz* qpr *abc* 123")))

  (testing "opening with _"
    (testing "minimal"
      (is (= "foo bar" (content "_foo bar_"))))

    (testing "multiple"
      (is (= "abc" (content "_abc_ xyz _def_"))))

    (testing "part of rfdr preceded by punctuation and followed by punctuation"
      (is (= ".xyz" (content "._.xyz_"))))

    (testing "opening _ followed by whitespace"
      (is (nil? (match "_ foo bar_"))))

    (testing "opening _ preceded by alphanumeric and followed by punctuation"
      (is (nil? (match "a_\"foo \"_"))))

    (testing "intraword"
      (are [s] (nil? (match s))
           "foo_bar_"
           "5_6_78"))

    (testing "left-flank, right-flank"
      (is (nil? (match "aa_\"bb\"_cc"))))

    (testing "left-flank, right-flank, preceded by punctuation"
      (is (= "(bar)" (content "foo-_(bar)_")))))

  (testing "closing with *"
    (testing "closing and opening delimiter don't match"
      (is (nil? (match "_foo*"))))

    (testing "preceded by whitespace"
      (are [s] (nil? (match s))
           "*foo bar *"
           "*foo bar\n*"))

    (testing "preceded by punctuation, followed by alphanumeric"
      (is (nil? (match "*(*foo)"))))

    (testing "preceded by punctuation, followed by whitespace"
      (is (= "(xyz)" (content "*(xyz)*"))))

    (testing "nested"
      (is (= "(*xyz*)" (content "*(*xyz*)*"))))

    (testing "intraword"
      (is (= "foo" (content "*foo*bar")))))

  (testing "closing with _"
    (testing "preceded by whitespace"
      (is (nil? (match "_foo bar _"))))

    (testing "nested"
      (is (= "(_xyz_)" (content "_(_xyz_)_"))))

    (testing "preceded by punctuation, followed by whitespace"
      (is (= "(xyz)" (content "_(xyz)_"))))

    (testing "preceded by punctuation, followed by alphanumeric"
      (is (nil? (match "_(_foo)"))))

    (testing "lfdr followed by punctuation"
      (are [s c] (= c (content s))
           "_xyz)_(" "xyz)"
           "_(xyz)_." "(xyz)"))

    (testing "intraword"
      (are [s] (nil? (match s))
           "_foo_bar"
           "foo_bar_baz"))))

(deftest strong-emphasis-test
  (defn match [s]
    (re-find (emphasis-re 2) s))

  (defn content [s]
    (some->> (match s) strong-emphasis :content))

  (testing "opening with **"
    (testing "minimal"
      (is (= "foo bar" (content "**foo bar**"))))

    (testing "followed by whitespace"
      (is (nil? (match "** foo bar**"))))

    (testing "preceded by alphanumeric, followed by punctuation"
      (is (nil? (match "a**\"foo\"**"))))

    (testing "intraword"
      (is (= "bar" (content "foo**bar**"))))

    (testing "nested"
      (is (= "(**xyz**)" (content "**(**xyz**)**")))))

  (testing "opening with __"
    (testing "minimal"
      (is (= "foo bar" (content "__foo bar__"))))

    (testing "nested"
      (is (= "(__xyz__)" (content "__(__xyz__)__"))))

    (testing "followed by whitespace"
      (are [s] (nil? (match s))
           "__ foo bar__"
           "__\nfoo bar__"))

    (testing "preceded by alphanumeric, followed by punctuation"
      (is (nil? (match "a__\"foo\"__"))))

    (testing "intraword"
      (are [s] (nil? (match s))
           "foo__bar__"
           "5__6__78"))

    (testing "left-flank, right-flank, preceded by punctuation"
      (is (= "(bar)" (content "foo-__(bar)__")))))

  (testing "closing with **"
    (testing "closing and opening delimiter don't match"
      (is (nil? (match "__foo**"))))

    (testing "preceded by whitespace"
      (are [s] (nil? (match s))
           "**foo bar **"
           "**foo bar\n**"))

    (testing "preceded by punctuation, followed by alphanumeric"
      (is (nil? (match "**(**foo)"))))

    (testing "preceded by punctuation, followed by whitespace"
      (are [s c] (= c (content s))
           "*(**foo**)*" "foo"
           "**Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)**"
           "Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)"
           "**foo \"*bar*\" foo**" "foo \"*bar*\" foo"))

    (testing "intraword"
      (is (= "foo" (content "**foo**bar")))))

  (testing "closing with __"
    (testing "preceded by whitespace"
      (are [s] (nil? (match s))
           "__foo bar __"
           "__foo bar\n__"))

    (testing "preceded by punctuation, followed by alphanumeric"
      (is (nil? (match "__(__foo)"))))

    (testing "preceded by punctuation, followed by whitespace"
      (are [s c] (= c (content s))
           "_(__foo__)_" "foo"
           "__foo \"_bar_\" foo__" "foo \"_bar_\" foo"))

    (testing "intraword"
      (is (nil? (match "__foo__bar"))))

    (testing "intraword"
      (is (= "foo__bar__baz" (content "__foo__bar__baz__"))))

    (testing "left-flank, right-flank, followed by punctuation"
      (is (= "(bar)" (content "__(bar)__."))))))

(deftest inline-link-test
  (defn match [s]
    (re-find re.link/inline-re s))

  (defn text [s]
    (some->> (match s) inline-link :text))

  (defn destination [s]
    (some->> (match s) inline-link :destination))

  (defn title [s]
    (some->> (match s) inline-link :title))

  (defn tag [s]
    (some->> (match s) inline-link :tag))

  (testing "invalid input"
    (is (nil? (match "not-a-valid-inline-link"))))

  (testing "omit destination"
    (is (some? (match "[abc]()"))))

  (testing "omit title"
    (testing "text"
      (is (some? (text "[abc](xyz)"))))

    (testing "destination"
      (is (some? (destination "[abc](xyz)"))))

    (testing "title"
      (is (nil? (title "[abc](xyz)")))))

  (testing "text"
    (testing "backslash-escaped brackets"
      (are [s] (= s (text (str "[" s "](xyz)")))
           "abc\\]123"
           "abc\\[123"))

    (testing "balanced brackets"
      (are [s] (= s (text (str "[" s "](xyz)")))
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
      (are [s] (nil? (match s))
           "[]]()"
           "[[]]]()"
           "[[[]]]]()"))

    (testing "contains line breaks"
      (testing "single"
        (is (= "ab\ncd" (text "[ab\ncd](xyz)"))))

      (testing "multiple"
        (is (= "ab\ncd\nef" (text "[ab\ncd\nef](xyz)")))))

    (testing "contains blank line"
      (is (nil? (match "[ab\n\ncd](xyz)"))))

    (testing "nested links"
      (testing "text"
        (is (= "[in](in.com)" (text "[[in](in.com)](out.com)"))))

      (testing "destination"
        (is (= (destination "out.com"))))))

  (testing "destination"
    (testing "wrapped in <>"
      (testing "minimal"
        (is (= "xyz" (destination "[abc](<xyz>)"))))

      (testing "spaces"
        (is (= "xyz 123 qpr" (destination "[abc](<xyz 123 qpr>)"))))

      (testing "line breaks"
        (is (nil? (match "[abc](<123\nxyz>)"))))

      (testing "parentheses"
        (are [s d] (= d (destination s))
             "[abc](<123)xyz>)" "123)xyz"
             "[abc](<123(xyz>)" "123(xyz"))

      (testing "with title"
        (are [t] (= "xyz" (destination (str "[abc](<xyz> " t ")")))
             "'123'"
             "\"123\""))

      (testing "escaped delimeters"
        (are [s] (= s (destination (str "[abc](<" s ">)")))
             "123\\<xyz"
             "123\\>xyz"
             "123\\<qpr\\>xyz"))

      (testing "unescaped delimeters"
        (are [s] (nil? (match (str "[abc](<" s ">)")))
             "123<xyz"
             "123>xyz"
             "123<qpr>xyz"))

      (testing "improperly matched opening delimiters"
        (are [s] (nil? (match s))
             "[a] (<b)c"
             "[a] (<b)c>"
             "[a] (<b>c)")))

    (testing "not wrapped in <>"
      (testing "minimal"
        (is (= "xyz" (destination "[abc](xyz)"))))

      (testing "begins with <"
        (is (nil? (match "[abc](<xyz)"))))

      (testing "contains <"
        (is (= "x<yz" (destination "[abc](x<yz)"))))

      (testing "spaces"
        (is (nil? (match "[abc](xyz 123)"))))

      (testing "with title"
        (are [t] (= "xyz" (destination (str "[abc](xyz " t ")")))
             "'123'"
             "\"123\""))

      (testing "control characters"
        (are [s] (nil? (match (str "[abc](" s ")")))
             "123\rxyz"
             "123\nxyz"))

      (testing "parentheses"
        (testing "unescaped, unbalanced"
          (are [s] (nil? (match (str "[abc](" s ")")))
               "123(xyz"
               "123()xyz("
               "123((qpr))xyz("))

        (testing "unescaped, balanced"
          (are [s] (= s (destination (str "[abc](" s ")")))
               "()"
               "123()xyz"
               "123(!(qpr)!)xyz"))

        (testing "escaped"
          (are [s] (= s (destination (str "[abc](" s ")")))
               "123\\(xyz"
               "123\\)xyz"
               "123\\)q\\(pr\\)xyz"))))

    (testing "fragments and queries"
      (are [d] (= d (destination (str "[abc](" d ")")))
           "#fragment"
           "http://example.com#fragment"
           "http://example.com?foo=3#frag")))

  (testing "title"
    (testing "'-delimited"
      (testing "minimal"
        (is (= "123" (title "[abc](xyz '123')"))))

      (testing "double quotes"
        (is (= "12 \"34\" 56" (title "[abc](xyz '12 \"34\" 56')"))))

      (testing "escaped delimeters"
        (are [t] (= t (title (str "[abc](xyz '" t "')")))
             "1\\'23"
             "12\\'3"
             "1\\'2\\'3"))

      (testing "unescaped delimeters"
        (are [t] (nil? (match (str "[abc](xyz '" t "')")))
             "1'23"
             "12'3"
             "1'2'3"))

      (testing "line breaks"
        (testing "single"
          (is (= "12\n34\n56" (title "[abc](xyz '12\n34\n56')"))))

        (testing "multiple"
          (is (nil? (match "[abc](xyz '12\n\n34')"))))))

    (testing "\"-delimited"
      (testing "minimal"
        (is (= "123" (title "[abc](xyz \"123\")"))))

      (testing "single quotes"
        (is (= "12 '34' 56" (title "[abc](xyz \"12 '34' 56\")"))))

      (testing "escaped delimeters"
        (are [t] (= t (title (str "[abc](xyz \"" t "\")")))
             "1\\\"23"
             "12\\\"3"
             "1\\\"2\\\"3"))

      (testing "unescaped delimeters"
        (are [t] (nil? (match (str "[abc](xyz \"" t "\")")))
             "1\"23"
             "12\"3"
             "1\"2\"3"))

      (testing "line breaks"
        (testing "single"
          (is (= "12\n34\n56" (title "[abc](xyz \"12\n34\n56\")"))))

        (testing "multiple"
          (is (nil? (match "[abc](xyz \"12\n\n34\")"))))))

    (testing "()-delimited"
      (testing "minimal"
        (is (= "123" (title (str "[abc](xyz (123))")))))

      (testing "escaped delimeters"
        (are [t] (= t (title (str "[abc](xyz (" t "))")))
             "1\\(23"
             "12\\(3"
             "1\\(2\\(3"
             "1\\)23"
             "12\\)3"
             "1\\)2\\)3"
             "1\\(2\\)3"
             "1\\)2\\(3"))

      (testing "unescaped delimeters"
        (are [t] (nil? (match (str "[abc](xyz (" t "))")))
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
          (is (= "12\n34\n56" (title "[abc](xyz (12\n34\n56))"))))

        (testing "multiple"
          (is (nil? (match "[abc](xyz (12\n\n34))")))))

      (testing "backslash escapes"
        (is (= "be there in 5\\\"" (title "[abc](xyz \"be there in 5\\\"\")"))))

      (testing "entity"
        (is (= "be there in 5&quot;" (title "[abc](xyz \"be there in 5&quot;\")"))))))

  (testing "separating destination from title with non-unicode whitespace"
    (are [c] (= (str "xyz" c "123") (destination (str "[abc](xyz" c "123)")))
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
      (is (= "xyz" (destination "[abc]( \t\nxyz \t\n'12 34' \t\n)"))))

    (testing "title"
      (is (= "12 34" (title "[abc]( \t\nxyz \t\n'12 34' \t\n)")))))

  (testing "whitespace between text and destination"
    (are [s] (nil? (match (str "[abc]" s "(xyz)")))
         \space
         \newline
         \tab
         " \n\t"))

  (testing "opening bracket is escaped"
    (is (nil? (match (str "\\[abc](xyz)")))))

  (testing "all in one"
    (let [s "[p `code` *em*](http://example.com 'The title')"]
      (testing "text"
        (is (= "p `code` *em*" (text s))))

      (testing "destination"
        (is (= "http://example.com" (destination s))))

      (testing "title"
        (is (= "The title" (title s))))))

  (testing "image"
    (testing "whitespace"
      (let [s "My ![abc def](/xyz \"123\"   )"]
        (testing "tag"
          (is (= :img (tag s))))

        (testing "text"
          (is (= "abc def" (text s))))

        (testing "destination"
          (is (= "/xyz" (destination s))))

        (testing "title"
          (is (= "123" (title s))))))

    (testing "description"
      (testing "empty"
        (is (= (text "![](xyz)") ""))))

    (testing "destination"
      (testing "<>-delimited"
        (is (= "xyz" (destination "![abc](<xyz>)")))))))

(deftest autolink-test
  (defn match [s]
    (re-find autolink-re s))

  (defn uri [s]
    (some->> (match s) autolink :uri))

  (testing "URI"
    (testing "valid URIs"
      (are [s] (= s (uri (str "<" s ">")))
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
           "<m:abc>"
           "<abc.xyz.123>"))

    (testing "not wrapped in <>"
      (are [s] (nil? (match s))
           "http://xyz.com"
           "abc@qpr.xyz.com"))

    (testing "contains space"
      (is (nil? (match "<http://abc.xyz/qpr jkl>"))))

    (testing "backslash escape"
      (is (let [s "http://example.com/\\[\\"]
            (= s (uri (str "<" s ">"))))))

    (testing "label"
      (defn label [s]
        (some->> (match s) autolink :label))

      (testing "nothing to decode"
        (is (let [res (autolink (match "<https://abc.xyz?q=1>"))]
              (= (:label res)
                 (:uri res)))))

      (testing "percent decoding"
        (is (= (label "<https://a%09b%0Dc%0A.%20x%5By%5Cz?q=%3C1%3E>")
               "https://a\tb\rc\n. x[y\\z?q=<1>")))))

  (testing "email address"
    (testing "valid addresses"
      (are [s] (= s (uri (str "<" s ">")))
           "abc@xyz.example.com"
           "abc+special@Xyz.123-xyz0.com"))

    (testing "backslash escape"
      (is (nil? (match "<foo\\+@bar.example.com>"))))))

(deftest html-test
  (defn match [s]
    (re-find re.html/tag-re s))

  (defn content [s]
    (some->> (match s) html :content))

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
    (re-find hard-line-break-re s))

  (defn content [s]
    (some->> (match s) hard-line-break :content))

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
    (re-find soft-line-break-re s))

  (defn content [s]
    (some->> (match s) soft-line-break :content))

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

(deftest reference-link-test
  (defn match [s context]
    (re-find (re.link/reference-re (keys (:definitions context))) s))

  (defn matcher [context]
    (reference-link (:definitions context)))

  (defn text [s context]
    (some->> (match s context) ((matcher context)) :text))

  (defn destination [s context]
    (some->> (match s context) ((matcher context)) :destination))

  (defn title [s context]
    (some->> (match s context) ((matcher context)) :title))

  (defn context [label]
    {:definitions {label {:title "t"
                          :destination "d"
                          :label label}}})

  (testing "full"
    (testing "text"
      (testing "minimal"
        (is (= "abc" (text "[abc][xyz]" (context "xyz")))))

      (testing "empty"
        (is (= "" (text "[][xyz]" (context "xyz")))))

      (testing "inline elements"
        (is (= (text "[*abc* `def` **ghi**][xyz]" (context "xyz"))
               "*abc* `def` **ghi**")))

      (testing "brackets"
        (testing "backslash-escaped"
          (is (= (text "[a\\]b\\[c][xyz]" (context "xyz"))
                 "a\\]b\\[c")))

        (testing "matched"
          (is (= (text "[1[2[3]4]5][xyz]" (context "xyz"))
                 "1[2[3]4]5")))))

    (testing "destination"
      (testing "minimal"
        (is (= "d" (destination "[abc][xyz]" (context "xyz")))))

      (testing "brackets"
        (testing "backslash-escaped"
          (is (some? (match "[abc][x\\]y\\[z]" (context "x\\]y\\[z")))))))

    (testing "title"
      (testing "minimal"
        (is (= "t" (title "[abc][xyz]" (context "xyz"))))))

    (testing "label"
      (testing "brackets"
        (testing "backslash-escaped"
          (is (some? (match "[abc][x\\]y\\[z]" (context "x\\]y\\[z"))))))))

  (testing "collapsed"
    (testing "minimal"
      (is (some? (match "[abc][]" (context "abc")))))

    (testing "text"
      (is (= "abc" (text "[abc][]" (context "abc")))))

    (testing "destination"
      (is (= "d" (destination "[abc][]" (context "abc")))))

    (testing "title"
      (is (= "t" (title "[abc][]" (context "abc")))))

    (testing "brackets"
      (testing "backslash-escaped"
        (is (some? (match "[a\\]b\\[c][]" (context "a\\]b\\[c")))))))

  (testing "shortcut"
    (testing "minimal"
      (is (some? (match "[abc]" (context "abc")))))

    (testing "text"
      (is (= "abc" (text "[abc]" (context "abc")))))

    (testing "destination"
      (is (= "d" (destination "[abc]" (context "abc")))))

    (testing "title"
      (is (= "t" (title "[abc]" (context "abc")))))

    (testing "brackets"
      (testing "backslash-escaped"
        (is (some? (match "[a\\]b\\[c]" (context "a\\]b\\[c"))))))))

