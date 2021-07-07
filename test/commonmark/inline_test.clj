(ns commonmark.inline-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [commonmark.inline :refer :all]))

(deftest code-span-test
  (testing "minimal"
    (is (= (->> "`foo`" code-span :content)
           "foo")))

  (testing "multiple"
    (is (= (->> "`foo` bar `baz`" code-span :content)
           "foo")))

  (testing "backtick in contents"
    (is (= (->> "``foo`bar``" code-span :content)
           "foo`bar")))

  (testing "wrap backtick pairs"
    (is (= (->> "`` `ls -l` ``" code-span :content)
           "`ls -l`")))

  (testing "strip"
    (testing "single space"
      (are [s c] (= (->> s code-span :content)
                    c)
           "` foo `"   "foo"
           "`  foo  `" " foo "
           "` `` `"    "``"))

    (testing "space not on both ends => don't"
      (is (= (->> "` foo`" code-span :content)
             " foo")))

    (testing "not unicode whitespace => don't"
      (are [c] (let [s (str c "foo" c)]
                 (= (->> (str "`" s "`") code-span :content count)
                    5))
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

    (testing "contains only spaces => don't"
      (are [s c] (= (->> s code-span :content)
                    c)
           "` `"  " "
           "`  `" "  ")))

  (testing "line endings => treat like spaces"
    (are [s c] (= (->> s code-span :content)
                  c)
         "``\nfoo\nbar  \nbaz\n``" "foo bar   baz"
         "``\nfoo \n``" "foo "))

  (testing "interior spaces => don't collapse"
    (is (= (->> "`foo   bar \nbaz`" code-span :content)
           "foo   bar  baz")))

  (testing "backtick strings unequal length"
    (testing "no match"
      (are [s] (nil? (code-span s))
           "```foo``"
           "`foo"
           "`foo``"))

    (testing "match"
      (are [s c] (= c (->> s code-span :content))
           "`foo``bar``" "bar")))

  (testing "line endings"
    (are [s c] (= c (->> s code-span :content))
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
  (testing "opening with *"
    (testing "minimal"
      (is (= (-> "*foo bar*" emphasis :content)
             "foo bar")))

    (testing "multiple => match first"
      (is (= (-> "*abc* xyz *def*" emphasis :content)
             "abc")))

    (testing "opening * followed by whitespace => not emphasis"
      (is (nil? (emphasis "a * foo bar*"))))

    (testing "opening * preceded by alphanumeric and followed by punctuation => not emphasis"
      (is (nil? (emphasis "a*\"foo\"*"))))

    (testing "unicode nonbreaking spaces => not emphasis"
      (are [c] (nil? (emphasis (str "*" c "a *")))
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
      (are [s c] (= (-> s emphasis :content)
                    c)
           "foo*bar*" "bar"
           "5*6*78"   "6"))

    (testing "multiple"
      (are [s] (= (-> s emphasis :content)
                  "xyz")
               "*xyz* *abc*"
               "*xyz* qpr *abc*"
           "def *xyz* qpr *abc*"
           "def *xyz* qpr *abc* 123")))

  (testing "opening with _"
    (testing "minimal"
      (is (= (-> "_foo bar_" emphasis :content)
             "foo bar")))

    (testing "multiple => match first"
      (is (= (-> "_abc_ xyz _def_" emphasis :content)
             "abc")))

    (testing "part of rfdr preceded by punctuation and followed by punctuation"
      (is (= (-> "._.xyz_" emphasis :content)
             ".xyz")))

    (testing "opening _ followed by whitespace => not emphasis"
      (is (nil? (emphasis "_ foo bar_"))))

    (testing "opening _ preceded by alphanumeric and followed by punctuation => not emphasis"
      (is (nil? (emphasis "a_\"foo \"_"))))

    (testing "intraword"
      (are [s] (nil? (emphasis s))
           "foo_bar_"
           "5_6_78"))

    (testing "left-flank, right-flank => not emphasis"
      (is (nil? (emphasis "aa_\"bb\"_cc"))))

    (testing "left-flank, right-flank, preceded by punctuation => emphasis"
      (is (= (-> "foo-_(bar)_" emphasis :content)
             "(bar)"))))

  (testing "closing with *"
    (testing "closing and opening delimiter don't match => not emphasis"
      (is (nil? (emphasis "_foo*"))))

    (testing "preceded by whitespace => not emphasis"
      (are [s] (nil? (emphasis s))
           "*foo bar *"
           "*foo bar\n*"))

    (testing "preceded by punctuation, followed by alphanumeric => not emphasis"
      (is (nil? (emphasis "*(*foo)"))))

    (testing "preceded by punctuation, followed by whitespace"
      (is (= (-> "*(xyz)*" emphasis :content)
             "(xyz)")))

    (testing "nested => matches innermost"
      (is (= (-> "*(*xyz*)*" emphasis :content)
             "xyz")))

    (testing "intraword"
      (is (= (-> "*foo*bar" emphasis :content)
             "foo"))))

  (testing "closing with _"
    (testing "preceded by whitespace => not emphasis"
      (is (nil? (emphasis "_foo bar _"))))

    (testing "nested => matches innermost"
      (is (= (-> "_(_xyz_)_" emphasis :content)
             "xyz")))

    (testing "preceded by punctuation, followed by whitespace"
      (is (= (-> "_(xyz)_" emphasis :content)
             "(xyz)")))

    (testing "preceded by punctuation, followed by alphanumeric => not emphasis"
      (is (nil? (emphasis "_(_foo)"))))

    (testing "lfdr followed by punctuation"
      (are [s c] (= c (-> s emphasis :content))
           "_xyz)_(" "xyz)"
           "_(xyz)_." "(xyz)"))

    (testing "intraword => not emphasis"
      (are [s] (nil? (emphasis s))
           "_foo_bar"
           "foo_bar_baz"))))

(deftest strong-emphasis-test
  (testing "opening with **"
    (testing "minimal"
      (is (= (-> "**foo bar**" strong-emphasis :content)
             "foo bar")))

    (testing "followed by whitespace => not strong emphasis"
      (is (nil? (strong-emphasis "** foo bar**"))))

    (testing "preceded by alphanumeric, followed by punctuation => not strong emphasis"
      (is (nil? (strong-emphasis "a**\"foo\"**"))))

    (testing "intraword"
      (is (= (-> "foo**bar**" strong-emphasis :content)
             "bar")))

    (testing "nested => matches innermost"
      (is (= (-> "**(**xyz**)**" strong-emphasis :content)
             "xyz"))))

  (testing "opening with __"
    (testing "minimal"
      (is (= (-> "__foo bar__" strong-emphasis :content)
             "foo bar")))

    (testing "nested => matches innermost"
      (is (= (-> "__(__xyz__)__" strong-emphasis :content)
             "xyz")))

    (testing "followed by whitespace => not strong emphasis"
      (are [s] (nil? (strong-emphasis s))
           "__ foo bar__"
           "__\nfoo bar__"))

    (testing "preceded by alphanumeric, followed by punctuation => not strong emphasis"
      (is (nil? (strong-emphasis "a__\"foo\"__"))))

    (testing "intraword"
      (are [s] (nil? (strong-emphasis s))
           "foo__bar__"
           "5__6__78"))

    (testing "left-flank, right-flank, preceded by punctuation => strong emphasis"
      (is (= (-> "foo-__(bar)__" strong-emphasis :content)
             "(bar)"))))

  (testing "closing with **"
    (testing "closing and opening delimiter don't match => not emphasis"
      (is (nil? (strong-emphasis "__foo**"))))

    (testing "preceded by whitespace => not emphasis"
      (are [s] (nil? (strong-emphasis s))
           "**foo bar **"
           "**foo bar\n**"))

    (testing "preceded by punctuation, followed by alphanumeric => not emphasis"
      (is (nil? (strong-emphasis "**(**foo)"))))

    (testing "preceded by punctuation, followed by whitespace"
      (are [s c] (= (-> s strong-emphasis :content))
           "*(**foo**)*" "foo"
           "**Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)**"
           "Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)"
           "**foo \"*bar*\" foo**" "foo \"*bar*\" foo"))

    (testing "intraword"
      (is (= (-> "**foo**bar" strong-emphasis :content)
             "foo"))))

  (testing "closing with __"
    (testing "preceded by whitespace => not emphasis"
      (are [s] (nil? (strong-emphasis s))
           "__foo bar __"
           "__foo bar\n__"))

    (testing "preceded by punctuation, followed by alphanumeric => not emphasis"
      (is (nil? (strong-emphasis "__(__foo)"))))

    (testing "preceded by punctuation, followed by whitespace"
      (are [s c] (= (-> s strong-emphasis :content))
           "_(__foo__)_" "foo"
           "__foo \"_bar_\" foo__" "foo \"_bar_\" foo"))

    (testing "intraword => not strong emphasis"
      (is (nil? (-> "__foo__bar" strong-emphasis :content))))

    (testing "intraword"
      (is (= (-> "__foo__bar__baz__" strong-emphasis :content)
             "foo__bar__baz")))

    (testing "left-flank, right-flank, followed by punctuation => strong emphasis"
      (is (= (-> "__(bar)__." strong-emphasis :content)
             "(bar)")))))

(deftest inline-link-test
  (testing "pun nil"
    (is (nil? (inline-link nil))))

  (testing "invalid input => nil"
    (is (nil? (inline-link "not-a-valid-inline-link"))))

  (testing "destination is optional"
    (is (some? (inline-link "[abc]()"))))

  (testing "title is optional"
    (is (let [{:keys [text destination title]} (inline-link "[abc](xyz)")]
          (and (some? text)
               (some? destination)
               (nil? title)))))

  (testing "text"
    (testing "bracket binding"
      (testing "less tightly than backticks"
        (is (= (-> "`[abc`](xyz)" tagger :tag)
               :cs)))

      (testing "more tightly than emphasis markers"
        (are [s] (= :a (-> s tagger :tag))
             "*[abc*](xyz)"
             "[abc *xyz](123*)")))

    (testing "backslash-escaped brackets"
      (are [s] (= s (-> (str "[" s "](xyz)") inline-link :text))
           "abc\\]123"
           "abc\\[123"))

    (testing "balanced brackets"
      (are [s] (= s (-> (str "[" s "](xyz)") inline-link :text))
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
      (are [s] (nil? (inline-link s))
           "[]]()"
           "[[]]]()"
           "[[[]]]]()"))

    (testing "nested links"
      (is (let [res (inline-link "[[inner](inner.com)](outer.com)")
                {:keys [text destination title]} res]
            (and (= text "inner")
                 (= destination "inner.com"))))))

  (testing "destination"
    (testing "wrapped in <>"
      (testing "minimal"
        (is (= (-> "[abc](<xyz>)" inline-link :destination)
               "xyz")))

      (testing "contains spaces"
        (is (= (-> "[abc](<xyz 123 qpr>)" inline-link :destination)
             "xyz 123 qpr")))

      (testing "contains line breaks"
        (is (nil? (inline-link "[abc](<123\nxyz>)"))))

      (testing "contains parentheses"
        (are [s d] (= d (-> s inline-link :destination))
             "[abc](<123)xyz>)" "123)xyz"
             "[abc](<123(xyz>)" "123(xyz"))

      (testing "with title"
        (are [t] (= (-> (str "[abc](<xyz> " t ")") inline-link :destination)
                    "xyz")
             "'123'"
             "\"123\""))

      (testing "contains escaped delimeters"
        (are [s] (= s (-> (str "[abc](<" s ">)") inline-link :destination))
             "123\\<xyz"
             "123\\>xyz"
             "123\\<qpr\\>xyz"))

      (testing "contains unescaped delimeters"
        (are [s] (nil? (-> (str "[abc](<" s ">)") inline-link))
             "123<xyz"
             "123>xyz"
             "123<qpr>xyz"))

      (testing "improperly matched opening delimiters"
        (are [s] (nil? (inline-link s))
             "[a] (<b)c"
             "[a] (<b)c>"
             "[a] (<b>c)")))

    (testing "not wrapped in <>"
      (testing "minimal"
        (is (= (-> "[abc](xyz)" inline-link :destination)
               "xyz")))

      (testing "begins with <"
        (is (nil? (-> "[abc](<xyz)" inline-link :destination))))

      (testing "contains <"
        (is (= (-> "[abc](x<yz)" inline-link :destination)
               "x<yz")))

      (testing "contains spaces"
        (is (nil? (-> "[abc](xyz 123)" inline-link :destination))))

      (testing "with title"
        (are [t] (= (-> (str "[abc](xyz " t ")") inline-link :destination)
                    "xyz")
             "'123'"
             "\"123\""))

      (testing "contains control characters"
        (are [s] (nil? (inline-link (str "[abc](" s ")")))
             "123\rxyz"
             "123\nxyz"))

      (testing "parentheses"
        (testing "unescaped, unbalanced"
          (are [s] (nil? (inline-link (str "[abc](" s ")")))
               "123(xyz"
               "123()xyz("
               "123((qpr))xyz("))

        (testing "unescaped, balanced"
          (are [s] (= s (-> (str "[abc](" s ")") inline-link :destination))
               "()"
               "123()xyz"
               "123(!(qpr)!)xyz"))

        (testing "escaped"
          (are [s] (= s (-> (str "[abc](" s ")") inline-link :destination))
               "123\\(xyz"
               "123\\)xyz"
               "123\\)q\\(pr\\)xyz"))))

    (testing "contains fragments and queries"
      (are [d] (= d (-> (str "[abc](" d ")") inline-link :destination))
           "#fragment"
           "http://example.com#fragment"
           "http://example.com?foo=3#frag")))

  (testing "title"
    (testing "'-delimited"
      (testing "minimal"
        (is (= (-> "[abc](xyz '123')" inline-link :title)
               "123")))

      (testing "contains double quotes"
        (is (= (-> "[abc](xyz '12 \"34\" 56')" inline-link :title)
               "12 \"34\" 56")))

      (testing "contains escaped delimeters"
        (are [t] (= t (-> (str "[abc](xyz '" t "')") inline-link :title))
             "1\\'23"
             "12\\'3"
             "1\\'2\\'3"))

      (testing "contains unescaped delimeters"
        (are [t] (nil? (-> (str "[abc](xyz '" t "')") inline-link))
             "1'23"
             "12'3"
             "1'2'3")))

    (testing "\"-delimited"
      (testing "minimal"
        (is (= (-> "[abc](xyz \"123\")" inline-link :title)
               "123")))

      (testing "contains single quotes"
        (is (= (-> "[abc](xyz \"12 '34' 56\")" inline-link :title)
               "12 '34' 56")))

      (testing "contains escaped delimeters"
        (are [t] (= t (-> (str "[abc](xyz \"" t "\")") inline-link :title))
             "1\\\"23"
             "12\\\"3"
             "1\\\"2\\\"3"))

      (testing "contains unescaped delimeters"
        (are [t] (nil? (-> (str "[abc](xyz \"" t "\")") inline-link))
             "1\"23"
             "12\"3"
             "1\"2\"3")))

    (testing "()-delimited"
      (testing "minimal"
        (are [t] (= (-> (str "[abc](xyz " t ")") inline-link :title)
                    "123")
             "(123)"
             "((123))"
             "(((123)))"
             "((((123))))"))

      (testing "unbalanced delimeters"
        (are [t] (nil? (-> (str "[abc](xyz " t ")") inline-link))
             "(123"
             "123)"
             "((123)"
             "(((123))"
             "((((123)))"))

      (testing "contains escaped delimeters"
        (are [t] (= t (-> (str "[abc](xyz (" t "))") inline-link :title))
             "1\\(23"
             "12\\(3"
             "1\\(2\\(3"
             "1\\)23"
             "12\\)3"
             "1\\)2\\)3"
             "1\\(2\\)3"
             "1\\)2\\(3"))

      (testing "contains unescaped delimeters"
        (are [t] (nil? (-> (str "[abc](xyz (" t "))") inline-link))
             "1(23"
             "12(3"
             "1(2(3"
             "1)23"
             "12)3"
             "1)2)3"
             "1(2)3"
             "1)2(3"))

      (testing "contains backslash escapes"
        (is (= (-> "[abc](xyz \"be there in 5\\\"\")" inline-link :title)
               "be there in 5\\\"")))

      (testing "contains entity"
        (is (= (-> "[abc](xyz \"be there in 5&quot;\")" inline-link :title)
               "be there in 5&quot;")))))

  (testing "separating destination from title with non-unicode whitespace"
    (are [c] (= (-> (str "[abc](xyz" c "123)") inline-link :destination)
                (str "xyz" c "123"))
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
    (is (let [res (inline-link "[abc]( \t\nxyz \t\n'12 34' \t\n)")
              {:keys [destination title]} res]
          (and (= destination "xyz")
               (= title "12 34")))))

  (testing "whitespace between text and destination"
    (are [s] (nil? (inline-link (str "[abc]" s "(xyz)")))
         \space
         \newline
         \tab
         " \n\t"))

  (testing "opening bracket is escaped"
    (is (nil? (inline-link (str "\\[abc](xyz)")))))

  (testing "all in one"
    (is (let [res (inline-link "[p `code` *em*](http://example.com 'The title')")
              {:keys [text destination title]} res]
          (and (= text "p `code` *em*")
               (= destination "http://example.com")
               (= title "The title")))))

  (testing "image"
    (testing "whitespace"
      (is (let [res (inline-link "My ![abc def](/xyz \"123\"   )")
                {:keys [tag text destination title]} res]
            (and (= tag :img)
                 (= text "abc def")
                 (= destination "/xyz")
                 (= title "123")))))

    (testing "description"
      (testing "empty"
        (is (let [res (inline-link "![](xyz)")
                  {:keys [tag text destination title]} res]
              (and (= tag :img)
                   (= text "")
                   (= destination "xyz"))))))

    (testing "destination"
      (testing "<>-delimited"
        (is (let [res (inline-link "![abc](<xyz>)")
                  {:keys [tag text destination title]} res]
              (and (= tag :img)
                   (= text "abc")
                   (= destination "xyz"))))))))

(deftest autolink-test
  (testing "URI"
    (testing "valid URIs"
      (are [s] (= s (->> (str "<" s ">") autolink :uri))
           "http://abc.xyz.123"
           "http://abc.xyz.123/test?q=hello&id=22&boolean"
           "irc://abc.xyz:2233/123"
           "MAILTO:ABC@XYZ.123"
           "a+b+c:d"
           "made-up-scheme://abc,xyz"
           "http://../"
           "localhost:5001/abc"))

    (testing "invalid URIs"
      (are [s] (nil? (autolink s))
           "<>"
           "< http://abc.xyz >"
           "<m:abc>"
           "<abc.xyz.123>"))

    (testing "not wrapped in <>"
      (are [s] (nil? (autolink s))
           "http://xyz.com"
           "abc@qpr.xyz.com"))

    (testing "contains space"
      (is (nil? (autolink "<http://abc.xyz/qpr jkl>"))))

    (testing "backslash escape"
      (is (let [s "http://example.com/\\[\\"]
            (= s (->> (str "<" s ">") autolink :uri)))))

    (testing "label"
      (testing "nothing to decode"
        (is (let [res (autolink "<https://abc.xyz?q=1>")]
              (= (:label res)
                 (:uri res)))))

      (testing "percent decoding"
        (is (= (->> "<https://a%09b%0Dc%0A.%20x%5By%5Cz?q=%3C1%3E>" autolink :label)
               "https://a\tb\rc\n. x[y\\z?q=<1>")))))

  (testing "email address"
    (testing "valid addresses"
      (are [s] (= s (->> (str "<" s ">") autolink :uri))
           "abc@xyz.example.com"
           "abc+special@Xyz.123-xyz0.com"))

    (testing "backslash escape"
      (is (nil? (autolink "<foo\\+@bar.example.com>"))))))

(deftest html-test
  (testing "open tags"
    (testing "simple open"
      (are [s] (= s (-> s html :content))
           "<a>"
           "<p>"
           "<bab>"
           "<c2c>"))

    (testing "empty"
      (are [s] (= s (-> s html :content))
           "<a/>"
           "<b2/>"))

    (testing "whitespace"
      (are [s] (= s (-> s html :content))
           "<a  />"
           "<b2\ndata=\"xyz\" >"))

    (testing "tag names"
      (testing "valid"
        (are [s] (= s (-> s html :content))
             "<p>"
             "<p2>"
             "<p->"
             "<pP>"))

      (testing "invalid"
        (are [s] (nil? (html s))
             "<_>"
             "<p_>"
             "<-p>"
             "<2p>")))

    (testing "attributes"
      (testing "name"
        (testing "valid"
          (are [s] (= s (-> s html :content))
               "<p abc=xyz>"
               "<p _bc=xyz>"
               "<p :bc=xyz>"
               "<p a2c=xyz>"
               "<p a.c=xyz>"
               "<p a:c=xyz>"
               "<p a-c=xyz>"))

        (testing "invalid"
          (are [s] (nil? (html s))
               "<p 2bc=xyz>"
               "<p .bc=xyz>"
               "<p -bc=xyz>"
               "<p a#c=xyz>"
               "<p a*c=xyz>")))

      (testing "value"
        (testing "unquoted"
          (testing "valid"
            (are [s] (= s (-> s html :content))
                 "<p abc=xyz>"
                 "<p abc = xyz>"
                 "<p abc = xyz123>"
                 "<p abc=@#$%^&*()_+->"
                 "<p abc=&quot;>"
                 "<p abc=\\*>"))

          (testing "invalid"
            (are [s] (not= s (-> s html :content))
                 "<p abc=xy'z>"
                 "<p abc=xy\"z>"
                 "<p abc=xy=z>"
                 "<p abc=xy<z>"
                 "<p abc=xy>z>"
                 "<p abc=xy`z>")))

        (testing "single-quoted"
          (are [s] (= s (-> s html :content))
               "<p abc='xyz'>"
               "<p abc = 'xyz'>"
               "<p abc = 'xyz \"123\"'>"
               "<p abc = 'xyz \"123\"=123'>"
               "<p abc = 'xyz \"123\"=123 <em>'>"
               "<p abc = 'xyz \"123\"=123 <em> `ls`'>"
               "<p abc = '&quot;'>"
               "<p abc = '\\*'>"))

        (testing "double-quoted"
          (are [s] (= s (-> s html :content))
               "<p abc=\"xyz\">"
               "<p abc = \"xyz\">"
               "<p abc = \"xyz '123'=123\">"
               "<p abc = \"xyz '123'=123 <em>\">"
               "<p abc = \"xyz '123'=123 <em> `ls`\">"
               "<p abc = \"&quot;\">"
               "<p abc = \"\\*\">"))))

    (testing "whitespace"
      (testing "misplaced"
        (are [s] (nil? (html s))
                 "< p>"
                 "<\np>"
                 "<a/ >"))

      (testing "missing"
        (is (nil? (html "<p a='b'c='d'>"))))))

  (testing "closing tags"
    (testing "tag names"
      (testing "valid"
        (are [s] (= s (-> s html :content))
             "</p>"
             "</p2>"
             "</p->"
             "</pP>"))

      (testing "invalid"
        (are [s] (nil? (html s))
             "</_>"
             "</p_>"
             "</-p>"
             "</2p>")))

    (testing "attributes"
      (is (nil? (html "</p abc=xyz>")))))

  (testing "comments"
    (testing "valid"
      (are [s] (= s (-> s html :content))
           "<!--x-->"
           "<!---->"
           "<!-- x -->"
           "<!-- x y -->"
           "<!-- x\ny -->"
           "<!-- x-y -->"
           "<!-- x<!->y -->"))

    (testing "invalid"
      (are [s] (nil? (html s))
           "\\<!--x-->"
           "<!--x--->"
           "<!--x--y-->"
           "<!-->x-->"
           "<!--->x-->")))

  (testing "processing instructions"
    (testing "valid"
      (are [s] (= s (-> s html :content))
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
      (are [s] (nil? (html s))
           "\\<? x ?>"
           "<\\? x ?>")))

  (testing "declarations"
    (testing "valid"
      (are [s] (= s (-> s html :content))
           "<!X >"
           "<!X x>"
           "<!X x >"
           "<!X x y>"
           "<!X x\ny>"
           "<!X x <!X y>"))

    (testing "followed by >"
      (is (= (-> "<!X x>>" html :content)
             "<!X x>")))

    (testing "invalid"
      (are [s] (nil? (html s))
           "<!X>"
           "\\<!X x>"
           "<\\!X x>")))

  (testing "cdata sections"
    (testing "valid"
      (are [s] (= s (-> s html :content))
           "<![CDATA[xyz]]>"
           "<![CDATA[xy z]]>"
           "<![CDATA[x y z]]>"
           "<![CDATA[]]>"
           "<![CDATA[xy\nz]]>"
           "<![CDATA[xy<![CDATA[z]]>"
           "<![CDATA[>&<]]>"))

    (testing "followed by >"
      (is (= (-> "<![CDATA[xyz]]>>" html :content)
             "<![CDATA[xyz]]>")))

    (testing "invalid"
      (are [s] (nil? (html s))
           "\\<![CDATA[X x]]>"
           "<\\![CDATA[X x]]>"))))

(deftest hard-line-break-test
  (testing "puns nil"
    (is (nil? (hard-line-break nil))))

  (testing "standard"
    (are [s c] (= c (-> s hard-line-break :content))
         "abc  \nxyz"   "  \n"
         "abc  \rxyz"   "  \r"
         "abc  \r\nxyz" "  \r\n"
         "abc\\\nxyz"   "\\\n"
         "abc\\\rxyz"   "\\\r"
         "abc\\\r\nxyz" "\\\r\n"))

  (testing "alone on a line"
    (testing "space"
      (is (nil? (hard-line-break "  \nabc"))))

    (testing "backslash"
      (is (some? (hard-line-break "\\\nabc"))))))

(deftest soft-line-break-test
  (testing "puns nil"
    (is (nil? (soft-line-break nil))))

  (testing "preceded by spaces"
    (are [s] (nil? (soft-line-break s))
         "  \nxyz"
         "   \nxyz"
         "    \nxyz"))

  (testing "preceded by backslash"
    (is (nil? (soft-line-break "\\\nxyz"))))

  (testing "standard"
    (are [s c] (= c (-> s soft-line-break :content))
         "abc \nxyz"   "\n"
         "abc \rxyz"   "\r"
         "abc \r\nxyz" "\r\n"
         "abc\nxyz"    "\n"
         "abc\rxyz"    "\r"
         "abc\r\nxyz"  "\r\n"))

  (testing "alone on a line"
    (is (nil? (soft-line-break "\nabc")))))

(deftest text-test
  (testing "puns nil"
    (is (nil? (text nil))))

  (testing "captures all"
    (let [s "abc *def* **ghi** [jkl](mno 'pqr') ![stu](vwx) `yz0`"]
      (is (= s (-> s text :content)))))

  (testing "removes backslash escapes from ASCII punctuation"
    (are [in out] (= out (-> in text :content))
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
         "\\~" "~")))

