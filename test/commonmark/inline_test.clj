(ns commonmark.inline-test
  (:require [clojure.test :refer :all]
            [commonmark.inline :refer :all]))

(deftest code-span-test
  (testing "minimal"
    (is (= (->> "`foo`" code-span :content)
           "foo")))

  (testing "backtick in contents"
    (is (= (->> "``foo`bar``" code-span :content)
           "foo`bar")))

  (testing "wrap backtick pairs"
    (is (= (->> "` `ls -l` `" code-span :content)
           "`ls -l`")))

  (testing "strip"
    (testing "single space"
      (are [s c] (= (->> s code-span :content)
                    c)
           "` foo `"   "foo"
           "`  foo  `" " foo "))

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

  (testing "backtick strings unequal length => no match"
    (are [s] (nil? (code-span s))
         "```foo``"
         "`foo"
         "`foo``bar``")))

(deftest emphasis-test
  (testing "opening with *"
    (testing "minimal"
      (is (= (-> "*foo bar*" emphasis :content)
             "foo bar")))

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
           "5*6*78"   "6")))

  (testing "opening with _"
    (testing "minimal"
      (is (= (-> "_foo bar_" emphasis :content)
             "foo bar")))

    (testing "opening _ followed by whitespace => not emphasis"
      (is (nil? (emphasis "_ foo bar_"))))

    (testing "opening _ preceded by alphanumeric and followed by punctuation => not emphasis"
      (is (nil? (emphasis "a_\"foo \"_"))))

    (testing "intraword"
      (are [s] (nil? (emphasis s))
           "foo_bar_"
           "5_6_78"))))

; TODO
(comment "
   Here _ does not generate emphasis, because the first delimiter run is right-flanking and the second
   left-flanking:

aa_ "bb"_cc
<p>aa_&quot;bb&quot;_cc</p>

   This is emphasis, even though the opening delimiter is both left- and right-flanking, because it is preceded by
   punctuation:

foo-_ (bar)_
<p>foo-<em> (bar)</em></p>
")
