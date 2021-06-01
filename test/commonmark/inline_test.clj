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
      (is (= (-> "*(*foo*)*" emphasis :content)
             "(*foo*)")))

    (testing "intraword"
      (is (= (-> "*foo*bar" emphasis :content)
             "foo"))))

  (testing "closing with _"
    (testing "preceded by whitespace => not emphasis"
      (is (nil? (emphasis "_foo bar _"))))

    (testing "preceded by punctuation, followed by alphanumeric => not emphasis"
      (is (nil? (emphasis "_(_foo)"))))

    (testing "preceded by punctuation, followed by whitespace"
      (is (= (-> "_(_foo_)_" emphasis :content)
             "(_foo_)")))

    (testing "intraword => not emphasis"
      (are [s] (nil? (emphasis s))
           "_foo_bar"
           "foo_bar_baz"))

    (testing "left-flank, right-flank, followed by punctuation => emphasis"
      (is (= (-> "_(bar)_." emphasis :content)
             "(bar)")))))

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
             "bar"))))

  (testing "opening with __"
    (testing "minimal"
      (is (= (-> "__foo bar__" strong-emphasis :content)
             "foo bar")))

    (testing "complex"
      (is (= (-> "__foo, __bar__, baz__" strong-emphasis :content)
             "foo, __bar__, baz")))

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
             "foo")))))

