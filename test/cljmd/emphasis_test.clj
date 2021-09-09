(ns cljmd.emphasis-test
  (:require [clojure.test :refer :all]
            [cljmd.emphasis :refer :all]))

(deftest from-string-test
  (defn matches [s]
    (->> s from-string (map :re/match)))

  (defn no-match? [s]
    (->> s from-string empty?))

  (testing "delimiter-agnostic"
    (testing "minimal"
      (are [s] (= [s] (matches s))
           "*abc xyz*"
           "_abc xyz_"))

    (testing "multiple"
      (are [s ms] (= ms (matches s))
               "*xyz* *abc*"         ["*xyz*" "*abc*"]
               "*xyz* 123 *abc*"     ["*xyz*" "*abc*"]
           "def *xyz* 123 *abc*"     ["*xyz*" "*abc*"]
               "*xyz* 123 *abc* qpr" ["*xyz*" "*abc*"]
           "def *xyz* 123 *abc* qpr" ["*xyz*" "*abc*"]
               "_xyz_ _abc_"         ["_xyz_" "_abc_"]
               "_xyz_ 123 _abc_"     ["_xyz_" "_abc_"]
           "def _xyz_ 123 _abc_"     ["_xyz_" "_abc_"]
               "_xyz_ 123 _abc_ qpr" ["_xyz_" "_abc_"]
           "def _xyz_ 123 _abc_ qpr" ["_xyz_" "_abc_"]))

    (testing "lfdr followed by whitespace"
      (are [s] (no-match? s)
           "* xyz abc*"
           "_ xyz abc_"))

    (testing "lfdr preceded by alphanumeric and followed by punctuation"
      (are [s] (no-match? s)
           "abc*\"xyz\"*"
           "abc_\"xyz\"_"))

    (testing "unicode nonbreaking spaces"
      (are [c] (and (no-match? (str "*" c "abc *"))
                    (no-match? (str "_" c "abc _")))
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

    (testing "left-flank, right-flank"
      (are [s] (no-match? s)
           "abc*\"123\"*xyz"
           "abc_\"123\"_xyz"))

    (testing "lfdr preceded by punctuation, rfdr preceded by punctuation"
      (are [s ms] (= ms (matches s))
           "abc-*(xyz)*" ["*(xyz)*"]
           "abc-_(xyz)_" ["_(xyz)_"]))

    (testing "lfdr preceded by punctuation and followed by punctuation"
      (are [s ms] (= ms (matches s))
           ".*.xyz*" ["*.xyz*"]
           "._.xyz_" ["_.xyz_"]))

    (testing "lfdr sharing"
      (are [s ms] (= ms (matches s))
           "****abc* x* y* z*" ["****abc* x* y* z*"]
           "____abc_ x_ y_ z_" ["____abc_ x_ y_ z_"]))

    (testing "rfdr preceded by whitespace"
      (are [s] (no-match? s)
           "*abc *"
           "*abc\n*"
           "_abc _"
           "_abc\n_"))

    (testing "rfdr preceded by punctuation, followed by alphanumeric"
      (are [s] (no-match? s)
           "*(*abc)"
           "_(_abc)"))

    (testing "rfdr preceded by punctuation, followed by whitespace"
      (are [s ms] (= ms (matches s))
           "*(xyz)*" ["*(xyz)*"]
           "_(xyz)_" ["_(xyz)_"]))

    (testing "rfdr is also lfdr followed by punctuation"
      (are [s ms] (= ms (matches s))
           "*(xyz)*." ["*(xyz)*"]
           "_(xyz)_." ["_(xyz)_"]))

    (testing "rfdr sharing"
      (are [s ms] (= ms (matches s))
           "*a *b *c *xyz****" ["*a *b *c *xyz****"]
           "_a _b _c _xyz____" ["_a _b _c _xyz____"]))

    (testing "nesting"
      (are [s ms] (= ms (matches s))
           "*(*xyz*)*" ["*(*xyz*)*"]
           "_(_xyz_)_" ["_(_xyz_)_"]))

    (testing "dangle"
      (are [s ms] (= ms (matches s))
           "**abc*" ["*abc*"]
           "*abc**" ["*abc*"]
           "__abc_" ["_abc_"]
           "_abc__" ["_abc_"]))

    (testing "multiples of 3"
      (testing "sum is, parts aren't"
        (are [s] (= [s] (matches s))
             "*abc**xyz*"
             "*abc**xyz**123*"
             "_abc__xyz_"
             "_abc__xyz__123_"))

      (testing "sum is, parts are"
        (are [s ms] (= ms (matches s))
             "***abc***123***xyz***" ["***abc***" "***xyz***"]
             "___abc___123___xyz___" ["___abc___123___xyz___"])))

    (testing "delimiter mismatch"
      (are [s] (no-match? s)
           "_abc*"
           "__abc**"
           "*abc_"
           "**abc__")))

  (testing "*-delimited"
    (testing "intraword"
      (are [s m] (= m (matches s))
           "abc*xyz*"    ["*xyz*"]
           "*abc*xyz"    ["*abc*"]
           "abc*123*xyz" ["*123*"])))

  (testing "_-delimited"
    (testing "intraword"
      (are [s] (no-match? s)
           "abc_xyz_"
           "_abc_xyz"
           "abc_123_xyz"))))

