(ns cljmd.blockrun.entry-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [cljmd.blockrun.entry :refer :all]))

(deftest content-test
  (testing "list item"
    (testing "basic case"
      (testing "paragraph"
        (testing "minimal"
          (is (= "abc" (content [:li ["- abc"]]))))

        (testing "multiple"
          (is (= (content [:li ["- abc" "  xyz"]])
               "abc\r\nxyz")))

        (testing "lazily continued"
          (is (= (content [:li ["- abc" "xyz"]])
               "abc\r\nxyz")))

        (testing "followed by indented code block"
          (is (= (content [:li ["- xyz" "      abc" "      def"]])
                 "xyz\r\n    abc\r\n    def")))

        (testing "followed by fenced code block"
          (is (= (content [:li ["- xyz" "  ```" "  abc" "  ```"]])
                 "xyz\r\n```\r\nabc\r\n```")))

        (testing "followed by thematic break"
          (is (= (content [:li ["- xyz" "" "  ---"]])
                 "xyz\r\n\r\n---")))

        (testing "followed by blanks"
          (are [ls c] (= c (content [:li ls]))
               ["- abc" "" "  xyz"]          "abc\r\n\r\nxyz"
               ["- abc" "" "" "  xyz"]       "abc\r\n\r\n\r\nxyz"
               ["- abc" "" "" "" "  xyz"]    "abc\r\n\r\n\r\n\r\nxyz"
               ["- abc" "" "" "" "" "  xyz"] "abc\r\n\r\n\r\n\r\n\r\nxyz")))

      (testing "atx heading"
        (testing "minimal"
          (is (= "# xyz" (content [:li ["- # xyz"]]))))

        (testing "multiple"
          (is (= (content [:li ["- # xyz" "  # abc"]])
                 "# xyz\r\n# abc"))))

      (testing "indentation"
        (are [ls] (= (content [:li ls])
                     "abc\r\n\r\n# foo\r\nxyz\r\n```\r\nopq\r\n```\r\n    bar")
             [   "- abc" ""    "  # foo"    "  xyz"    "  ```"    "  opq"    "  ```"    "      bar"]
             [  " - abc" ""   "   # foo"   "   xyz"   "   ```"   "   opq"   "   ```"   "       bar"]
             [ "  - abc" ""  "    # foo"  "    xyz"  "    ```"  "    opq"  "    ```"  "        bar"]
             ["   - abc" "" "     # foo" "     xyz" "     ```" "     opq" "     ```" "         bar"])))

    (testing "starting with indented code"
      (testing "minimal"
        (is (= (content [:li ["-     abc"]])
               "    abc")))

      (testing "followed by paragraph"
        (is (= (content [:li ["-     abc" "  xyz"]])
               "    abc\r\nxyz")))

      (testing "followed by atx heading"
        (is (= (content [:li ["-     abc" "# xyz"]])
               "    abc\r\n# xyz")))

      (testing "followed by fenced code block"
        (is (= (content [:li ["-     abc" "  ```" "  xyz" "  ```"]])
               "    abc\r\n```\r\nxyz\r\n```")))

      (testing "followed by blank"
        (is (= (content [:li ["-     abc" "" "  xyz"]])
               "    abc\r\n\r\nxyz")))

      (testing "followed by thematic break"
        (is (= (content [:li ["-     abc" "" "  ---"]])
               "    abc\r\n\r\n---")))

      (testing "blanks"
        (are [ls c] (= c (content [:li ls]))
             ["-     abc" ""          "  xyz"] "    abc\r\n\r\nxyz"
             ["-     abc" "" ""       "  xyz"] "    abc\r\n\r\n\r\nxyz"
             ["-     abc" "" "" ""    "  xyz"] "    abc\r\n\r\n\r\n\r\nxyz"
             ["-     abc" "" "" "" "" "  xyz"] "    abc\r\n\r\n\r\n\r\n\r\nxyz"))

      (testing "indentation"
        (are [ls] (= (content [:li ls])
                    "    abc\r\n\r\n# foo\r\nxyz\r\n```\r\nopq\r\n```")
             [   "-     abc" ""    "  # foo"    "  xyz"    "  ```"    "  opq"    "  ```"]
             [  " -     abc" ""   "   # foo"   "   xyz"   "   ```"   "   opq"   "   ```"]
             [ "  -     abc" ""  "    # foo"  "    xyz"  "    ```"  "    opq"  "    ```"]
             ["   -     abc" "" "     # foo" "     xyz" "     ```" "     opq" "     ```"])))

    (testing "starting with blank line"
      (testing "minimal"
        (is (= "" (content [:li ["-"]]))))

      (testing "followed by paragraph"
        (is (= (content [:li ["-" "  xyz"]])
               "\r\nxyz")))

      (testing "followed by atx heading"
        (is (= (content [:li ["-" "# xyz"]])
               "\r\n# xyz")))

      (testing "followed by fenced code block"
        (is (= (content [:li ["-" "```" "xyz" "```"]])
               "\r\n```\r\nxyz\r\n```")))

      (testing "followed by blank"
        (is (= (content [:li ["-" ""]])
               "\r\n")))

      (testing "followed by thematic break"
        (is (= (content [:li ["-" "  ---"]])
               "\r\n---")))

      (testing "blanks"
        (are [ls c] (= c (content [:li ls]))
             ["-" "  xyz" ""          "  abc"] "\r\nxyz\r\n\r\nabc"
             ["-" "  xyz" "" ""       "  abc"] "\r\nxyz\r\n\r\n\r\nabc"
             ["-" "  xyz" "" "" ""    "  abc"] "\r\nxyz\r\n\r\n\r\n\r\nabc"
             ["-" "  xyz" "" "" "" "" "  abc"] "\r\nxyz\r\n\r\n\r\n\r\n\r\nabc"))

      (testing "indentation"
        (are [ls] (= (content [:li ls])
                     "\r\nabc\r\n# foo\r\n```\r\nxyz\r\n```\r\n    bar")
             [   "-"    "  abc"    "  # foo"    "  ```"    "  xyz"    "  ```"   "      bar"]
             [  " -"   "   abc"   "   # foo"   "   ```"   "   xyz"   "   ```"  "       bar"]
             [ "  -"  "    abc"  "    # foo"  "    ```"  "    xyz"  "    ```" "        bar"]
             ["   -" "     abc" "     # foo" "     ```" "     xyz" "     ```""         bar"]))))

  (testing "block quote"
    (testing "basic case"
      (testing "minimal"
        (is (= "xyz" (content [:bq ["> xyz"]]))))

      (testing "indentation"
        (are [s] (= "xyz" (content [:bq [s]]))
             " > xyz"
             "  > xyz"
             "   > xyz"))

      (testing "multiline"
        (is (= (content [:bq ["> xyz" "> # abc" "> def"]])
               "xyz\r\n# abc\r\ndef")))

      (testing "empty"
        (are [ls c] (= c (content [:bq ls]))
             [">"]             ""
             [">" ">"]         "\r\n"
             [">" ">" ">"]     "\r\n\r\n"
             ["> xyz" ">"]     "xyz\r\n"
             [">" "> xyz" ">"] "\r\nxyz\r\n"
             [">" "> xyz"]     "\r\nxyz")))

    (testing "laziness"
      (testing "minimal"
        (is (= (content [:bq ["> xyz" "abc"]])
               "xyz\r\nabc")))

      (testing "marker omission"
        (testing "before paragraph continuation text"
          (are [ls c] (= c (content [:bq ls]))
               ["> xyz" "abc" "> def" "pqr"] "xyz\r\nabc\r\ndef\r\npqr"
               ["> xyz" "    abc"]           "xyz\r\n    abc")))

      (testing "whitespace before paragraph continuation text"
        (are [l] (= (content [:bq ["> xyz" l]])
                    (str "xyz\r\n" l))
             " abc"
             "  abc"
             "   abc"))

      (testing "multiple levels of nesting"
        (is (= (content [:bq ["> > > abc" "xyz"]])
               "> > abc\r\nxyz"))))

    (testing "paragraph continuation text"
      (testing "not lazy"
        (are [ls c] (= c (content [:bq ls]))
             ["> xyz" "abc"]                         "xyz\r\nabc"
             ["> xyz" "    abc"]                     "xyz\r\n    abc"
             ["> xyz" ">     abc" "pqr"]             "xyz\r\n    abc\r\npqr"
             ["> xyz" ">     abc" ">     pqr" "123"] "xyz\r\n    abc\r\n    pqr\r\n123"))

      (testing "lazy"
        (are [ls c] (= c (content [:bq ls]))
             ["> xyz" "abc"]                     "xyz\r\nabc"
             ["> xyz" "    abc"]                 "xyz\r\n    abc"
             ["> xyz" "    abc" "pqr"]           "xyz\r\n    abc\r\npqr"
             ["> xyz" "    abc" "    pqr" "123"] "xyz\r\n    abc\r\n    pqr\r\n123")))))

(deftest link-reference-definition-batch-test
  (testing "empty"
    (testing "string"
      (is (= (link-reference-definition-batch [:_ [""]])
             [])))

    (testing "collection"
      (is (= (link-reference-definition-batch [:_ []])
             []))))

  (testing "one"
    (is (= (link-reference-definition-batch [:_ ["[abc]: xyz '123'"]])
           ["[abc]: xyz '123'"])))

  (testing "multiple"
    (is (= (link-reference-definition-batch [:_ ["[abc]: xyz '123'"
                                                 "[cba]: zyx '321'"]])
           ["[abc]: xyz '123'"
            "[cba]: zyx '321'"])))

  (testing "multiline"
    (testing "label and destination"
      (is (= (link-reference-definition-batch [:_ ["[abc]: xyz" "'123'"]])
           ["[abc]: xyz" "'123'"])))

    (testing "destination and title"
      (is (= (link-reference-definition-batch [:_ ["[abc]:" "xyz '123'"]])
           ["[abc]:" "xyz '123'"])))

    (testing "three lines"
      (is (= (link-reference-definition-batch [:_ ["[abc]:" "xyz" "'123'"]])
             ["[abc]:" "xyz" "'123'"])))

    (testing "multiline title"
      (is (= (link-reference-definition-batch [:_ ["[abc]:" "xyz" "'12" "34" "56'"]])
             ["[abc]:" "xyz" "'12" "34" "56'"]))))

  (testing "preceded by non-definition"
    (is (= (link-reference-definition-batch [:_ ["qpr" "[abc]: xyz '123'"]])
           [])))

  (testing "followed by non-definition"
    (is (= (link-reference-definition-batch [:_ ["[abc]: xyz '123'" "qpr"]])
           ["[abc]: xyz '123'"]))))

