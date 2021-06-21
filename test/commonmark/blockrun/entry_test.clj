(ns commonmark.blockrun.entry-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [commonmark.blockrun.entry :refer :all]))

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
             ["   -     abc" "" "     # foo" "     xyz" "     ```" "     opq" "     ```"])))))

