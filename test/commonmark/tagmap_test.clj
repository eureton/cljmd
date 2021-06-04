(ns commonmark.tagmap-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [commonmark.tagmap :refer :all]))

(deftest add-test
  (testing "monoid structure"
    (let [tm [[:p ["xyz"]]]]
      (testing "zero => identity of itself"
        (is (= zero (add zero zero))))

      (testing "zero => identity left"
        (is (= tm (add zero tm))))

      (testing "zero => identity right"
        (is (= tm (add tm zero))))))

  (testing "as reducer"
    (testing "empty collection"
      (is (= zero (reduce add []))))

    (testing "singleton collection"
      (is (= :x (reduce add [:x]))))

    (testing "init value, empty collection"
      (is (= :x (reduce add :x []))))

    (testing "init value, singleton collection"
      (is (= (reduce add (from-line "abc") [(from-line "xyz")])
             [[:p ["abc" "xyz"]]])))

    (testing "ordinary collection"
      (is (= (reduce add (map from-line ["abc" "xyz"]))
             [[:p ["abc" "xyz"]]]))))

  (testing "unravel second"
    (let [ls1 ["``` c++"
               "class Foo {"
               "};"]
          ls2 ["```"
               "one"
               "two"]
          result (add (->> ls1 (map from-line) (reduce add))
                      (->> ls2 (map from-line) (reduce add)))]
      (testing "count"
        (is (= (count result)
               2)))

      (testing "tags"
        (is (= (map first result)
               [:ofcblk :p])))

      (testing "lines"
        (is (= (map second result)
               [["``` c++" "class Foo {" "};" "```"]
                ["one" "two"]])))))

  (testing "retag first"
    (let [ls1 ["xyz" "abc"]
          ls2 ["123" "==="]
          result (add (->> ls1 (map from-line) (reduce add))
                      (->> ls2 (map from-line) (reduce add)))]
      (testing "count"
        (is (= (count result)
               1)))

      (testing "tags"
        (is (= (map first result)
               [:stxh])))

      (testing "lines"
        (is (= (map second result)
               [["xyz" "abc" "123" "==="]]))))))

(deftest parse-test
  (testing "setext heading"
    (testing "minimal"
      (is (= (parse "xyz\r\n===")
             [[:stxh ["xyz" "==="]]])))

    (testing "not preceded by blank"
      (is (= (second (parse "# xyz\r\nabc\r\n==="))
             [:stxh ["abc" "==="]])))

    (testing "not followed by blank"
      (is (= (first (parse "abc\r\n===\r\n- xyz"))
             [:stxh ["abc" "==="]])))

    (testing "separation from paragraph with blank"
      (let [result (parse "abc\r\n\r\nxyz\r\n===\r\n")]
        (testing "tags"
          (is (= (map first result)
                 [:p :blank :stxh])))

        (testing "lines"
          (is (= (map second result)
                 [["abc"] [""] ["xyz" "==="]])))))

    (testing "multiple lines"
      (is (= (parse "xyz\r\nabc\r\n===")
             [[:stxh ["xyz" "abc" "==="]]]))))

  (testing "fenced code block"
    (testing "minimal"
      (is (= (parse "```\r\nxyz\r\n```")
             [[:ofcblk ["```" "xyz" "```"]]])))

    (testing "may interrupt paragraph"
      (let [result (parse "abc\n```\n123\n```\nxyz")]
        (testing "tags"
          (is (= (map first result)
                 [:p :ofcblk :p])))

        (testing "lines"
          (is (= (map second result)
                 [["abc"] ["```" "123" "```"] ["xyz"]])))))

    (testing "no closing fence => contains all till end of input"
      (let [result (parse "```\nabc\nxyz")]
        (testing "tags"
          (is (= (map first result)
                 [:ofcblk])))

        (testing "lines"
          (is (= (map second result)
                 [["```" "abc" "xyz"]])))))

    (testing "associativity"
      (let [s1 "```\nabc"
            s2 "```\n123\n```"
            s3 "xyz\n```"
            [tm1 tm2 tm3] (map parse [s1 s2 s3])
            result (add tm1 (add tm2 tm3))]
        (testing "tags"
          (is (= (map first result)
                 [:ofcblk :p :ofcblk])))

        (testing "lines"
          (is (= (map second result)
                 [["```" "abc" "```"] ["123"] ["```" "xyz" "```"]]))))))

  (testing "indented code block"
    (testing "minimal"
      (is (= (parse "    abc")
             [[:icblk ["    abc"]]])))

    (testing "multiple lines"
      (is (= (parse "    abc\r\n    xyz\r\n")
             [[:icblk ["    abc" "    xyz"]]])))

    (testing "cannot interrupt paragraph"
      (are [s ls] (= (parse s)
                     [[:p ls]])
           "abc\r\n    xyz\r\n"    ["abc" "    xyz"]
           "abc\r\n    xyz\r\n123" ["abc" "    xyz" "123"]))

    (testing "paragraph may follow without blank"
      (let [result (parse "    xyz\r\nabc")]
        (testing "tags"
          (is (= (map first result)
                 [:icblk :p])))

        (testing "lines"
          (is (= (map second result)
                 [["    xyz"] ["abc"]])))))

    (testing "chunks"
      (is (= (parse "    xyz\r\n    abc")
             [[:icblk ["    xyz" "    abc"]]])))

    (testing "chunks separated by blanks"
      (are [s ls] (= (parse s)
                     [[:icblk ls]])
           "    1\n\n    2"          ["    1" "" "    2"]
           "    1\n\n\n    2"        ["    1" "" "" "    2"]
           "    1\n\n    2\n\n    3" ["    1" "" "    2" "" "    3"])))

  (testing "list item"
    (defn indent
      [m s]
      (let [with-marker #(str m %)
            ws (-> m count (repeat \space) string/join)
            with-ws #(cond->> %
                       (not (string/blank? %)) (str ws))]
        (->> s
             string/split-lines
             ((juxt (comp vector with-marker first)
                    (comp #(map with-ws %) rest)))
             (apply concat)
             (string/join "\r\n"))))

    (testing "basic case"
      (testing "absorb paragraph"
        (is (= (parse (indent "- " "abc\nxyz"))
               [[:li ["- abc" "  xyz"]]])))

      (testing "absorb atx heading"
        (is (= (parse (indent "- " "abc\n# xyz"))
               [[:li ["- abc" "  # xyz"]]])))

      (testing "absorb thematic break"
        (is (= (parse (indent "- " "abc\n---"))
               [[:li ["- abc" "  ---"]]])))

      (testing "absorb blank"
        (is (= (parse "- abc\n\n  xyz")
               [[:li ["- abc" "" "  xyz"]]]))))))

