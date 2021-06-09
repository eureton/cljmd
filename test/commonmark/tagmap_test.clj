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
      (are [s ul] (= (parse s)
                     [[:stxh ["xyz" ul]]])
           "xyz\n===" "==="
           "xyz\n---" "---"))

    (testing "underline length"
      (are [s ul] (= (parse s)
                     [[:stxh ["xyz" ul]]])
           "xyz\n=" "="
           "xyz\n==" "=="
           "xyz\n===" "==="
           "xyz\n====" "===="
           "xyz\n=====" "====="
           "xyz\n======" "======"
           "xyz\n=======" "======="
           "xyz\n========" "========"
           "xyz\n-" "-"
           "xyz\n--" "--"
           "xyz\n---" "---"
           "xyz\n----" "----"
           "xyz\n-----" "-----"
           "xyz\n------" "------"
           "xyz\n-------" "-------"
           "xyz\n--------" "--------"))

    (testing "not preceded by blank"
      (is (= (second (parse "# xyz\r\nabc\r\n==="))
             [:stxh ["abc" "==="]])))

    (testing "multiline"
      (is (= (first (parse "xyz\nabc\n==="))
             [:stxh ["xyz" "abc" "==="]])))

    (testing "not followed by blank"
      (is (= (first (parse "abc\r\n===\r\n- xyz"))
             [:stxh ["abc" "==="]])))

    (testing "separation from paragraph with blank"
      (let [result (parse "abc\r\n\r\nxyz\r\n===")]
        (testing "tags"
          (is (= (map first result)
                 [:p :blank :stxh])))

        (testing "lines"
          (is (= (map second result)
                 [["abc"] [""] ["xyz" "==="]])))))

    (testing "multiple lines"
      (is (= (parse "xyz\r\nabc\r\n===")
             [[:stxh ["xyz" "abc" "==="]]]))))

  (testing "thematic break"
    (testing "minimal"
      (is (= (parse "---")
             [[:tbr ["---"]]])))

    (testing "followed by block"
      (are [s e] (= (parse s)
                    [[:tbr ["---"]] e])
           "---\nxyz"    [:p ["xyz"]]
           "---\n# xyz"  [:atxh ["# xyz"]]
           "---\nxyz\n-" [:stxh ["xyz" "-"]]
           "---\n- xyz"  [:li ["- xyz"]]
           "---\n"       [:blank [""]]))

    (testing "preceded by paragraph"
      (testing "using -"
        (is (= (parse "xyz\n---")
               [[:stxh ["xyz" "---"]]])))

      (testing "using _"
        (is (= (parse "xyz\n___")
               [[:p ["xyz"]] [:tbr ["___"]]])))))

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
      (is (= (parse "    abc\r\n    xyz")
             [[:icblk ["    abc" "    xyz"]]])))

    (testing "cannot interrupt paragraph"
      (are [s ls] (= (parse s)
                     [[:p ls]])
           "abc\r\n    xyz"        ["abc" "    xyz"]
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

      (testing "absorb indented code block"
        (is (= (parse (indent "- " "abc\n    foo\n    bar"))
               [[:li ["- abc" "      foo" "      bar"]]])))

      (testing "absorb fenced code block"
        (is (= (parse (indent "- " "abc\n```\nxyz\n```"))
               [[:li ["- abc" "  ```" "  xyz" "  ```"]]])))

      (testing "absorb thematic break"
        (is (= (parse (indent "- " "abc\n\n---"))
               [[:li ["- abc" "" "  ---"]]])))

      (testing "blanks"
        (are [s n] (= (parse s)
                      [[:li (concat ["- abc"] (repeat n "") ["  xyz"])]])
             "- abc\n\n  xyz"       1
             "- abc\n\n\n  xyz"     2
             "- abc\n\n\n\n  xyz"   3
             "- abc\n\n\n\n\n  xyz" 4))

      (testing "indentation"
        (are [s] (= (parse s)
                      [[:li (string/split-lines s)]])
             "- abc\n\n  # foo\n  xyz\n  ```\n  opq\n  ```\n      bar"
             " - abc\n\n   # foo\n   xyz\n   ```\n   opq\n   ```\n       bar"
             "  - abc\n\n    # foo\n    xyz\n    ```\n    opq\n    ```\n        bar"
             "   - abc\n\n     # foo\n     xyz\n     ```\n     opq\n     ```\n         bar")))

    (testing "starting with indented code"
      (testing "minimal"
        (is (= (parse "-     abc")
               [[:li ["-     abc"]]])))

      (testing "absorb paragraph"
        (is (= (parse (indent "- " "    abc\nxyz"))
               [[:li ["-     abc" "  xyz"]]])))

      (testing "absorb atx heading"
        (is (= (parse (indent "- " "    abc\n# xyz"))
               [[:li ["-     abc" "  # xyz"]]])))

      (testing "absorb fenced code block"
        (is (= (parse (indent "- " "    abc\n```\nxyz\n```"))
               [[:li ["-     abc" "  ```" "  xyz" "  ```"]]])))

      (testing "absorb blank"
        (is (= (parse (indent "- " "    abc\n\nxyz"))
               [[:li ["-     abc" "" "  xyz"]]])))

      (testing "absorb thematic break"
        (is (= (parse (indent "- " "    abc\n\n---"))
               [[:li ["-     abc" "" "  ---"]]])))

      (testing "blanks"
        (are [s n] (= (parse s)
                      [[:li (concat ["-     abc"] (repeat n "") ["  xyz"])]])
             "-     abc\n\n  xyz"       1
             "-     abc\n\n\n  xyz"     2
             "-     abc\n\n\n\n  xyz"   3
             "-     abc\n\n\n\n\n  xyz" 4))

      (testing "indentation"
        (are [s] (= (parse s)
                    [[:li (string/split-lines s)]])
             "-     abc\n\n  # foo\n  xyz\n  ```\n  opq\n  ```"
             " -     abc\n\n   # foo\n   xyz\n   ```\n   opq\n   ```"
             "  -     abc\n\n    # foo\n    xyz\n    ```\n    opq\n    ```"
             "   -     abc\n\n     # foo\n     xyz\n     ```\n     opq\n     ```")))

    (testing "starting with blank line"
      (testing "minimal"
        (is (= (parse "-")
               [[:li ["-"]]])))

      (testing "absorb paragraph"
        (is (= (parse "-\n  abc")
               [[:li ["-" "  abc"]]])))

      (testing "absorb atx heading"
        (is (= (parse (indent "- " "    abc\n# xyz"))
               [[:li ["-     abc" "  # xyz"]]])))

      (testing "absorb fenced code block"
        (is (= (parse (indent "- " "    abc\n```\nxyz\n```"))
               [[:li ["-     abc" "  ```" "  xyz" "  ```"]]])))

      (testing "absorb blank"
        (is (= (parse (indent "- " "    abc\n\nxyz"))
               [[:li ["-     abc" "" "  xyz"]]])))

      (testing "absorb thematic break"
        (is (= (parse (indent "- " "    abc\n\n---"))
               [[:li ["-     abc" "" "  ---"]]])))

      (testing "don't absorb unindented"
        (are [s] (= (first (parse s))
                    [:li ["-"]])
             "-\nabc"
             "-\n abc"))

      (testing "blanks"
        (are [s n] (= (parse s)
                      [[:li (concat ["-" "  abc"] (repeat n "") ["  xyz"])]])
             "-\n  abc\n\n  xyz"       1
             "-\n  abc\n\n\n  xyz"     2
             "-\n  abc\n\n\n\n  xyz"   3
             "-\n  abc\n\n\n\n\n  xyz" 4))

      (testing "indentation"
        (are [s] (= (parse s)
                      [[:li (string/split-lines s)]])
             "-\n  abc\n  # foo\n  ```\n  xyz\n  ```\n      bar"
             " -\n   abc\n   # foo\n   ```\n   xyz\n   ```\n       bar"
             "  -\n    abc\n    # foo\n    ```\n    xyz\n    ```\n        bar"
             "   -\n     abc\n     # foo\n     ```\n     xyz\n     ```\n         bar"))))

  (testing "block quote"
    (testing "basic case"
      (testing "minimal"
        (is (= (parse "> xyz")
               [[:bq ["> xyz"]]])))

      (testing "indentation"
        (are [s] (= (parse s)
                    [[:bq [s]]])
             " > xyz"
             "  > xyz"
             "   > xyz"))

      (testing "multiline"
        (is (= (parse "> xyz\n> # abc\n> foo")
               [[:bq ["> xyz" "> # abc" "> foo"]]])))

      (testing "empty"
        (are [s ls] (= (parse s)
                       [[:bq ls]])
             ">"           [">"]
             ">\n>"        [">" ">"]
             ">\n>\n>"     [">" ">" ">"]
             "> xyz\n>"    ["> xyz" ">"]
             ">\n> xyz\n>" [">" "> xyz" ">"]
             ">\n> xyz"    [">" "> xyz"]))

      (testing "blank line separates"
        (is (= (parse "> xyz\n\n> abc")
               [[:bq ["> xyz"]] [:blank [""]] [:bq ["> abc"]]])))

      (testing "can interrupt paragraphs"
        (is (= (parse "xyz\n> abc")
               [[:p ["xyz"]] [:bq ["> abc"]]])))

      (testing "no need for blank line before"
        (is (= (parse "***\n> abc")
               [[:tbr ["***"]] [:bq ["> abc"]]])))

      (testing "no need for blank line after"
        (is (= (parse "> abc\n***")
               [[:bq ["> abc"]] [:tbr ["***"]]]))))

    (testing "laziness"
      (testing "minimal"
        (is (= (parse "> xyz\nabc")
               [[:bq ["> xyz" "abc"]]])))

      (testing "marker omission"
        (testing "before paragraph continuation text"
          (are [s ls] (= (parse s)
                         [[:bq ls]])
               "> xyz\nabc\n> foo\nbar" ["> xyz" "abc" "> foo" "bar"]
               "> xyz\n    abc"         ["> xyz" "    abc"]))

        (testing "before non-paragraph-continuation text"
          (are [s e] (= e (second (parse s)))
               "> xyz\n---"         [:tbr ["---"]]
               "> xyz\n- abc"       [:li ["- abc"]]
               ">     xyz\n    abc" [:icblk ["    abc"]]
               "> ```\nxyz\n```"    [:p ["xyz"]]
               "> # xyz\n    abc"   [:icblk ["    abc"]])))

      (testing "whitespace before paragraph continuation text"
        (are [s] (= (parse (str "> xyz\n" s))
                    [[:bq ["> xyz" s]]])
             " abc"
             "  abc"
             "   abc"))

      (testing "separation from following paragraph"
        (are [s r] (= r (parse s))
             "> abc\n\nxyz"  [[:bq ["> abc"]] [:blank [""]] [:p ["xyz"]]]
             "> abc\n>\nxyz" [[:bq ["> abc" ">"]] [:p ["xyz"]]])))))

