(ns commonmark.blockrun-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [commonmark.blockrun :refer :all]))

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

(deftest from-string-test
  (testing "setext heading"
    (testing "minimal"
      (are [s ul] (= (from-string s)
                     [[:stxh ["xyz" ul]]])
           "xyz\n===" "==="
           "xyz\n---" "---"))

    (testing "underline length"
      (are [s ul] (= (from-string s)
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
      (is (= (second (from-string "# xyz\r\nabc\r\n==="))
             [:stxh ["abc" "==="]])))

    (testing "multiline"
      (is (= (first (from-string "xyz\nabc\n==="))
             [:stxh ["xyz" "abc" "==="]])))

    (testing "not followed by blank"
      (is (= (first (from-string "abc\r\n===\r\n- xyz"))
             [:stxh ["abc" "==="]])))

    (testing "separation from paragraph with blank"
      (let [result (from-string "abc\r\n\r\nxyz\r\n===")]
        (testing "tags"
          (is (= (map first result)
                 [:p :blank :stxh])))

        (testing "lines"
          (is (= (map second result)
                 [["abc"] [""] ["xyz" "==="]])))))

    (testing "multiple lines"
      (is (= (from-string "xyz\r\nabc\r\n===")
             [[:stxh ["xyz" "abc" "==="]]]))))

  (testing "thematic break"
    (testing "minimal"
      (is (= (from-string "---")
             [[:tbr ["---"]]])))

    (testing "followed by block"
      (are [s e] (= (from-string s)
                    [[:tbr ["---"]] e])
           "---\nxyz"    [:p ["xyz"]]
           "---\n# xyz"  [:atxh ["# xyz"]]
           "---\nxyz\n-" [:stxh ["xyz" "-"]]
           "---\n- xyz"  [:li ["- xyz"]]
           "---\n"       [:blank [""]]))

    (testing "preceded by paragraph"
      (testing "using -"
        (is (= (from-string "xyz\n---")
               [[:stxh ["xyz" "---"]]])))

      (testing "using _"
        (is (= (from-string "xyz\n___")
               [[:p ["xyz"]] [:tbr ["___"]]])))))

  (testing "fenced code block"
    (testing "minimal"
      (is (= (from-string "```\r\nxyz\r\n```")
             [[:ofcblk ["```" "xyz" "```"]]])))

    (testing "may interrupt paragraph"
      (let [result (from-string "abc\n```\n123\n```\nxyz")]
        (testing "tags"
          (is (= (map first result)
                 [:p :ofcblk :p])))

        (testing "lines"
          (is (= (map second result)
                 [["abc"] ["```" "123" "```"] ["xyz"]])))))

    (testing "no closing fence => contains all till end of input"
      (let [result (from-string "```\nabc\nxyz")]
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
            [tm1 tm2 tm3] (map from-string [s1 s2 s3])
            result (add tm1 (add tm2 tm3))]
        (testing "tags"
          (is (= (map first result)
                 [:ofcblk :p :ofcblk])))

        (testing "lines"
          (is (= (map second result)
                 [["```" "abc" "```"] ["123"] ["```" "xyz" "```"]]))))))

  (testing "indented code block"
    (testing "minimal"
      (is (= (from-string "    abc")
             [[:icblk ["    abc"]]])))

    (testing "multiple lines"
      (is (= (from-string "    abc\r\n    xyz")
             [[:icblk ["    abc" "    xyz"]]])))

    (testing "cannot interrupt paragraph"
      (are [s ls] (= (from-string s)
                     [[:p ls]])
           "abc\r\n    xyz"        ["abc" "    xyz"]
           "abc\r\n    xyz\r\n123" ["abc" "    xyz" "123"]))

    (testing "paragraph may follow without blank"
      (let [result (from-string "    xyz\r\nabc")]
        (testing "tags"
          (is (= (map first result)
                 [:icblk :p])))

        (testing "lines"
          (is (= (map second result)
                 [["    xyz"] ["abc"]])))))

    (testing "chunks"
      (is (= (from-string "    xyz\r\n    abc")
             [[:icblk ["    xyz" "    abc"]]])))

    (testing "chunks separated by blanks"
      (are [s ls] (= (from-string s)
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
        (is (= (from-string (indent "- " "abc\nxyz"))
               [[:li ["- abc" "  xyz"]]])))

      (testing "absorb atx heading"
        (is (= (from-string (indent "- " "abc\n# xyz"))
               [[:li ["- abc" "  # xyz"]]])))

      (testing "absorb indented code block"
        (is (= (from-string (indent "- " "abc\n    foo\n    bar"))
               [[:li ["- abc" "      foo" "      bar"]]])))

      (testing "absorb fenced code block"
        (is (= (from-string (indent "- " "abc\n```\nxyz\n```"))
               [[:li ["- abc" "  ```" "  xyz" "  ```"]]])))

      (testing "absorb thematic break"
        (is (= (from-string (indent "- " "abc\n\n---"))
               [[:li ["- abc" "" "  ---"]]])))

      (testing "blanks"
        (are [s n] (= (from-string s)
                      [[:li (concat ["- abc"] (repeat n "") ["  xyz"])]])
             "- abc\n\n  xyz"       1
             "- abc\n\n\n  xyz"     2
             "- abc\n\n\n\n  xyz"   3
             "- abc\n\n\n\n\n  xyz" 4))

      (testing "indentation"
        (are [s] (= (from-string s)
                      [[:li (string/split-lines s)]])
             "- abc\n\n  # foo\n  xyz\n  ```\n  opq\n  ```\n      bar"
             " - abc\n\n   # foo\n   xyz\n   ```\n   opq\n   ```\n       bar"
             "  - abc\n\n    # foo\n    xyz\n    ```\n    opq\n    ```\n        bar"
             "   - abc\n\n     # foo\n     xyz\n     ```\n     opq\n     ```\n         bar")))

    (testing "starting with indented code"
      (testing "minimal"
        (is (= (from-string "-     abc")
               [[:li ["-     abc"]]])))

      (testing "absorb paragraph"
        (is (= (from-string (indent "- " "    abc\nxyz"))
               [[:li ["-     abc" "  xyz"]]])))

      (testing "absorb atx heading"
        (is (= (from-string (indent "- " "    abc\n# xyz"))
               [[:li ["-     abc" "  # xyz"]]])))

      (testing "absorb fenced code block"
        (is (= (from-string (indent "- " "    abc\n```\nxyz\n```"))
               [[:li ["-     abc" "  ```" "  xyz" "  ```"]]])))

      (testing "absorb blank"
        (is (= (from-string (indent "- " "    abc\n\nxyz"))
               [[:li ["-     abc" "" "  xyz"]]])))

      (testing "absorb thematic break"
        (is (= (from-string (indent "- " "    abc\n\n---"))
               [[:li ["-     abc" "" "  ---"]]])))

      (testing "blanks"
        (are [s n] (= (from-string s)
                      [[:li (concat ["-     abc"] (repeat n "") ["  xyz"])]])
             "-     abc\n\n  xyz"       1
             "-     abc\n\n\n  xyz"     2
             "-     abc\n\n\n\n  xyz"   3
             "-     abc\n\n\n\n\n  xyz" 4))

      (testing "indentation"
        (are [s] (= (from-string s)
                    [[:li (string/split-lines s)]])
             "-     abc\n\n  # foo\n  xyz\n  ```\n  opq\n  ```"
             " -     abc\n\n   # foo\n   xyz\n   ```\n   opq\n   ```"
             "  -     abc\n\n    # foo\n    xyz\n    ```\n    opq\n    ```"
             "   -     abc\n\n     # foo\n     xyz\n     ```\n     opq\n     ```")))

    (testing "starting with blank line"
      (testing "minimal"
        (is (= (from-string "-")
               [[:li ["-"]]])))

      (testing "absorb paragraph"
        (is (= (from-string "-\n  abc")
               [[:li ["-" "  abc"]]])))

      (testing "absorb atx heading"
        (is (= (from-string (indent "- " "    abc\n# xyz"))
               [[:li ["-     abc" "  # xyz"]]])))

      (testing "absorb fenced code block"
        (is (= (from-string (indent "- " "    abc\n```\nxyz\n```"))
               [[:li ["-     abc" "  ```" "  xyz" "  ```"]]])))

      (testing "absorb blank"
        (is (= (from-string (indent "- " "    abc\n\nxyz"))
               [[:li ["-     abc" "" "  xyz"]]])))

      (testing "absorb thematic break"
        (is (= (from-string (indent "- " "    abc\n\n---"))
               [[:li ["-     abc" "" "  ---"]]])))

      (testing "don't absorb unindented"
        (are [s] (= (first (from-string s))
                    [:li ["-"]])
             "-\nabc"
             "-\n abc"))

      (testing "blanks"
        (are [s n] (= (from-string s)
                      [[:li (concat ["-" "  abc"] (repeat n "") ["  xyz"])]])
             "-\n  abc\n\n  xyz"       1
             "-\n  abc\n\n\n  xyz"     2
             "-\n  abc\n\n\n\n  xyz"   3
             "-\n  abc\n\n\n\n\n  xyz" 4))

      (testing "indentation"
        (are [s] (= (from-string s)
                      [[:li (string/split-lines s)]])
             "-\n  abc\n  # foo\n  ```\n  xyz\n  ```\n      bar"
             " -\n   abc\n   # foo\n   ```\n   xyz\n   ```\n       bar"
             "  -\n    abc\n    # foo\n    ```\n    xyz\n    ```\n        bar"
             "   -\n     abc\n     # foo\n     ```\n     xyz\n     ```\n         bar"))))

  (testing "block quote"
    (testing "basic case"
      (testing "minimal"
        (is (= (from-string "> xyz")
               [[:bq ["> xyz"]]])))

      (testing "indentation"
        (are [s] (= (from-string s)
                    [[:bq [s]]])
             " > xyz"
             "  > xyz"
             "   > xyz"))

      (testing "multiline"
        (is (= (from-string "> xyz\n> # abc\n> foo")
               [[:bq ["> xyz" "> # abc" "> foo"]]])))

      (testing "empty"
        (are [s ls] (= (from-string s)
                       [[:bq ls]])
             ">"           [">"]
             ">\n>"        [">" ">"]
             ">\n>\n>"     [">" ">" ">"]
             "> xyz\n>"    ["> xyz" ">"]
             ">\n> xyz\n>" [">" "> xyz" ">"]
             ">\n> xyz"    [">" "> xyz"]))

      (testing "blank line separates"
        (is (= (from-string "> xyz\n\n> abc")
               [[:bq ["> xyz"]] [:blank [""]] [:bq ["> abc"]]])))

      (testing "can interrupt paragraphs"
        (is (= (from-string "xyz\n> abc")
               [[:p ["xyz"]] [:bq ["> abc"]]])))

      (testing "no need for blank line before"
        (is (= (from-string "***\n> abc")
               [[:tbr ["***"]] [:bq ["> abc"]]])))

      (testing "no need for blank line after"
        (is (= (from-string "> abc\n***")
               [[:bq ["> abc"]] [:tbr ["***"]]]))))

    (testing "laziness"
      (testing "minimal"
        (is (= (from-string "> xyz\nabc")
               [[:bq ["> xyz" "abc"]]])))

      (testing "marker omission"
        (testing "before paragraph continuation text"
          (are [s ls] (= (from-string s)
                         [[:bq ls]])
               "> xyz\nabc\n> foo\nbar" ["> xyz" "abc" "> foo" "bar"]
               "> xyz\n    abc"         ["> xyz" "    abc"]))

        (testing "before non-paragraph-continuation text"
          (are [s e] (= e (second (from-string s)))
               "> xyz\n---"         [:tbr ["---"]]
               "> xyz\n- abc"       [:li ["- abc"]]
               ">     xyz\n    abc" [:icblk ["    abc"]]
               "> ```\nxyz\n```"    [:p ["xyz"]]
               "> # xyz\n    abc"   [:icblk ["    abc"]])))

      (testing "whitespace before paragraph continuation text"
        (are [s] (= (from-string (str "> xyz\n" s))
                    [[:bq ["> xyz" s]]])
             " abc"
             "  abc"
             "   abc"))

      (testing "separation from following paragraph"
        (are [s r] (= r (from-string s))
             "> abc\n\nxyz"  [[:bq ["> abc"]] [:blank [""]] [:p ["xyz"]]]
             "> abc\n>\nxyz" [[:bq ["> abc" ">"]] [:p ["xyz"]]]))

      (testing "multiple levels of nesting"
        (is (= (from-string "> > > abc\nxyz")
               [[:bq ["> > > abc" "xyz"]]]))))

    (testing "paragraph continuation text"
      (testing "not lazy"
        (are [s ls] (= (from-string s)
                       [[:bq ls]])
             "> xyz\nabc"                       ["> xyz" "abc"]
             "> xyz\n    abc"                   ["> xyz" "    abc"]
             "> xyz\n>     abc\npqr"            ["> xyz" ">     abc" "pqr"]
             "> xyz\n>     abc\n>     pqr\n123" ["> xyz" ">     abc" ">     pqr" "123"]))

      (testing "lazy"
        (are [s ls] (= (from-string s)
                       [[:bq ls]])
             "> xyz\nabc"                   ["> xyz" "abc"]
             "> xyz\n    abc"               ["> xyz" "    abc"]
             "> xyz\n    abc\npqr"          ["> xyz" "    abc" "pqr"]
             "> xyz\n    abc\n    pqr\n123" ["> xyz" "    abc" "    pqr" "123"]))))

  (testing "html block"
    (testing "variant 1"
      (testing "single line"
        (let [s "<pre>abc</pre>"]
          (is (= (from-string s)
                 [[:html-block [s]]]))))

      (testing "multiline"
        (is (= (from-string "<pre>\nabc\nxyz\n</pre>")
               [[:html-block ["<pre>" "abc" "xyz" "</pre>"]]])))

      (testing "tag interchangability"
        (are [t1 t2] (= (from-string (str t1 "\nabc\n" t2))
                        [[:html-block [t1 "abc" t2]]])
             "<pre>"    "</script>"
             "<pre>"    "</style>"
             "<script>" "</pre>"
             "<script>" "</style>"
             "<style>"  "</pre>"
             "<style>"  "</script>"))

      (testing "empty"
        (let [s "<pre></pre>"]
          (= (from-string s)
             [[:html-block [s]]])))

      (testing "interrupt paragraph"
        (is (= (from-string "xyz\n<pre>\n123</pre>\nabc")
               [[:p          ["xyz"]]
                [:html-block ["<pre>" "123</pre>"]]
                [:p          ["abc"]]])))

      (testing "unpaired"
        (testing "opener"
          (is (= (postprocess (from-string "xyz\n<pre>\nabc"))
                 [[:p          ["xyz"]]
                  [:html-block ["<pre>" "abc"]]])))

        (testing "closer"
          (is (= (postprocess (from-string "xyz\n</pre>\nabc"))
                 [[:p ["xyz" "</pre>" "abc"]]])))))

    (testing "variant 2"
      (testing "single line"
        (let [s "<!--abc-->"]
          (is (= (from-string s)
                 [[:html-block [s]]]))))

      (testing "multiline"
        (is (= (from-string "<!--\nabc\nxyz\n-->")
               [[:html-block ["<!--" "abc" "xyz" "-->"]]])))

      (testing "empty"
        (let [s "<!---->"]
          (= (from-string s)
             [[:html-block [s]]])))

      (testing "interrupt paragraph"
        (is (= (from-string "xyz\n<!--\n123-->\nabc")
               [[:p          ["xyz"]]
                [:html-block ["<!--" "123-->"]]
                [:p          ["abc"]]])))

      (testing "unpaired"
        (testing "opener"
          (is (= (postprocess (from-string "xyz\n<!--\nabc"))
                 [[:p          ["xyz"]]
                  [:html-block ["<!--" "abc"]]])))

        (testing "closer"
          (is (= (postprocess (from-string "xyz\n-->\nabc"))
                 [[:p ["xyz" "-->" "abc"]]])))))

    (testing "variant 3"
      (testing "single line"
        (let [s "<?abc?>"]
          (is (= (from-string s)
                 [[:html-block [s]]]))))

      (testing "multiline"
        (is (= (from-string "<?\nabc\nxyz\n?>")
               [[:html-block ["<?" "abc" "xyz" "?>"]]])))

      (testing "empty"
        (let [s "<??>"]
          (= (from-string s)
             [[:html-block [s]]])))

      (testing "interrupt paragraph"
        (is (= (from-string "xyz\n<?\n123?>\nabc")
               [[:p          ["xyz"]]
                [:html-block ["<?" "123?>"]]
                [:p          ["abc"]]])))

      (testing "unpaired"
        (testing "opener"
          (is (= (postprocess (from-string "xyz\n<?\nabc"))
                 [[:p          ["xyz"]]
                  [:html-block ["<?" "abc"]]])))

        (testing "closer"
          (is (= (postprocess (from-string "xyz\n?>\nabc"))
                 [[:p ["xyz" "?>" "abc"]]])))))

    (testing "variant 4"
      (testing "single line"
        (let [s "<!Wabc>"]
          (is (= (from-string s)
                 [[:html-block [s]]]))))

      (testing "multiline"
        (is (= (from-string "<!W\nabc\nxyz\n>")
               [[:html-block ["<!W" "abc" "xyz" ">"]]])))

      (testing "empty"
        (let [s "<!W>"]
          (= (from-string s)
             [[:html-block [s]]])))

      (testing "interrupt paragraph"
        (is (= (from-string "xyz\n<!W\n123>\nabc")
               [[:p          ["xyz"]]
                [:html-block ["<!W" "123>"]]
                [:p          ["abc"]]])))

      (testing "unpaired"
        (testing "opener"
          (is (= (postprocess (from-string "xyz\n<!W\nabc"))
                 [[:p          ["xyz"]]
                  [:html-block ["<!W" "abc"]]])))

        (testing "closer"
          (is (= (from-string "xyz\n>\nabc")
                 [[:p  ["xyz"]]
                  [:bq [">"]]
                  [:p  ["abc"]]]))))

      (testing "blockquote as closer"
        (testing "variant 4 opener"
          (is (= (add (from-string "0\n<!W\n1")
                      (from-string "2\n> 3\n4"))
                 [[:p          ["0"]]
                  [:html-block ["<!W" "1" "2" "> 3"]]
                  [:p          ["4"]]])))

        (testing "non-variant 4 opener"
          (is (= (add (from-string "0\n<!--\n1")
                      (from-string "2\n> 3\n4"))
                 [[:p                   ["0"]]
                  [:html-block-unpaired ["<!--" "1" "2"]]
                  [:bq                  ["> 3" "4"]]])))))

    (testing "variant 5"
      (testing "single line"
        (let [s "<![CDATA[abc]]>"]
          (is (= (from-string s)
                 [[:html-block [s]]]))))

      (testing "multiline"
        (is (= (from-string "<![CDATA[\nabc\nxyz\n]]>")
               [[:html-block ["<![CDATA[" "abc" "xyz" "]]>"]]])))

      (testing "empty"
        (let [s "<![CDATA[]]>"]
          (= (from-string s)
             [[:html-block [s]]])))

      (testing "interrupt paragraph"
        (is (= (from-string "xyz\n<![CDATA[\n123]]>\nabc")
               [[:p          ["xyz"]]
                [:html-block ["<![CDATA[" "123]]>"]]
                [:p          ["abc"]]])))

      (testing "unpaired"
        (testing "opener"
          (is (= (postprocess (from-string "xyz\n<![CDATA[\nabc"))
                 [[:p          ["xyz"]]
                  [:html-block ["<![CDATA[" "abc"]]])))

        (testing "closer"
          (is (= (postprocess (from-string "xyz\n]]>\nabc"))
                 [[:p  ["xyz" "]]>" "abc"]]])))))

    (testing "variant 6"
      (testing "standard"
        (is (= (from-string "<p abc\nxyz\n\n123")
               [[:html-block ["<p abc" "xyz"]]
                [:blank      [""]]
                [:p          ["123"]]])))

      (testing "empty"
        (let [s "<p\n\n123"]
          (= (from-string s)
             [[:html-block ["<p"]]
              [:blank      [""]]
              [:p          ["123"]]])))

      (testing "interrupt paragraph"
        (is (= (from-string "xyz\n<p\n\nabc")
               [[:p          ["xyz"]]
                [:html-block ["<p"]]
                [:blank      [""]]
                [:p          ["abc"]]])))

      (testing "unpaired"
        (is (= (postprocess (from-string "xyz\n<p\nabc"))
               [[:p          ["xyz"]]
                [:html-block ["<p" "abc"]]]))))

    (testing "variant 7"
      (testing "standard"
        (is (= (from-string "<a qpr=\"klm\">abc\nxyz\n\n123")
               [[:html-block ["<a qpr=\"klm\">abc" "xyz"]]
                [:blank      [""]]
                [:p          ["123"]]])))

      (testing "empty"
        (let [s "<a qpr=\"klm\">\n\n123"]
          (= (from-string s)
             [[:html-block ["<a qpr=\"klm\">"]]
              [:blank      [""]]
              [:p          ["123"]]])))

      (testing "interrupt paragraph"
        (testing "variant exclusively 7"
          (is (= (from-string "xyz\n<a qpr=\"klm\">\n\nabc")
                 [[:p     ["xyz" "<a qpr=\"klm\">"]]
                  [:blank [""]]
                  [:p     ["abc"]]])))

        (testing "variant is both 6 and 7"
          (is (= (from-string "xyz\n<td>\n123\n\nabc")
                 [[:p          ["xyz"]]
                  [:html-block ["<td>" "123"]]
                  [:blank      [""]]
                  [:p          ["abc"]]]))))

      (testing "unpaired"
        (is (= (postprocess (from-string "xyz\n\n<a qpr=\"klm\">\nabc"))
               [[:p          ["xyz"]]
                [:blank      [""]]
                [:html-block ["<a qpr=\"klm\">" "abc"]]]))))

    (testing "nesting"
      (testing "different variants"
        (is (= (from-string "0\n<!--\n1\n<pre>\n2\n</pre>\n3\n-->\n4")
               [[:p          ["0"]]
                [:html-block ["<!--" "1" "<pre>" "2" "</pre>" "3" "-->"]]
                [:p          ["4"]]])))

      (testing "same variant"
        (is (= (postprocess (from-string "0\n<!--\n1\n<!--\n2\n-->\n3\n-->\n4"))
               [[:p          ["0"]]
                [:html-block ["<!--" "1" "<!--" "2" "-->"]]
                [:p          ["3" "-->" "4"]]]))))

    (testing "fuse iteratively"
      (is (= (add (from-string "0\n<!--\n1")
                  (from-string "2\n-->\n3"))
             [[:p          ["0"]]
              [:html-block ["<!--" "1" "2" "-->"]]
              [:p          ["3"]]])))))

