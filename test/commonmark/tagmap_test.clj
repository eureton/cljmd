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
          ls2 ["``` "
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

      (testing "tags"
        (is (= (map second result)
               [(concat ls1 [(first ls2)])
                (rest ls2)]))))))

(deftest parse-test
  (testing "setext heading"
    (testing "minimal"
      (is (= (parse "xyz\r\n===")
             [[:stxh ["xyz"]]]))))

  (testing "fenced code block"
    (testing "minimal"
      (is (= (parse "```\r\nxyz\r\n```")
             [[:ofcblk ["```" "xyz" "```"]]])))))

