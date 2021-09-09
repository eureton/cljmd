(ns cljmd.conformance-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as java.io]
            [cheshire.core :as cheshire]
            [cljmd.render :refer [from-string]]))

(deftest conformance-test
  (let [filename "tests.json"
        tests (-> filename
                  java.io/resource
                  java.io/reader
                  cheshire/parse-stream)]
    (doseq [{:strs [markdown html example section]} tests]
      (testing (format "%s: Example %d" section example)
        (is (= html (from-string markdown)))))))

