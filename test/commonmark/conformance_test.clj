(ns commonmark.conformance-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as java.io]
            [cheshire.core :as cheshire]
            [commonmark.render :as render]))

(deftest conformance-test
  (let [filename "tests.json"
        tests (-> filename
                  java.io/resource
                  java.io/reader
                  cheshire/parse-stream)
        normalize (comp #(format "%s\n" %)
                        #(string/replace % "\r\n" "\n")
                        #(string/replace % "<br />" "<br />\n"))
        from-string (comp normalize render/from-string)]
    (doseq [{:strs [markdown html example section]} tests]
      (testing (format "%s: Example %d" section example)
        (is (= html (from-string markdown)))))))

