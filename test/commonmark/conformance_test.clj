(ns commonmark.conformance-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as java.io]
            [cheshire.core :as cheshire]
            [commonmark.render :as render]))

(deftest conformance-test
  (comment(let [filename "tests.json"
        tests (-> filename
                  java.io/resource
                  java.io/reader
                  cheshire/parse-stream)
        normalize (comp #(cond-> %
                           (not (string/ends-with? % "\n")) (str "\n"))
                        #(string/replace % #"(</?(?:ul|ol|li)>)" "$1\n")
                        #(string/replace % "</p>" "</p>\n")
                        #(string/replace % "</code></pre>" "\n</code></pre>")
                        #(string/replace % "\r\n" "\n")
                        #(string/replace % "<br />" "<br />\n"))
        from-string (comp normalize render/from-string)]
    (doseq [{:strs [markdown html example section]} tests]
      (testing (format "%s: Example %d" section example)
        (is (= html (from-string markdown))))))))

