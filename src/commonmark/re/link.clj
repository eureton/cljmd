(ns commonmark.re.link
  (:require [clojure.string :as string]
            [commonmark.re.common :refer :all]
            [commonmark.util :as util]))

(def text-re
  (re-pattern (str "(?s)"
                   (util/non-backslash-re #"\[") "("
                     (util/balanced-re \[ \])
                     (util/excluding-re (blank-line-re))
                   ")" #"\]")))

(def wrapped-destination-re
  (re-pattern (str (util/but-unescaped-re \< \> {:exclude ["\r" "\n"]})
                   "*?")))

(def unwrapped-destination-re
  (re-pattern (str "(?!<)"
                   (util/balanced-re \( \) {:intersect #"[^ \p{Cntrl}]"}))))

(def destination-re
  (re-pattern (str "(?:"
                     "<(" wrapped-destination-re ")>"
                     "|"
                     "(" unwrapped-destination-re ")"
                   ")")))

(def title-re
  (let [escape #(string/escape (str %) {\( #"\(" \) #"\)"})
        inner #(str (escape %1)
                    (no-blank-line-till (escape %2))
                    "(" (util/but-unescaped-re %1 %2) "*)"
                    (escape %2))]
    (re-pattern (str "(?s)(?:"
                       (inner \' \') "|"
                       (inner \" \") "|"
                       (inner \( \))
                     ")"))))

(def label-re
  (let [open (util/non-backslash-re \[)
        close (util/non-backslash-re \])]
    (re-pattern (str "(?s)(?:" open
                             "(?=" #"\s*(?!\\\])[\S&&[^\]]].*?" close ")"
                             "(" (util/but-unescaped-re \[ \]) "{1,999})"
                           close ")"))))

(def inline-re
  (re-pattern (str #"(?<!\\)(!)?" text-re
                   #"\("
                     #"\s*"
                     destination-re "?"
                     "(?:"
                       #"\s+" "(" title-re ")"
                     ")?"
                     #"\s*"
                   #"\)")))

(def full-reference-re
  (re-pattern (str #"(!)?" text-re label-re)))

(def textless-reference-re
  (re-pattern (str label-re #"(?:\[\])?")))

(def reference-definition-re
  (re-pattern (str #"(?m)^ {0,3}" label-re ":"
                   #"\s*(?=\S+(?:\s|$))" destination-re
                   "(?:" #"\s+" title-re ")?"
                   #"\s*$")))

(def reference-definition-batch-re
  (re-pattern (str #"(?m)(?s)\A"
                   "(?:^" reference-definition-re "$"
                          line-ending-re "?"
                   ")*")))

