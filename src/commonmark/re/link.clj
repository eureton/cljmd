(ns commonmark.re.link
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.re.common :refer :all]
            [commonmark.util :as util]))

(def text
  (re-pattern (str "(?<!" (unescaped \!) ")"
                   balanced-square-brackets)))

(def wrapped-destination
  (re-pattern (str (util/but-unescaped-re \< \> {:exclude ["\r" "\n"]})
                   "*?")))

(def unwrapped-destination
  (re-pattern (str "(?!<)"
                   (util/balanced-unescaped-re \( \) {:intersect #"[^ \p{Cntrl}]"}))))

(def destination
  (util/or-re (str (unescaped \<) "(" wrapped-destination ")" (unescaped \>))
              (str "(" unwrapped-destination ")")))

(def title
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

(def label
  (let [open (unescaped "\\[")
        close (unescaped "\\]")]
    (re-pattern (str "(?s)(?:" open
                             "(?=" #"\s*(?!\\\])[\S&&[^\]]].*?" close ")"
                             "(" (util/but-unescaped-re \[ \]) "{1,999})"
                           close ")"))))

(def inline
  (re-pattern (str text
                   #"\("
                     #"\s*"
                     destination "?"
                     "(?:" #"\s+" "(" title ")" ")?"
                     #"\s*"
                   #"\)")))

(defn label-matcher
  [string]
  (as-> string v
        (string/replace v #"([-\[\]{}()*+?.\\^$|#])" "\\\\$1")
        (string/replace v #"\s+" "\\\\s+")
        (str #"\[" "(" v ")" #"\]")
        (re-pattern v)))

(defn full-reference
  [labels]
  (if (empty? labels)
    unmatchable
    (re-pattern (str #"(?u)(?i)" text
                     (apply util/or-re (map label-matcher labels))))))

(defn collapsed-reference
  [labels]
  (if (empty? labels)
    unmatchable
    (re-pattern (str "(?u)(?i)(?<!" (unescaped \!) ")"
                     (apply util/or-re (map label-matcher labels))
                     #"\[\]"))))

(defn shortcut-reference
  [labels]
  (if (empty? labels)
    unmatchable
    (re-pattern (str "(?u)(?i)(?<!" (unescaped \!) ")"
                     (apply util/or-re (map label-matcher labels))
                     #"(?!\[\])"
                     "(?!" label ")"))))

(def reference-definition
  (re-pattern (str #"(?m)^ {0,3}" label ":"
                   #"\s*(?=\S+(?:\s|$))" destination
                   "(?:" #"\s+" title ")?"
                   #"\s*$")))

(def reference-definition-batch
  (re-pattern (str #"(?m)(?s)\A"
                   "(?:^" reference-definition "$"
                          line-ending "?"
                   ")*")))

