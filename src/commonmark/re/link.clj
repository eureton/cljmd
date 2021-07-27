(ns commonmark.re.link
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.re.common :refer :all]
            [commonmark.util :as util]))

(def text
  (re-pattern (str "(?s)"
                   (unescaped #"\[") "("
                     (util/balanced-unescaped-re \[ \])
                     (util/excluding-re (blank-line))
                   ")" #"\]")))

(def wrapped-destination
  (re-pattern (str (util/but-unescaped-re \< \> {:exclude ["\r" "\n"]})
                   "*?")))

(def unwrapped-destination
  (re-pattern (str "(?!<)"
                   (util/balanced-unescaped-re \( \) {:intersect #"[^ \p{Cntrl}]"}))))

(def destination
  (re-pattern (str "(?:"
                     "<(" wrapped-destination ")>"
                     "|"
                     "(" unwrapped-destination ")"
                   ")")))

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
  (re-pattern (str no-preceding-backslash #"(!)?" text
                   #"\("
                     #"\s*"
                     destination "?"
                     "(?:"
                       #"\s+" "(" title ")"
                     ")?"
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
  (when (not-empty labels)
    (re-pattern (str #"(?u)(?i)(!)?" text
                     (apply util/or-re (map label-matcher labels))))))

(defn collapsed-reference
  [labels]
  (re-pattern (str #"(?u)(?i)(!)?"
                   (apply util/or-re (map label-matcher labels))
                   #"\[\]")))

(defn shortcut-reference
  [labels]
  (re-pattern (str #"(?u)(?i)(!)?"
                   (apply util/or-re (map label-matcher labels))
                   #"(?!\[\])"
                   "(?!" label ")")))

(def reference
  (ufn/to-fix not-empty (comp (ufn/ap util/or-re)
                              (juxt full-reference
                                    collapsed-reference
                                    shortcut-reference))
              nil))

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

