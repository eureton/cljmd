(ns commonmark.html
  (:require [clojure.string :as string]
            [commonmark.util :as util]))

(def tag-name-re
  #"[a-zA-Z][\w-&&[^_]]*")

(def open-tag-re
  (let [attribute-name #"[a-zA-Z_:][\w.:-]*"
        unquoted-value #"[^\s\"'=<>`]+"
        single-quoted-value #"'[^']*'"
        double-quoted-value #"\"[^\"]*\""
        attribute-value (str "(?:" unquoted-value "|"
                                   single-quoted-value "|"
                                   double-quoted-value ")")
        attribute-value-spec (str #"\s*=\s*" attribute-value)
        attribute (str #"\s+" attribute-name "(?:" attribute-value-spec ")?")]
    (re-pattern (str #"(?<!\\)<"
                     tag-name-re
                     "(?:" attribute ")*"
                     #"\s*"
                     "/?>"))))

(def closing-tag-re
  (re-pattern (str #"(?<!\\)</" tag-name-re #"\s*" ">")))

(def comment-re
  #"(?:(?s)(?<!\\)<!--(?:|(?!>)(?!->)(?!.*--.*-->).*?(?<!-))-->)")

(def processing-instruction-re
  #"(?s)(?<!\\)<\?.*?\?>")

(def declaration-re
  #"(?s)(?<!\\)<![A-Z]+\s+(?:|[^>]*)>")

(def cdata-section-re
  #"(?s)(?<!\\)<!\[CDATA\[.*?\]\]>")

(def tag-re
  (re-pattern (str "(?:" open-tag-re "|"
                         closing-tag-re "|"
                         comment-re "|"
                         processing-instruction-re "|"
                         declaration-re "|"
                         cdata-section-re ")")))

