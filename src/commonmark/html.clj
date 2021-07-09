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

(def comment-begin-re #"(?<!\\)<!--")

(def comment-end-re #"(?<!\\)-->")

(def processing-instruction-begin-re #"(?<!\\)<\?")

(def processing-instruction-end-re #"(?<!\\)\?>")

(def declaration-begin-re #"(?<!\\)<![A-Z]")

(def declaration-end-re #"(?<!\\)>")

(def cdata-section-begin-re #"(?<!\\)<!\[CDATA\[")

(def cdata-section-end-re #"(?<!\\)\]\]>")

(def tag-re
  (re-pattern (str "(?:(?s)" open-tag-re "|"
                             closing-tag-re "|"
                             comment-begin-re #"(?:|(?!>)(?!->)(?!.*--.*-->).*?(?<!-))" comment-end-re "|"
                             processing-instruction-begin-re ".*?" processing-instruction-end-re "|"
                             declaration-begin-re #"[A-Z]*\s+(?:|[^>]*)" declaration-end-re "|"
                             cdata-section-begin-re ".*?" cdata-section-end-re ")")))

(def block-variant-6-tags ["address" "article" "aside" "base" "basefont"
                           "blockquote" "body" "caption" "center" "col"
                           "colgroup" "dd" "details" "dialog" "dir" "div" "dl"
                           "dt" "fieldset" "figcaption" "figure" "footer" "form"
                           "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6"
                           "head" "header" "hr" "html" "iframe" "legend" "li"
                           "link" "main" "menu" "menuitem" "nav" "noframes" "ol"
                           "optgroup" "option" "p" "param" "section" "source"
                           "summary" "table" "tbody" "td" "tfoot" "th" "thead"
                           "title" "tr" "track" "ul"])
