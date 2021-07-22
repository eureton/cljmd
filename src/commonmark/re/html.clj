(ns commonmark.re.html
  (:require [clojure.string :as string]
            [commonmark.util :as util]))

(def tag-name-re
  #"[a-zA-Z][\w-&&[^_]]*")

(defn open-tag-re
  ([{:keys [exclude-tags]
    :or {exclude-tags []}}]
   (let [attribute-name #"[a-zA-Z_:][\w.:-]*"
         unquoted-value #"[^\s\"'=<>`]+"
         single-quoted-value #"'[^']*'"
         double-quoted-value #"\"[^\"]*\""
         attribute-value (str "(?:" unquoted-value "|"
                                    single-quoted-value "|"
                                    double-quoted-value ")")
         attribute-value-spec (str #"\s*=\s*" attribute-value)
         attribute (str #"\s+" attribute-name "(?:" attribute-value-spec ")?")]
     (re-pattern (str (util/non-backslash-re \<)
                      (reduce #(str "(?!" %2 "\\b)" %1) tag-name-re exclude-tags)
                      "(?:" attribute ")*"
                      #"\s*"
                      "/?>"))))
  ([]
   (open-tag-re {})))

(def closing-tag-re
  (re-pattern (str (util/non-backslash-re "</") tag-name-re #"\s*" ">")))

(def comment-begin-re
  (util/non-backslash-re "<!--"))

(def comment-end-re
  (util/non-backslash-re "-->"))

(def processing-instruction-begin-re
  (util/non-backslash-re #"<\?"))

(def processing-instruction-end-re
  (util/non-backslash-re #"\?>"))

(def declaration-begin-re
  (util/non-backslash-re #"<![A-Z]"))

(def declaration-end-re
  (util/non-backslash-re \>))

(def cdata-section-begin-re
  (util/non-backslash-re #"<!\[CDATA\["))

(def cdata-section-end-re
  (util/non-backslash-re #"\]\]>"))

(def tag-re
  (re-pattern (str "(?:(?s)" (open-tag-re) "|"
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
