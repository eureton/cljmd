(ns commonmark.re.html
  (:require [clojure.string :as string]
            [commonmark.util :as util]))

(def tag-name
  #"[a-zA-Z][\w-&&[^_]]*")

(defn open-tag
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
                      (reduce #(str "(?!" %2 "\\b)" %1) tag-name exclude-tags)
                      "(?:" attribute ")*"
                      #"\s*"
                      "/?>"))))
  ([]
   (open-tag {})))

(def closing-tag
  (re-pattern (str (util/non-backslash-re "</") tag-name #"\s*" ">")))

(def comment-begin
  (util/non-backslash-re "<!--"))

(def comment-end
  (util/non-backslash-re "-->"))

(def processing-instruction-begin
  (util/non-backslash-re #"<\?"))

(def processing-instruction-end
  (util/non-backslash-re #"\?>"))

(def declaration-begin
  (util/non-backslash-re #"<![A-Z]"))

(def declaration-end
  (util/non-backslash-re \>))

(def cdata-section-begin
  (util/non-backslash-re #"<!\[CDATA\["))

(def cdata-section-end
  (util/non-backslash-re #"\]\]>"))

(def tag
  (re-pattern (str "(?:(?s)" (open-tag) "|"
                             closing-tag "|"
                             comment-begin #"(?:|(?!>)(?!->)(?!.*--.*-->).*?(?<!-))" comment-end "|"
                             processing-instruction-begin ".*?" processing-instruction-end "|"
                             declaration-begin #"[A-Z]*\s+(?:|[^>]*)" declaration-end "|"
                             cdata-section-begin ".*?" cdata-section-end ")")))

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
