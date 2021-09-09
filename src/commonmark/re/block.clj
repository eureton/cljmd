(ns commonmark.re.block
  (:require [clojure.string :as string]
            [commonmark.re.html :as re.html]
            [commonmark.re.common :as re.common]))

(def atx-heading
  #"^ {0,3}(#{1,6})(?:$|\s+(\p{Print}*?)\s*(?:(?<=\s)#+)?\s*$)")

(def setext-heading-underline
  #"^ {0,3}(=+|-+)\s*$")

(def thematic-break
  #"^ {0,3}(?:(?:-[ |\t]*){3,}|(?:_[ |\t]*){3,}|(?:[*][ |\t]*){3,})\s*$")

(def indented-chunk-line
  #"^( {0,3}\t| {4})(.*\S.*)$")

(def opening-code-fence
  #"^( {0,3})((`{3,})\s*([^`\s]*[^`]*)|(~{3,})\s*(\S*).*)$")

(def closing-code-fence
  #"^( {0,3})(`{3,}|~{3,}) *$")

(def paragraph-line
  #"^ {0,3}\S.*$")

(def list-item-marker
  #"( {0,3})([-+*]|\d{1,9}[.)])")

(def list-item-basic-lead-line
  (re-pattern (str "^" list-item-marker #"( {1,4})(\S\p{Print}*)$")))

(def list-item-indented-code-lead-line
  (re-pattern (str "^" list-item-marker #"( )(?= {4,})(\p{Print}*)$")))

(def list-item-blank-lead-line
  (re-pattern (str "^" list-item-marker #" *$")))

(def blockquote-marker
  #"( {0,3})>( ?)")

(def blockquote-line
  (re-pattern (str "^" blockquote-marker #"(.*)$")))

(def html-block-variant-1-tag
  #"(?:(?i)script|pre|style|textarea)")

(def html-block-variant-1-begin-line
  (re-pattern (str #"^ {0,3}" (re.common/unescaped \<)
                   html-block-variant-1-tag #"(?:[ \t>]|$).*")))

(def html-block-variant-2-begin-line
  (re-pattern (str #"^ {0,3}" re.html/comment-begin ".*")))

(def html-block-variant-3-begin-line
  (re-pattern (str #"^ {0,3}" re.html/processing-instruction-begin ".*")))

(def html-block-variant-4-begin-line
  (re-pattern (str #"^ {0,3}" re.html/declaration-begin ".*")))

(def html-block-variant-5-begin-line
  (re-pattern (str #"^ {0,3}" re.html/cdata-section-begin ".*")))

(def html-block-variant-6-begin-line
  (re-pattern (str #"^ {0,3}" (re.common/unescaped "</?")
                   "(?:(?i)" (string/join "|" re.html/block-variant-6-tags) ")"
                   #"(?:\s|/?>|$).*")))

(def html-block-variant-7-begin-line
  (re-pattern (str "^ {0,3}"
                   "(?:"
                   (re.html/open-tag {:exclude-tags ["script" "style"
                                                     "pre" "textarea"]})
                   "|"
                   re.html/closing-tag
                   ")"
                   #"\s*$")))

(def html-block-variant-1-end-line
  (re-pattern (str #"^ {0,3}(?! ).*?" (re.common/unescaped "</")
                   html-block-variant-1-tag #">.*")))

(def html-block-variant-2-end-line
  (re-pattern (str #"^ {0,3}(?! ).*?" re.html/comment-end ".*")))

(def html-block-variant-3-end-line
  (re-pattern (str #"^ {0,3}(?! ).*?"
                   re.html/processing-instruction-end
                   ".*")))

(def html-block-variant-4-end-line
  (re-pattern (str #"^ {0,3}(?! ).*?" re.html/declaration-end ".*")))

(def html-block-variant-5-end-line
  (re-pattern (str #"^ {0,3}(?! ).*?" re.html/cdata-section-end ".*")))

