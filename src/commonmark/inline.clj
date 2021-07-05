(ns commonmark.inline
  (:require [clojure.string :as string]
            [commonmark.util :as util]))

(def line-ending-re
  #"(?:\r\n|\n|\r)")

(comment "Code spans

OK  A backtick string is a string of one or more backtick characters (`)
OK  that is neither preceded nor followed by a backtick.

OK  A code span begins with a backtick string
OK  and ends with a backtick string of equal length.

OK  The contents of the code span are the characters between the two backtick strings,
OK  normalized in the following ways:
OK    * line endings are converted to spaces.
OK    * if the resulting string both begins and ends with a space character,
OK      but does not consist entirely of space characters,
OK      a single space character is removed from the front and back.")

(def code-span-re
  ""
  (let [backtick "((?<!`)`+(?!`))"
        spaced #"[ \n](.*?[^ ].*?)[ \n]"
        non-spaced #"(.*?[^`])"
        same-backtick #"\1(?!`)"]
    (re-pattern (str "(?s)" backtick
                     "(?:" spaced "|" non-spaced ")"
                     same-backtick))))

(defn code-span
  [string]
  (when-some [[_ backtick-string
               spaced-content non-spaced-content] (re-find code-span-re string)]
    {:backtick-string backtick-string
     :content (-> (or spaced-content non-spaced-content)
                  (string/replace #"(?:\r|\n|\r\n)"  " "))
     :tag :cs
     :pattern code-span-re}))

(comment "Emphasis, Strong Emphasis

OK  A delimiter run is
OK  either a sequence of one or more * characters
OK  that is not preceded or followed by a non-backslash-escaped * character,
OK  or a sequence of one or more _ characters that is not preceded or followed by a non-backslash-escaped _ character.

OK  A left-flanking delimiter run is a delimiter run that is
OK  (1) not followed by Unicode whitespace,
OK  and either (2a) not followed by a punctuation character,
OK  or (2b) followed by a punctuation character
OK  and preceded by Unicode whitespace or a punctuation character.

OK  A right-flanking delimiter run is a delimiter run that is
OK  (1) not preceded by Unicode whitespace,
OK  and either (2a) not preceded by a punctuation character,
OK  or (2b) preceded by a punctuation character
OK  and followed by Unicode whitespace
OK  or a punctuation character.

OK  The beginning and the end of the line count as Unicode whitespace.

    The following rules define emphasis and strong emphasis:
      1. A single * character can open emphasis iff it is part of a left-flanking delimiter run.
      2. A single _ character can open emphasis iff it is part of a left-flanking delimiter run and
         either (a) not part of a right-flanking delimiter run
         or (b) part of a right-flanking delimiter run preceded by punctuation.
      3. A single * character can close emphasis iff it is part of a right-flanking delimiter run.
      4. A single _ character can close emphasis iff it is part of a right-flanking delimiter run and
         either (a) not part of a left-flanking delimiter run
         or (b) part of a left-flanking delimiter run followed by punctuation.
      5. A double ** can open strong emphasis iff it is part of a left-flanking delimiter run.
      6. A double __ can open strong emphasis iff it is part of a left-flanking delimiter run and
         either (a) not part of a right-flanking delimiter run
         or (b) part of a right-flanking delimiter run preceded by punctuation.
      7. A double ** can close strong emphasis iff it is part of a right-flanking delimiter run.
      8. A double __ can close strong emphasis iff it is part of a right-flanking delimiter run and
         either (a) not part of a left-flanking delimiter run
         or (b) part of a left-flanking delimiter run followed by punctuation.
      9. Emphasis begins with a delimiter that can open emphasis
         and ends with a delimiter that can close emphasis,
         and that uses the same character (_ or *) as the opening delimiter.
         The opening and closing delimiters must belong to separate delimiter runs.
         If one of the delimiters can both open and close emphasis,
         then the sum of the lengths of the delimiter runs containing the opening and closing delimiters
         must not be a multiple of 3 unless both lengths are multiples of 3.
     10. Strong emphasis begins with a delimiter that can open strong emphasis
         and ends with a delimiter that can close strong emphasis,
         and that uses the same character (_ or *) as the opening delimiter.
         The opening and closing delimiters must belong to separate delimiter runs.
         If one of the delimiters can both open and close strong emphasis,
         then the sum of the lengths of the delimiter runs containing the opening and closing delimiters
         must not be a multiple of 3 unless both lengths are multiples of 3.
     11. A literal * character cannot occur at the beginning
         or end of *-delimited emphasis
         or **-delimited strong emphasis,
         unless it is backslash-escaped.
     12. A literal _ character cannot occur at the beginning
         or end of _-delimited emphasis
         or __-delimited strong emphasis,
         unless it is backslash-escaped.

    Where rules 1–12 above are compatible with multiple parsings, the following principles resolve ambiguity:
     13. The number of nestings should be minimized.
         Thus an interpretation <strong>...</strong> is always preferred to <em><em>...</em></em>.
     14. An interpretation <em><strong>...</strong></em> is always preferred to <strong><em>...</em></strong>.
     15. When two potential emphasis or strong emphasis spans overlap,
         so that the second begins before the first ends
         and ends after the first ends,
         the first takes precedence.
         Thus *foo _bar* baz_ is parsed as <em>foo _bar</em> baz_
         rather than *foo <em>bar* baz</em>.
     16. When there are two potential emphasis or strong emphasis spans
         with the same closing delimiter,
         the shorter one (the one that opens later) takes precedence.
         Thus **foo **bar baz** is parsed as **foo <strong>bar baz</strong>
         rather than <strong>foo **bar baz</strong>.
     17. Inline code spans, links, images, and HTML tags group more tightly than emphasis.
         So, when there is a choice between an interpretation that contains one of these elements
         and one that does not, the former always wins.
         Thus *[foo*] (bar) is parsed as *<a href= "bar">foo*</a>
         rather than as <em> [foo</em>] (bar).")

(defn emphasis-delimeter-re
  [character length]
  (let [escaped (string/escape (str character) {\* "\\*"})]
    (re-pattern (str "(?<!" escaped ")"
                     escaped "{" length "}"
                     "(?!" escaped ")"))))

(defn lfdr-nopunc-re
  [delimeter]
  (re-pattern (str delimeter
                   #"(?!\p{IsWhite_Space}|$)"
                   #"(?!\p{IsPunctuation})")))

(defn lfdr-punc-re
  [delimeter]
  (re-pattern (str #"(?<=^|\p{IsWhite_Space}|\p{IsPunctuation})"
                   delimeter
                   #"(?!\p{IsWhite_Space}|$)"
                   #"(?=\p{IsPunctuation})")))

(defn lfdr-re
  [delimeter]
  (re-pattern (str "(?:"
                     (lfdr-nopunc-re delimeter) "|" (lfdr-punc-re delimeter)
                   ")")))

(defn rfdr-punc-re
  [delimeter]
  (re-pattern (str #"(?<=\p{IsPunctuation})"
                   #"(?<!^|\p{IsWhite_Space})"
                   delimeter
                   #"(?=\p{IsWhite_Space}|\p{IsPunctuation}|$)")))

(defn rfdr-nopunc-re
  [delimeter]
  (re-pattern (str #"(?<!\p{IsPunctuation})"
                   #"(?<!^|\p{IsWhite_Space})"
                   delimeter)))

(defn rfdr-re
  [delimeter]
  (re-pattern (str "(?:"
                     (rfdr-nopunc-re delimeter) "|" (rfdr-punc-re delimeter)
                   ")")))

(defn star-emphasis-re
  [delimeter]
  (let [left (lfdr-re delimeter)
        right (rfdr-re delimeter)]
    (re-pattern (str left
                     "("
                       "(?:"
                         "(?!" left ")" #"\p{Print}"
                       ")*?"
                     ")"
                     right))))

(defn lobar-open-emphasis-re
  [delimeter]
  (let [left (lfdr-re delimeter)
        right (rfdr-re delimeter)
        punctuated-right (rfdr-punc-re delimeter)]
    (re-pattern (str "(?:"
                       "(?=" left ")" "(?!" right ")"
                       "|"
                       "(?=" left ")" "(?=" punctuated-right ")"
                     ")" left))))

(defn lobar-close-emphasis-re
  [delimeter]
  (let [left (lfdr-re delimeter)
        punctuated-left (lfdr-punc-re delimeter)
        right (rfdr-re delimeter)]
    (re-pattern (str "(?:"
                       "(?=" right ")" "(?!" left ")"
                       "|"
                       "(?=" right ")" "(?=" punctuated-left ")"
                     ")" right))))

(defn lobar-emphasis-re
  [delimeter]
  (let [open (lobar-open-emphasis-re delimeter)
        close (lobar-close-emphasis-re delimeter)]
    (re-pattern (str open
                     "("
                       "(?:"
                         "(?!" open ")" #"\p{Print}"
                       ")*?"
                     ")"
                     close))))

(defn emphasis-re
  [length]
  (re-pattern (str "(?:"
                     (star-emphasis-re (emphasis-delimeter-re \* length)) "|"
                     (lobar-emphasis-re (emphasis-delimeter-re \_ length))
                   ")")))

(defn emphasis
  [string]
  (let [pattern (emphasis-re 1)]
    (when-some [[_ star-content lobar-content] (re-find pattern string)]
      {:content (or star-content lobar-content)
       :tag :em
       :pattern pattern})))

(defn strong-emphasis
  [string]
  (let [pattern (emphasis-re 2)]
    (when-some [[_ star-content lobar-content] (re-find pattern string)]
      {:content (or star-content lobar-content)
       :tag :strong
       :pattern pattern})))

(comment "Links

    A link contains link text   (the visible text), a link destination   (the URI that is the link
    destination), and optionally a link title . There are two basic kinds of links in Markdown. In inline
    links  the destination and title are given immediately after the link text. In reference links  the
    destination and title are defined elsewhere in the document.

    A link text  consists of a sequence of zero or more inline elements enclosed by square brackets  ([ and ]).
    The following rules apply:
      * Links may not contain other links, at any level of nesting. If multiple otherwise valid link definitions
        appear nested inside each other, the inner-most definition is used.
      * Brackets are allowed in the link text  only if  (a) they are backslash-escaped or  (b) they appear as a
        matched pair of brackets, with an open bracket  [, a sequence of zero or more inlines, and a close bracket
                                                                                                        ].
      * Backtick code spans , autolinks , and raw HTML tags  bind more tightly than the brackets in
        link text. Thus, for example,  [foo`]` could not be a link text, since the second  is part of a code span.
      * The brackets in link text bind more tightly than markers for emphasis and strong emphasis . Thus, for
        example, *[foo*] (url) is a link.

    A link destination  consists of either
      * a sequence of zero or more characters between an opening < and a closing > that contains no line breaks or
        unescaped < or > characters, or
      * a nonempty sequence of characters that does not start with <, does not include ASCII space or control
        characters, and includes parentheses only if  (a) they are backslash-escaped or  (b) they are part of a
        balanced pair of unescaped parentheses.  (Implementations may impose limits on parentheses nesting to avoid
        performance issues, but at least three levels of nesting should be supported.)

    A link title  consists of either
      * a sequence of zero or more characters between straight double-quote characters  (\"), including a \" character
        only if it is backslash-escaped, or
      * a sequence of zero or more characters between straight single-quote characters  ('), including a ' character
        only if it is backslash-escaped, or
      * a sequence of zero or more characters between matching parentheses  ((...)), including a  ( or ) character
        only if it is backslash-escaped.

    Although link titles  may span multiple lines, they may not contain a blank line .

    An inline link  consists of a link text  followed immediately by a left parenthesis  (, optional
    whitespace , an optional link destination , an optional link title  separated from the link
    destination by whitespace , optional whitespace , and a right parenthesis ). The link’s text consists
    of the inlines contained in the link text   (excluding the enclosing square brackets). The link’s URI
    consists of the link destination, excluding enclosing <...> if present, with backslash-escapes in effect as
    described above. The link’s title consists of the link title, excluding its enclosing delimiters, with
    backslash-escapes in effect as described above.)")

(def link-text-re
  (util/balanced-re \[ \]))

(def inline-link-wrapped-destination-re
  (re-pattern (str "(?:"
                     #"\\[<>]"
                     "|"
                     #"[\p{Print}&&[^\r\n<>]]"
                   ")*?")))

(def inline-link-unwrapped-destination-re
  (re-pattern (str "(?!<)"
                   (util/balanced-re \( \) {:intersect #"[^ \p{Cntrl}]"}))))

(def inline-link-destination-re
  (re-pattern (str "(?:"
                     "<(" inline-link-wrapped-destination-re ")>"
                     "|"
                     "(" inline-link-unwrapped-destination-re ")"
                   ")")))

(def inline-link-quoted-title-re
  (re-pattern #"(?<delim>['\\\"])((?:\\\k<delim>|(?!\k<delim>)\p{Print})*)\k<delim>"))

(def inline-link-parenthesized-title-re
  (let [inner (str "("
                     "(?:"
                       #"\\[()]"
                       "|"
                       #"[\p{Print}&&[^()]]"
                     ")*"
                   ")")]
    (re-pattern (str "(?:"
                       #"\({1}" inner #"\){1}" "|"
                       #"\({2}" inner #"\){2}" "|"
                       #"\({3}" inner #"\){3}" "|"
                       #"\({4}" inner #"\){4}" "|"
                       #"\({5}" inner #"\){5}"
                     ")"))))

(def inline-link-title-re
  (re-pattern (str "(?:"
                     inline-link-quoted-title-re
                     "|"
                     inline-link-parenthesized-title-re
                   ")")))

(def inline-link-re
  (re-pattern (str #"(?<!\\)(!)?(?<!\\)\[" "(" link-text-re ")" #"\]"
                   #"\("
                     #"\s*"
                     inline-link-destination-re "?"
                     "(?:"
                       #"\s+" "(" inline-link-title-re ")"
                     ")?"
                     #"\s*"
                   #"\)")))

(defn inline-link
  [string]
  (when string
    (when-let [[_ img? text destination-wrapped
                destination-unwrapped full-title _
                quoted-title & parenthesized-title] (re-find inline-link-re string)]
      (if-let [inner (and (not img?)
                          (inline-link text))]
        (update inner
                :pattern
                (comp re-pattern (partial format "(?<=%2$s)%1$s"))
                (->> inner
                     :text
                     (string/index-of text)
                     (subs text 0)
                     util/escape-re-delimiter))
        (let [destination (or destination-wrapped destination-unwrapped)
              title (or quoted-title (->> parenthesized-title (remove nil?) first))]
          (cond-> {:text text
                   :tag (if img? :img :a)
                   :pattern inline-link-re}
            destination (assoc :destination destination)
            title (assoc :title title)))))))

(comment "Reference links
    There are three kinds of reference link[861]s: full[862], collapsed[863], and shortcut[864].

    A full reference link[865] consists of a link text[866] immediately followed by a link label[867] that
    matches [868] a link reference definition [869] elsewhere in the document.

    A link label [870] begins with a left bracket  ([) and ends with the first right bracket (]) that is not
    backslash-escaped. Between these brackets there must be at least one non-whitespace character[871]. Unescaped
    square bracket characters are not allowed inside the opening and closing square brackets of link labels[872]. A
    link label can have at most 999 characters inside the square brackets.

    One label matches [873] another just in case their normalized forms are equal. To normalize a label, strip off
    the opening and closing brackets, perform the Unicode case fold, strip leading and trailing whitespace [874] and
    collapse consecutive internal whitespace [875] to a single space. If there are multiple matching reference link
    definitions, the one that comes first in the document is used.  (It is desirable in such cases to emit a
    warning.)

    The contents of the first link label are parsed as inlines, which are used as the link’s text. The link’s URI
    and title are provided by the matching link reference definition [876].)")

(def link-label-re
  (re-pattern (str #"(?=.*\S)"
                   "(?:"
                     #"\\[\[\]]" "|"
                     #"[^\[\]]"
                   "){1,999}")))

(def reference-link-re
  (re-pattern (str #"(!)?\[" "(" link-text-re ")" #"\]"
                   "(?:"
                     #"\[" "(" link-label-re ")?" #"\]"
                   ")?")))

(defn reference-link
  [string]
  (when string
    (when-let [[_ img? text label] (re-find reference-link-re string)]
      (cond-> {:text text
               :tag (if img? :img :a)
               :pattern reference-link-re}
        label (assoc :label label)))))

(def absolute-uri-re
  (re-pattern (str #"\p{Alpha}[\p{Alnum}+.-]{1,31}" ":"
                   #"[\S&&[^\p{Cntrl}<>]]*")))

(def email-address-re
  (re-pattern (str "[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+"
                   "@"
                   "[a-zA-Z0-9]"
                   "(?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
                   "(?:"
                     #"\.[a-zA-Z0-9]"
                     "(?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
                   ")*")))

(def autolink-re
  (re-pattern (str "<(" absolute-uri-re "|" email-address-re ")>")))

(defn autolink
  [string]
  (when string
    (when-let [[_ uri] (re-find autolink-re string)]
      {:tag :auto
       :pattern autolink-re
       :uri uri
       :label (java.net.URLDecoder/decode uri)})))

(def html-tag-name-re
  #"[a-zA-Z][\w-&&[^_]]*")

(def html-open-tag-re
  (let [attribute-name #"[a-zA-Z_:][\w.:-]*"
        unquoted-value #"[\p{Print}&&[^\s\"'=<>`]]+"
        single-quoted-value #"'[\p{Print}&&[^']]*'"
        double-quoted-value #"\"[\p{Print}&&[^\"]]*\""
        attribute-value (str "(?:" unquoted-value "|"
                                   single-quoted-value "|"
                                   double-quoted-value ")")
        attribute-value-spec (str #"\s*=\s*" attribute-value)
        attribute (str #"\s+" attribute-name "(?:" attribute-value-spec ")?")]
    (re-pattern (str "<" html-tag-name-re
                         "(?:" attribute ")*"
                         #"\s*"
                         "/?" ">"))))

(def html-closing-tag-re
  (re-pattern (str "</" html-tag-name-re #"\s*" ">")))

(def html-comment-re
  #"(?:(?s)<!--(?:|(?!>)(?!->)(?!.*--.*-->).*?(?<!-))-->)")

(def html-processing-instruction-re
  #"(?s)(?<!\\)<\?.*?\?>")

(def html-declaration-re
  #"(?s)(?<!\\)<![A-Z]+\s+(?:|[^>]*)>")

(def html-cdata-section-re
  #"(?s)(?<!\\)<!\[CDATA\[.*?]]>")

(def html-tag-re
  (re-pattern (str "(?:" html-open-tag-re "|"
                         html-closing-tag-re "|"
                         html-comment-re "|"
                         html-processing-instruction-re "|"
                         html-declaration-re "|"
                         html-cdata-section-re ")")))

(def hard-line-break-re
  (re-pattern (str "(?:" "  " line-ending-re "|"
                         #"\\" line-ending-re ")")) )

(defn hard-line-break
  [string]
  (some->> string
           (re-find hard-line-break-re)
           (hash-map :tag :hbr :pattern hard-line-break-re :content)))

(defn html
  [string]
  (when string
    (when-some [html (re-find html-tag-re string)]
      {:tag :html-inline
       :pattern html-tag-re
       :content html})))

(defn text
  [string]
  (when string
    {:tag :txt
     :pattern #".*"
     :content (string/replace string #"\\(?=\p{Punct})" "")}))

(defn tagger
  [string]
  (some->> string
           ((some-fn code-span
                     hard-line-break
                     inline-link
                     reference-link
                     emphasis
                     strong-emphasis
                     autolink
                     html
                     text))))

