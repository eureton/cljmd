(ns commonmark.inline
  (:require [clojure.string :as string]
            [commonmark.util :as util]))

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

(def inline-link-text-re
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
  (re-pattern (str #"\(+"
                     "("
                       "(?:"
                         #"\\[()]"
                         "|"
                         #"(?![()])\p{Print}"
                       ")*"
                     ")"
                   #"\)+")))

(def inline-link-title-re
  (re-pattern (str "(?:"
                     inline-link-quoted-title-re
                     "|"
                     inline-link-parenthesized-title-re
                   ")")))

(def inline-link-re
  (re-pattern (str ".*"
                   #"(?<!\\)"
                   #"\[" "(" inline-link-text-re ")" #"\]"
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
    (when-let [[_ text destination-wrapped
                destination-unwrapped full-title _
                quoted-title parenthesized-title] (re-find inline-link-re string)]
      (when (util/balanced-delimiters? "(" ")" (or full-title ""))
        {:text text
         :destination (or destination-wrapped destination-unwrapped)
         :title (or quoted-title parenthesized-title)
         :tag :link
         :pattern inline-link-re}))))

(defn tagger
  [string]
  (some->> string
           ((some-fn code-span
                     inline-link
                     emphasis
                     strong-emphasis))))

