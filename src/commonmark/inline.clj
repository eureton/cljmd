(ns commonmark.inline
  (:require [clojure.string :as string]))

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

(defn left-flanking-emphasis-delimeter-run-re
  [delimeter]
  (let [no-whitespace (re-pattern (str delimeter #"(?!\p{IsWhite_Space}|$)"))
        no-punctuation (re-pattern (str no-whitespace #"(?!\p{IsPunctuation})"))
        punctuation (re-pattern
                      (str #"(?<=^|\p{IsWhite_Space}|\p{IsPunctuation})"
                           no-whitespace
                           #"(?=\p{IsPunctuation})"))]
    (re-pattern (str "(?:" no-punctuation "|" punctuation ")"))))

(defn right-flanking-emphasis-delimeter-run-re
  [delimeter]
  (let [no-whitespace (re-pattern (str #"(?<!^|\p{IsWhite_Space})" delimeter))
        no-punctuation (re-pattern (str #"(?<!\p{IsPunctuation})" no-whitespace))
        punctuation (re-pattern
                      (str #"(?<=\p{IsPunctuation})"
                           no-whitespace
                           #"(?=\p{IsWhite_Space}|\p{IsPunctuation}|$)"))]
    (re-pattern (str "(?:" no-punctuation "|" punctuation ")"))))

(def star-emphasis-re
  (let [delimeter (emphasis-delimeter-re \* 1)
        left (left-flanking-emphasis-delimeter-run-re delimeter)
        right (right-flanking-emphasis-delimeter-run-re delimeter)]
    (re-pattern (str left
                     "("
                       "(?:"
                         "(?!" left ")" #"\p{Print}"
                       ")"
                       "*?"
                     ")"
                     right))))

(def lobar-open-emphasis-re
  (let [delimeter (emphasis-delimeter-re \_ 1)
        left-re (left-flanking-emphasis-delimeter-run-re delimeter)
        right-re (right-flanking-emphasis-delimeter-run-re delimeter)
        punctuated-right-re (re-pattern (str #"\p{IsPunctuation}" right-re))]
    (re-pattern (str "(?<=" left-re ")"
                     "(?:"
                       "(?<!" right-re ")|(?<=" punctuated-right-re ")"
                     ")"))))

(def lobar-close-emphasis-re
  (let [delimeter (emphasis-delimeter-re \_ 1)
        left-re (left-flanking-emphasis-delimeter-run-re delimeter)
        right-re (right-flanking-emphasis-delimeter-run-re delimeter)
        punctuated-left-re (re-pattern (str left-re #"\p{IsPunctuation}"))]
    (re-pattern (str "(?=" right-re ")"
                     "(?:"
                       "(?!" left-re ")|(?=" punctuated-left-re ")"
                     ")"))))

(def lobar-emphasis-re
  (re-pattern (str lobar-open-emphasis-re
                   #"(\p{Print}*)"
                   lobar-close-emphasis-re)))

(def emphasis-re
  (re-pattern (str "(?:" star-emphasis-re "|" lobar-emphasis-re ")")))

(defn emphasis
  [string]
  (when-some [[_ star-content lobar-content] (re-find emphasis-re string)]
    {:content (or star-content lobar-content)
     :tag :em
     :pattern emphasis-re}))

(def star-strong-emphasis-re
  (let [delimeter (emphasis-delimeter-re \* 2)]
    (re-pattern (str (left-flanking-emphasis-delimeter-run-re delimeter)
                     #"(\p{Print}*)"
                     (right-flanking-emphasis-delimeter-run-re delimeter)))))

(def lobar-open-strong-emphasis-re
  (let [delimeter (emphasis-delimeter-re \_ 2)
        left-re (left-flanking-emphasis-delimeter-run-re delimeter)
        right-re (right-flanking-emphasis-delimeter-run-re delimeter)
        punctuated-right-re (re-pattern (str #"\p{IsPunctuation}" right-re))]
    (re-pattern (str "(?<=" left-re ")"
                     "(?:"
                       "(?<!" right-re ")|(?<=" punctuated-right-re ")"
                     ")"))))

(def lobar-close-strong-emphasis-re
  (let [delimeter (emphasis-delimeter-re \_ 2)
        left-re (left-flanking-emphasis-delimeter-run-re delimeter)
        right-re (right-flanking-emphasis-delimeter-run-re delimeter)
        punctuated-left-re (re-pattern (str left-re #"\p{IsPunctuation}"))]
    (re-pattern (str "(?=" right-re ")"
                     "(?:"
                       "(?!" left-re ")|(?=" punctuated-left-re ")"
                     ")"))))

(def lobar-strong-emphasis-re
  (re-pattern (str lobar-open-strong-emphasis-re
                   #"(\p{Print}*)"
                   lobar-close-strong-emphasis-re)))

(def strong-emphasis-re
  (re-pattern (str "(?:" star-strong-emphasis-re "|" lobar-strong-emphasis-re ")")))

(defn strong-emphasis
  [string]
  (when-some [[_ star-content lobar-content] (re-find strong-emphasis-re string)]
    {:content (or star-content lobar-content)}))

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

(defn tagger
  [string]
  (when string
    ((some-fn code-span emphasis) string)))

