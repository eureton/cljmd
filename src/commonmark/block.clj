(ns commonmark.block
  (:require [clojure.string :as string]))

(comment "ATX Headings
         
OK  An ATX heading consists of:

OK  a string of characters, parsed as inline content
OK       between an opening sequence of 1–6 unescaped # characters
OK  and an optional closing sequence of any number of unescaped # characters

OK  The opening sequence of # characters must be followed by a space
OK            or by the end of line.
OK  The optional closing sequence of #s must be preceded by a space
OK       and may be followed by spaces only.

OK  The opening # character may be indented 0-3 spaces.

OK  The raw contents of the heading are stripped of leading and trailing spaces before
OK  being parsed as inline content.

OK  The heading level is equal to the number of # characters in the opening sequence.")

(def atx-heading-re #"^ {0,3}(#{1,6})(?:$| \s*(\p{Print}*?)\s*)(?: #+ *)?$")

(defn atx-heading
  [line]
  (when-some [[_ opening content] (re-find atx-heading-re line)]
    {:level (count opening)
     :content (-> content (or "") (string/replace #"\\#" "#"))}))

(comment "Setext headings
    A setext heading consists of one or more lines of text, each containing at least one non-whitespace
    character, with no more than 3 spaces indentation, followed by a setext heading underline. The lines
    of text must be such that, were they not followed by the setext heading underline, they would be interpreted as
    a paragraph: they cannot be interpretable as a code fence, ATX heading, block quote, thematic
    break, list item, or HTML block.

OK  A setext heading underline is a sequence of = characters
OK       or a sequence of - characters,
OK       with no more than 3 spaces indentation
OK       and any number of trailing spaces.

    If a line containing a single - can be interpreted as an empty list item, it should be interpreted this way.

    The heading is a level 1 heading if = characters are used in the setext heading underline, and a level 2
    heading if - characters are used. The contents of the heading are the result of parsing the preceding lines of
    text as CommonMark inline content.

    In general, a setext heading need not be preceded or followed by a blank line. However, it cannot interrupt a
    paragraph, so when a setext heading comes after a paragraph, a blank line is needed between them.")

(def setext-heading-underline-re #"^ {0,3}(=+|-+) *$")

(comment "Thematic break

OK  A line consisting of 0-3 spaces of indentation,
OK       followed by a sequence of three or more matching -, _, or * characters,
OK       each followed optionally by any number of spaces or tabs, forms a thematic break.")

(def thematic-break-re #"^ {0,3}(?:(?:- *){3,}|(?:_ *){3,}|(?:[*] *){3,})\s*$")

(def thematic-break?
  (comp some? #(re-find thematic-break-re %)))

(comment "Indented code blocks

    An indented code block is composed of
    one or more indented chunks
    separated by blank lines.

    An indented chunk is a
         sequence of non-blank lines,
         each indented four or more spaces.

OK  The contents of the code block are the literal contents of the lines,
OK       including trailing line endings,
OK       minus four spaces of indentation.

OK  An indented code block has no info string.

    An indented code block cannot interrupt a paragraph, so there must be a blank line between a paragraph and a
    following indented code block. (A blank line is not needed, however, between a code block and a following
    paragraph.)")

(def indented-chunk-line-re #"^(?:\t| {4})(.*)$")

(def indented-chunk-line
  (comp second #(re-find indented-chunk-line-re %)))


(comment "Fenced code blocks

OK  A code fence is a sequence of at least three consecutive backtick characters (`)
OK  or tildes (~).
OK  Tildes and backticks cannot be mixed.
    A fenced code block begins with a code fence,
OK  indented no more than three spaces.

OK  The line with the opening code fence may optionally contain some text following the code fence;
OK  this is trimmed of leading and trailing whitespace and called the info string.
OK  If the info string comes after a backtick fence, it may not contain any backtick characters.

    The content of the code block consists of all subsequent lines,
    until a closing code fence of the same type as the code block began with (backticks or tildes),
    and with at least as many backticks or tildes as the opening code fence.

    If the leading code fence is indented N spaces,
         then up to N spaces of indentation are removed from each line of the content (if present).
    If a content line is not indented, it is preserved unchanged.
    If it is indented less than N spaces, all of the indentation is removed.

OK  The closing code fence may be indented up to three spaces,
OK  and may be followed only by spaces,
OK  which are ignored.

    If the end of the containing block (or document) is reached and no closing code fence has been found,
    the code block contains all of the lines after the opening code fence until the end of the containing block (or
    document).

    A fenced code block may interrupt a paragraph, and does not require a blank line either before or after.

    The content of a code fence is treated as literal text, not parsed as inlines.")

(def opening-code-fence-re #"^( {0,3})((`{3,})\s*([^`]*?)|(~{3,})\s*(\p{Print}*?))\s*$")

(defn opening-code-fence
  [line]
  (when-some [[_ indent _ backtick-fence backtick-info
               tilde-fence tilde-info] (re-find opening-code-fence-re line)]
    {:indent indent
     :info (or backtick-info tilde-info)
     :fence (or backtick-fence tilde-fence)}))

(def closing-code-fence-re #"^( {0,3})(`{3,}|~{3,}) *$")

(defn closing-code-fence
  [line]
  (when-some [[_ indent backtick-fence tilde-fence] (re-find closing-code-fence-re line)]
    {:indent indent
     :fence (or backtick-fence tilde-fence)}))

(comment "Paragraphs

    A sequence of
OK  non-blank lines
    that cannot be interpreted as other kinds of blocks
    forms a paragraph.
    The contents of the paragraph are the result of parsing the paragraph’s raw content as inlines.
    The paragraph’s raw content is formed by concatenating the lines
OK  and removing initial and final whitespace.")

(def paragraph-line-re #"^\s*(\S+)\s*$")

(defn paragraph-line
  [line]
  (some->> line
           (re-find paragraph-line-re)
           second))

(def blank-line-re #"^$")

(def blank-line?
  string/blank?)

(comment "List items

OK  A list marker is a bullet list marker or an ordered list marker.

OK  A bullet list marker is a -, +, or * character.

OK  An ordered list marker is a sequence of 1–9 arabic digits (0-9),
OK  followed by either a . character or a ) character.

    The following rules define list items:
    1. Basic case. If a sequence of lines Ls constitute a sequence of blocks Bs
       starting with a non-whitespace character,
       and M is a list marker of width W followed by 1 ≤ N ≤ 4 spaces,
       then the result of prepending M and the following spaces to the first line of Ls,
       and indenting subsequent lines of Ls by W + N spaces,
       is a list item with Bs as its contents.
       The type of the list item (bullet or ordered) is determined by the type of its list marker.
       If the list item is ordered, then it is also assigned a start number,
       based on the ordered list marker.

       Exceptions:
       1. When the first list item in a list interrupts a paragraph, i.e. starts on a line that
            would otherwise count as paragraph continuation text,
          then (a) the lines Ls must not begin with a blank line,
          and (b) if the list item is ordered, the start number must be 1.
       2. If any line is a thematic break then that line is not a list item.

    2. Item starting with indented code.
       If a sequence of lines Ls constitute a sequence of blocks Bs
       starting with an indented code block,
       and M is a list marker of width W followed by one space,
       then the result of prepending M and the following space to the first line of Ls,
       and indenting subsequent lines of Ls by W + 1 spaces, is a list item with Bs as its contents.
       If a line is empty, then it need not be indented.
       The type of the list item is determined by the type of its list marker.
       If the list item is ordered, then it is also assigned a start number, based on the ordered list marker.

       An indented code block will have to be indented four spaces
       beyond the edge of the region where text will be included in the list item.

    3. Item starting with a blank line.
       If a sequence of lines Ls starting with a single blank line
       constitute a (possibly empty) sequence of blocks Bs,
       not separated from each other by more than one blank line,
       and M is a list marker of width W,
       then the result of prepending M to the first line of Ls,
       and indenting subsequent lines of Ls by W + 1 spaces,
       is a list item with Bs as its contents.
       If a line is empty, then it need not be indented.
       The type of the list item is determined by the type of its list marker.
       If the list item is ordered, then it is also assigned a start number, based on the ordered list marker.

    4. Indentation.
       If a sequence of lines Ls constitutes a list item according to rule #1, #2, or #3,
       then the result of indenting each line of Ls by 1-3 spaces (the same for each line)
       also constitutes a list item with the same contents and attributes.
       If a line is empty, then it need not be indented.

    5. Laziness.
       If a string of lines Ls constitute a list item with contents Bs,
       then the result of deleting some
       or all of the indentation
       from one
       or more lines
       in which the next non-whitespace character after the indentation is paragraph continuation text
       is a list item with the same contents and attributes.
       The unindented lines are called lazy continuation lines.")

(def list-item-marker-re #"( {0,3})([-+*]|\d{1,9}[.)])")

(def list-item-basic-lead-line-re (re-pattern (str "^" list-item-marker-re #"( {1,4})(\S\p{Print}*)$")))

(def list-item-indented-code-lead-line-re (re-pattern (str "^" list-item-marker-re #"( )(?= {4,})(\p{Print}*)$")))

(def list-item-blank-lead-line-re (re-pattern (str "^" list-item-marker-re #" *$")))

(defn list-item-lead-line
  [line]
  (some->> line
           ((some-fn #(re-find list-item-basic-lead-line-re %)
                     #(re-find list-item-indented-code-lead-line-re %)
                     #(re-find list-item-blank-lead-line-re %)))
           (drop 1)
           (zipmap [:indent :marker :space :content])))

