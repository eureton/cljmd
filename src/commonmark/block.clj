(ns commonmark.block
  (:require [clojure.string :as string]
            [clojure.set]
            [flatland.useful.fn :as ufn]
            [commonmark.html :as html]
            [commonmark.inline :as inline]))

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
    {:tag :atxh
     :level (count opening)
     :content (-> content (or "") (string/replace #"\\#" "#"))}))

(comment "Setext headings
OK  A setext heading consists of one or more lines of text,
OK  each containing at least one non-whitespace character,
OK  with no more than 3 spaces indentation,
OK  followed by a setext heading underline.
OK  The lines of text must be such that, were they not followed by the setext heading underline,
OK  they would be interpreted as a paragraph:
OK  they cannot be interpretable as a code fence, ATX heading, block quote, thematic break, list item, or HTML block.

OK  A setext heading underline is a sequence of = characters
OK       or a sequence of - characters,
OK       with no more than 3 spaces indentation
OK       and any number of trailing spaces.

    If a line containing a single - can be interpreted as an empty list item, it should be interpreted this way.

OK  The heading is a level 1 heading if = characters are used in the setext heading underline,
OK  and a level 2 heading if - characters are used.
    The contents of the heading are the result of parsing the preceding lines of text as CommonMark inline content.

OK  In general, a setext heading need not be preceded or followed by a blank line.
OK  However, it cannot interrupt a paragraph, so when a setext heading comes after a paragraph,
OK  a blank line is needed between them.")

(def setext-heading-underline-re #"^ {0,3}(=+|-+) *$")

(defn setext-heading
  [line]
  (when-some [[_ underline] (re-find setext-heading-underline-re line)]
    {:tag :stxh
     :level (if (string/starts-with? underline "=") 1 2)}))

(comment "Thematic break

OK  A line consisting of 0-3 spaces of indentation,
OK       followed by a sequence of three or more matching -, _, or * characters,
OK       each followed optionally by any number of spaces or tabs, forms a thematic break.")

(def thematic-break-re #"^ {0,3}(?:(?:- *){3,}|(?:_ *){3,}|(?:[*] *){3,})\s*$")

(defn thematic-break
  [line]
  (when (re-find thematic-break-re line)
    {:tag :tbr}))

(comment "Indented code blocks

OK  An indented code block is composed of
OK  one or more indented chunks
OK  separated by blank lines.

OK  An indented chunk is a
OK       sequence of non-blank lines,
OK       each indented four or more spaces.

OK  The contents of the code block are the literal contents of the lines,
OK       including trailing line endings,
OK       minus four spaces of indentation.

OK  An indented code block has no info string.

OK  An indented code block cannot interrupt a paragraph,
OK  so there must be a blank line between a paragraph and a following indented code block.
OK  A blank line is not needed, however, between a code block and a following paragraph.")

(def indented-chunk-line-re #"^(?:\t| {4})(.*)$")

(defn indented-chunk-line
  [line]
  (when-some [[_ content] (re-find indented-chunk-line-re line)]
    {:tag :icblk
     :content content}))


(comment "Fenced code blocks

OK  A code fence is a sequence of at least three consecutive backtick characters (`)
OK  or tildes (~).
OK  Tildes and backticks cannot be mixed.
    A fenced code block begins with a code fence,
OK  indented no more than three spaces.

OK  The line with the opening code fence may optionally contain some text following the code fence;
OK  this is trimmed of leading and trailing whitespace and called the info string.
OK  If the info string comes after a backtick fence, it may not contain any backtick characters.

OK  The content of the code block consists of all subsequent lines,
OK  until a closing code fence of the same type as the code block began with (backticks or tildes),
OK  and with at least as many backticks or tildes as the opening code fence.

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

OK  A fenced code block may interrupt a paragraph,
OK  and does not require a blank line either before or after.

    The content of a code fence is treated as literal text, not parsed as inlines.")

(def opening-code-fence-re #"^( {0,3})((`{3,})\s*([^`]*?)|(~{3,})\s*(\p{Print}*?))\s*$")

(defn opening-code-fence
  [line]
  (when-some [[_ indent _ backtick-fence backtick-info
               tilde-fence tilde-info] (re-find opening-code-fence-re line)]
    {:tag :ofcblk
     :indent indent
     :info (or backtick-info tilde-info)
     :fence (or backtick-fence tilde-fence)}))

(def closing-code-fence-re #"^( {0,3})(`{3,}|~{3,}) *$")

(defn closing-code-fence
  [line]
  (when-some [[_ indent backtick-fence tilde-fence] (re-find closing-code-fence-re line)]
    {:tag :cfcblk
     :indent indent
     :fence (or backtick-fence tilde-fence)}))

(comment "Paragraphs

    A sequence of
OK  non-blank lines
OK  that cannot be interpreted as other kinds of blocks
OK  forms a paragraph.
    The contents of the paragraph are the result of parsing the paragraph’s raw content as inlines.
    The paragraph’s raw content is formed by concatenating the lines
OK  and removing initial and final whitespace.")

(def paragraph-line-re #"^\s*(.*?)\s*$")

(defn paragraph-line
  [line]
  (when-some [[_ content] (re-find paragraph-line-re line)]
    {:tag :p
     :content content}))

(defn blank-line
  [line]
  (when ((every-pred string? empty?) line)
    {:tag :blank}))

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
           (concat [:li])
           (zipmap [:tag :indent :marker :space :content])))

(comment "Block quotes

    A block quote marker consists of 0-3 spaces of initial indent, plus (a) the character > together with a
    following space, or (b) a single character > not followed by a space.

    The following rules define block quotes:
      1. Basic case. If a string of lines Ls constitute a sequence of blocks Bs, then the result of prepending a
         block quote marker to the beginning of each line in Ls is a block quote containing Bs.
      2. Laziness. If a string of lines Ls constitute a block quote with contents Bs, then the result of
         deleting the initial block quote marker from one or more lines in which the next non-whitespace
         character after the block quote marker is paragraph continuation text is a block quote with
         Bs as its content. Paragraph continuation text is text that will be parsed as part of the content of a
         paragraph, but does not occur at the beginning of the paragraph.
      3. Consecutiveness. A document cannot contain two block quotes in a row unless there is a blank line 
         between them.

      Nothing else counts as a block quote.")

(def block-quote-marker-re #"( {0,3})>( ?)")

(def block-quote-line-re (re-pattern (str "^" block-quote-marker-re #"(\p{Print}*)$")))

(defn block-quote-line
  [line]
  (some->> line
           (re-find block-quote-line-re)
           (drop 1)
           (concat [:bq])
           (zipmap [:tag :indent :space :content])))

(def html-block-variant-1-tag-re #"(?:(?i)script|pre|style)")

(def html-block-variant-1-begin-line-re
  (re-pattern (str #"^ {0,3}(?<!\\)<" html-block-variant-1-tag-re #"(?:\s|>|$).*")))

(def html-block-variant-2-begin-line-re
  (re-pattern (str #"^ {0,3}" html/comment-begin-re ".*")))

(def html-block-variant-3-begin-line-re
  (re-pattern (str #"^ {0,3}" html/processing-instruction-begin-re ".*")))

(def html-block-variant-4-begin-line-re
  (re-pattern (str #"^ {0,3}" html/declaration-begin-re ".*")))

(def html-block-variant-5-begin-line-re
  (re-pattern (str #"^ {0,3}" html/cdata-section-begin-re ".*")))

(def html-block-variant-6-begin-line-re
  (re-pattern (str #"^ {0,3}(?<!\\)</?"
                   "(?:(?i)" (string/join "|" html/block-variant-6-tags) ")"
                   #"(?:\s+|/?>|$).*")))

(def html-block-variant-7-begin-line-re
  (re-pattern (str "^ {0,3}"
                   "(?:"
                     (html/open-tag-re {:exclude-tags ["script" "style" "pre"]}) "|"
                     html/closing-tag-re
                   ")"
                   #"\s*$")))

(defn html-block-begin
  [line]
  (when line
    (let [re-info {html-block-variant-1-begin-line-re 1
                   html-block-variant-2-begin-line-re 2
                   html-block-variant-3-begin-line-re 3
                   html-block-variant-4-begin-line-re 4
                   html-block-variant-5-begin-line-re 5
                   html-block-variant-6-begin-line-re 6
                   html-block-variant-7-begin-line-re 7}]
      (->> (keys re-info)
           (filter #(re-find % line))
           (map re-info)
           set
           (hash-map :content line :variant)
           ((ufn/validator (comp not-empty :variant)))))))

(def html-block-variant-1-end-line-re
  (re-pattern (str #"^ {0,3}(?! ).*?(?<!\\)</" html-block-variant-1-tag-re #">.*")))

(def html-block-variant-2-end-line-re
  (re-pattern (str #"^ {0,3}(?! ).*?" html/comment-end-re ".*")))

(def html-block-variant-3-end-line-re
  (re-pattern (str #"^ {0,3}(?! ).*?" html/processing-instruction-end-re ".*")))

(def html-block-variant-4-end-line-re
  (re-pattern (str #"^ {0,3}(?! ).*?" html/declaration-end-re ".*")))

(def html-block-variant-5-end-line-re
  (re-pattern (str #"^ {0,3}(?! ).*?" html/cdata-section-end-re ".*")))

(defn html-block-end
  [line]
  (when line
    (let [re-info {html-block-variant-1-end-line-re 1
                   html-block-variant-2-end-line-re 2
                   html-block-variant-3-end-line-re 3
                   html-block-variant-4-end-line-re 4
                   html-block-variant-5-end-line-re 5
                   #"^\s*$" [6 7]}]
      (->> (keys re-info)
           (filter #(re-find % line))
           (map re-info)
           flatten
           set
           (hash-map :content line :variant)
           ((ufn/validator (comp not-empty :variant)))))))

(defn html-block-pair?
  "Returns true if line1 is the beginning of an HTML block and line2 is the end
   of an HTML block of the same variant, false otherwise."
  [line1 line2]
  (->> [line1 line2]
       ((ufn/knit html-block-begin html-block-end))
       (map :variant)
       (reduce clojure.set/intersection)
       not-empty))

(defn html-block
  [line]
  (let [begin (html-block-begin line)
        end (html-block-end line)
        info (or begin end)
        pair? (and begin end (html-block-pair? line line))]
    (cond
      pair? (assoc info :tag :html-block)
      info (assoc info :tag :html-block-unpaired))))

(def link-reference-definition-re
  (re-pattern (str #" {0,3}\[" "(" inline/link-label-re ")" #"\]:"
                   #"\s*(?=\S+(?:\s|$))" inline/inline-link-destination-re
                   "(?:" #"\s+" inline/inline-link-title-re ")?"
                   #"\s*")))

(def link-reference-definition-batch-re
  (re-pattern (str #"(?m)(?s)\A"
                   "(?:^" link-reference-definition-re "$"
                          inline/line-ending-re "?"
                   ")*")))

(defn link-reference-definition
  [line]
  (when line
    (if-some [[_ label wrapped unwrapped
               single-quoted double-quoted
               parenthesized] (re-find link-reference-definition-re line)]
      (->> {:tag :aref
            :label label
            :destination (or wrapped unwrapped)
            :title (or single-quoted double-quoted parenthesized)}
           (remove (comp nil? val))
           flatten
           (apply hash-map)))))

(defn tagger
  [line]
  (when line
    ((some-fn thematic-break
              atx-heading
              list-item-lead-line
              block-quote-line
              setext-heading
              indented-chunk-line
              opening-code-fence
              closing-code-fence
              blank-line
              html-block
              paragraph-line) line)))

(defn list-item-content
  "Returns the current line with as much leading whitespace trimmed, as
   corresponds to the indentation, marker and spacing of the origin line."
  [current origin]
  (when-some [{:keys [indent marker space]
               :or {space " "}} (list-item-lead-line origin)]
    (let [prefix-length (->> [indent marker space] (map count) (reduce +))
          trim-re (re-pattern (str "^ {0," prefix-length "}"))]
      (string/replace current trim-re ""))))

(defn fenced-code-block-pair?
  "True if lines x and y are matching code block fences, false otherwise."
  [x y]
  (let [x (tagger x)
        y (tagger y)]
    (and (every? (comp #{:ofcblk :cfcblk} :tag) [x y])
         (string/blank? (:info y))
         (->> [y x]
              (map :fence)
              (apply string/includes?)))))

(defn lazy-continuation-line?
  "Returns non-nil if current is a lazy continuation line of previous."
  [current previous]
  (let [{:keys [tag content]
         :or {content ""}} (tagger previous)]
    (and (= :p (:tag (tagger current)))
         (or (= :p tag)
             (and (= :li tag)
                  (= :p (:tag (tagger content))))))))

(defn paragraph-continuation-text?
  [current previous]
  (let [current-tag (->> current tagger :tag)
        head (first previous)
        tail (rest previous)
        {previous-tag :tag :keys [content]} (tagger head)]
    (boolean
      (and (#{:p :icblk} current-tag)
           (or (= :p previous-tag)
               (and (= :icblk previous-tag)
                    (paragraph-continuation-text? head tail))
               (and (#{:li :bq} previous-tag)
                    (paragraph-continuation-text? current
                                                  (concat [content] tail))))))))

(defn belongs-to-list-item?
  "True if current belongs to LI, assuming:
     1. current is the line in question
     2. previous is the line preceding current
     3. LI is a list item, whose first line is origin
   False otherwise."
  [current previous origin]
  (when-some [{:keys [indent marker space]
               :or {space " "}} (list-item-lead-line origin)]
    (let [prefix (string/replace (str indent marker space) #"."  " ")
          trim-re (re-pattern (str "^ {0," (count prefix) "}"))
          ; TODO refactor this to use list-item-content
          previous (string/replace previous trim-re "")]
      (or (string/starts-with? current prefix)
          (lazy-continuation-line? current previous)
          (= :blank (->> current tagger :tag))))))

(defn belongs-to-block-quote?
  [current previous]
  (or (->> current tagger :tag (= :bq))
      (paragraph-continuation-text? current previous)))

