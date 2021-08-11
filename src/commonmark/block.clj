(ns commonmark.block
  (:require [clojure.string :as string]
            [clojure.set]
            [flatland.useful.fn :as ufn]
            [commonmark.re.block :as re.block]
            [commonmark.re.link :as re.link]
            [commonmark.util :as util]))

(defn atx-heading
  [line]
  (when-some [[_ opening content] (re-find re.block/atx-heading line)]
    {:tag :atxh
     :level (count opening)
     :content (-> content (or "") (string/replace #"\\#" "#"))}))

(defn setext-heading
  [line]
  (when-some [[_ underline] (re-find re.block/setext-heading-underline line)]
    {:tag :stxh
     :level (if (string/starts-with? underline "=") 1 2)}))

(defn thematic-break
  [line]
  (when (re-find re.block/thematic-break line)
    {:tag :tbr}))

(defn indented-chunk-line
  [line]
  (when-some [[_ content] (re-find re.block/indented-chunk-line line)]
    {:tag :icblk
     :content content}))

(defn opening-code-fence
  [line]
  (when-some [[_ indent _ backtick-fence
               backtick-info tilde-fence
               tilde-info] (re-find re.block/opening-code-fence line)]
    {:tag :ofcblk
     :indent indent
     :info (or backtick-info tilde-info)
     :fence (or backtick-fence tilde-fence)}))

(defn closing-code-fence
  [line]
  (when-some [[_ indent backtick-fence
               tilde-fence] (re-find re.block/closing-code-fence line)]
    {:tag :cfcblk
     :indent indent
     :fence (or backtick-fence tilde-fence)}))

(defn paragraph-line
  [line]
  (when-some [[_ content break] (re-find re.block/paragraph-line line)]
    {:tag :p
     :content (str content break)}))

(defn blank-line
  [line]
  (when (string/blank? line)
    {:tag :blank}))

(defn list-item-lead-line
  [line]
  (some->> line
           ((some-fn #(re-find re.block/list-item-basic-lead-line %)
                     #(re-find re.block/list-item-indented-code-lead-line %)
                     #(re-find re.block/list-item-blank-lead-line %)))
           (drop 1)
           (concat [:li])
           (zipmap [:tag :indent :marker :space :content])))

(defn blockquote-line
  [line]
  (some->> line
           (re-find re.block/blockquote-line)
           (drop 1)
           (concat [:bq])
           (zipmap [:tag :indent :space :content])))

(defn html-block-begin
  [line]
  (when line
    (let [re-info {re.block/html-block-variant-1-begin-line 1
                   re.block/html-block-variant-2-begin-line 2
                   re.block/html-block-variant-3-begin-line 3
                   re.block/html-block-variant-4-begin-line 4
                   re.block/html-block-variant-5-begin-line 5
                   re.block/html-block-variant-6-begin-line 6
                   re.block/html-block-variant-7-begin-line 7}]
      (->> (keys re-info)
           (filter #(re-find % line))
           (map re-info)
           set
           (hash-map :content line :variant)
           ((ufn/validator (comp not-empty :variant)))))))

(defn html-block-end
  [line]
  (when line
    (let [re-info {re.block/html-block-variant-1-end-line 1
                   re.block/html-block-variant-2-end-line 2
                   re.block/html-block-variant-3-end-line 3
                   re.block/html-block-variant-4-end-line 4
                   re.block/html-block-variant-5-end-line 5
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

(defn link-reference-definition
  [line]
  (when line
    (if-some [[_ label wrapped unwrapped
               single-quoted double-quoted
               parenthesized] (re-find re.link/reference-definition line)]
      (->> {:tag :adef
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
              blockquote-line
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
    (util/trim-leading-whitespace current
                                  (-> (str indent marker space)
                                      util/expand-tab
                                      count))))

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

(defn strip-containers
  "Recursively extracts content from list item and blockquote lines until only
   leaf content remains."
  [line]
  (loop [x line]
    (if-some [{:keys [content]} ((some-fn blockquote-line list-item-lead-line) x)]
      (recur content)
      x)))

(defn paragraph-continuation-text?
  "True if all of the following apply:
     1. current is paragraph continuation text
     2. previous is a collection of lines preceding current
     3. previous comprise a paragraph
   False otherwise.
   The paragraph may be nested in an arbitrarily deep series of containers."
  [current previous]
  (let [current-ok? (->> current tagger :tag (contains? #{:p :icblk}))
        head (strip-containers (last previous))
        previous-tag (:tag (tagger head))
        previous-ok? (= :p previous-tag)]
    (case (count previous)
      0 false
      1 (and current-ok? previous-ok?)
      (and current-ok?
           (or previous-ok?
               (paragraph-continuation-text? head (butlast previous)))))))

(defn belongs-to-list-item?
  "True if current belongs to LI, assuming:
     1. current is the line in question
     2. previous is a vector of lines already belonging to LI
   False otherwise."
  [current previous]
  (when-some [origin (first previous)]
    (when-some [{:keys [indent marker space content]
                 :or {space " "}} (list-item-lead-line origin)]
      (let [prefix (-> (str indent marker space) count (repeat " ") string/join)
            starts-with? (comp (ufn/ap string/starts-with?)
                               #(map util/expand-tab %)
                               vector)
            blank? (blank-line current)]
        (or (starts-with? current prefix)
            (paragraph-continuation-text? current previous)
            (and blank? (or (some? content)
                            (> (count previous) 1))))))))

(defn belongs-to-blockquote?
  [current previous]
  (or (->> current tagger :tag (= :bq))
      (paragraph-continuation-text? current previous)))

