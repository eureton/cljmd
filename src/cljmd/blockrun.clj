(ns cljmd.blockrun
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [cljmd.block :as block]
            [cljmd.blockrun.entry :as entry]
            [cljmd.re.link :as re.link]
            [cljmd.re.block :as re.block]
            [cljmd.ast.list :as ast.list]
            [cljmd.util :as util]))

(def zero
  "Identity element of the add binary operation."
  [])

(defn add-df
  "Dispatch function for blockrun/add."
  ([] :default)
  ([x] :default)
  ([x y]
   [(->> x last first)
    (->> y first first)]))

(defmulti add
  "Binary operation on blockruns. Serves to compose blockruns."
  #'add-df)

(defn from-line
  "Returns a blockrun containing a single tagged line."
  [line]
  [[(-> line block/tagger :tag)
    [line]]])

(defn retag
  "Returns the blockrun with either its first or its last entry retagged to tag.
   The entry to be affected is specified by position and may be either :first or
   :last."
  [blockrun position tag]
  (let [positionf (case position
                    :last last
                    :first first)]
    (concat (butlast blockrun)
            [(assoc (positionf blockrun) 0 tag)])))

(defn shift
  "Recalculates blockrun after removing the first n lines of the first entry."
  [n blockrun]
  (->> (rest blockrun)
       (map second)
       (concat (->> blockrun first second (drop n)))
       flatten
       (map from-line)
       (reduce add)))

(defn fuse
  "Concatenates blockruns x and y with the exception of their adjoining entries.
   The latter are merged into a single entry, according to direction.
     * if direction equals :rtol
       y-side lines are appended to x-side lines under the x-side tag
     * if direction equals :ltor
       x-side lines are appended to y-side lines under the y-side tag"
  [x y direction]
  (let [x-block (last x)
        y-block (first y)
        destination (case direction
                      :ltor y-block
                      :rtol x-block)
        mid (some-> destination
                    (assoc 1 (apply conj (second x-block) (second y-block)))
                    vector)]
    (concat (butlast x)
            mid
            (rest y))))

(defn fuse-left
  "Same as (fuse x y :rtol)"
  [x y]
  (fuse x y :rtol))

(defn fuse-right
  "Same as (fuse x y :ltor)"
  [x y]
  (fuse x y :ltor))

(defn fuse-split
  "Removes the first n lines of the first entry of y, appends them to the last
   entry of x, recalculates the remainder of y and concatenates the results."
  [x y n]
  (concat (butlast x)
          (fuse-left [(last x)]
                     [(update (first y) 1 #(take n %))])
          (shift n y)))

(defmethod add [:ofcblk :_]
  [x y]
  (let [block-lines (->> x last second)
        pair? (block/fenced-code-block-pair? (first block-lines)
                                             (-> y first entry/origin))
        closed? (and (> (count block-lines) 1)
                     (->> block-lines
                          ((juxt first last))
                          (apply block/fenced-code-block-pair?)))]
    (cond
      pair?   (fuse-split x y 1)
      closed? (concat x y)
      :else   (fuse-left x y))))

(defmethod add [:li :_]
  [x y]
  (let [x-lines (->> x last second)
        y-lines (mapcat second y)
        belongs? (fn [[index current]]
                   (block/belongs-to-list-item? current
                                                (->> y-lines
                                                     (take index)
                                                     (concat x-lines))))
        n (->> (map-indexed vector y-lines)
               (take-while belongs?)
               count)]
    (fuse-split x [[:_ y-lines]] n)))

(defmethod add [:bq :_]
  [x y]
  (if (block/belongs-to-blockquote? (->> y first entry/origin)
                                    (->> x last second))
    (fuse-left x y)
    (concat x y)))

(defmethod add [:p :tbr]
  [x y]
  (if (-> y first entry/origin block/setext-heading some?)
    (fuse-split (retag x :last :stxh) y 1)
    (concat x y)))

(defmethod add [:tbr :tbr]
  [x y]
  (concat x y))

(defmethod add [:p :li]
  [x y]
  (let [{:keys [marker content]} (->> y first entry/origin block/list-item-lead-line)
        stxh? (and (nil? content)
                   (= marker "-"))
        p? (or (and (nil? content)
                    (not= marker "-"))
               (->> marker
                    ast.list/start
                    ((every-pred some? #(not= % "1")))))]
    (cond stxh? (fuse-split (retag x :last :stxh) y 1)
          p?    (fuse-split x y 1)
          :else (concat x y))))

(defmethod add [:p :stxh]
  [x y]
  (fuse-left (retag x :last :stxh) y))

(defmethod add [:p :icblk]
  [x y]
  (fuse-left x y))

(defmethod add [:p :html-block-unpaired]
  [x y]
  (if (->> y
           first
           entry/origin
           ((juxt block/html-block-begin block/html-block-end))
           (some (comp #(= % #{7}) :variant)))
    (fuse-left x y)
    (concat x y)))

(defmethod add [:html-block-unpaired :html-block-unpaired]
  [x y]
  (if (->> [(last x) (first y)]
           (map entry/origin)
           (apply block/html-block-pair?))
    (fuse-split (retag x :last :html-block) y 1)
    (fuse-split x y 1)))

(defmethod add [:html-block-unpaired :bq]
  [x y]
  (let [origin-x (->> x last entry/origin)
        origin-y (->> y first entry/origin)]
    (if (block/html-block-pair? origin-x origin-y)
      (fuse-split (retag x :last :html-block) y 1)
      (concat x y))))

(defmethod add [:html-block-unpaired :blank]
  [x y]
  (let [origin-x (->> x last entry/origin)
        origin-y (->> y first entry/origin)
        pair? (block/html-block-pair? origin-x origin-y)
        x-opens? (some? (block/html-block-begin origin-x))]
    (cond pair?    (concat (retag x :last :html-block) y)
          x-opens? (fuse-left x y)
          :else    (concat x y))))

(defmethod add [:html-block-unpaired :_]
  [x y]
  (let [x-begins? (->> x last entry/origin block/html-block-begin some?)
        y-tbr? (-> y first first (= :tbr))]
    (cond x-begins? (add (fuse-left x (take 1 y))
                         (rest y))
          y-tbr?    (fuse-split (retag x :last :stxh) y 1)
          :else     (concat x y))))

(defmethod add [:atxh :atxh]
  [x y]
  (concat x y))

(defmethod add :default
  ([] zero)
  ([x] x)
  ([x y]
   (let [x-entry (last x)
         y-entry (first y)
         x-tag (first x-entry)
         y-tag (first y-entry)
         method-map (methods add)
         left-handler (method-map [x-tag :_])
         right-handler (method-map [:_ y-tag])]
     (cond
       (empty? y)      x
       left-handler    (left-handler x y)
       right-handler   (right-handler x y)
       (= x-tag y-tag) (fuse-left x y)
       :else           (concat x y)))))

(defn tokenize
  [string]
  (let [lines (string/split (str "_" string "_") #"(?:\r\n|\r|\n)")]
    (if (= 1 (count lines))
      [string]
      (->> lines
           ((juxt (comp list #(subs % 1) first)
                  (comp rest butlast)
                  (comp list #(subs % 0 (dec (count %))) last)))
           (reduce concat)))))

(defn coalesce
  "Tests blockrun entries in pairs and concatenates those for which all of the
   following apply:
    1. are adjacent
    2. bear the same tag
    3. (pred t) returns logical true, where t is the tag of the entries"
  [pred blockrun]
  (util/coalesce #(let [tag (first %2)]
                    (and (= tag (-> %1 peek peek first))
                         (pred tag)))
                 #(update %1 1 (comp vec concat) (second %2))
                 blockrun))

(defn merge-indented-chunks
  "Merges entries of indented code which are separated by blanks."
  [blockrun]
  (let [merge? (comp #(= % [:icblk :blank :icblk])
                     #(map (comp first second) %))
        indexed (->> blockrun
                     (coalesce #{:blank :icblk})
                     (map-indexed vector))
        mergee-indices (->> indexed
                            (partition 3 1)
                            (filter merge?)
                            (map #(map first (rest %)))
                            flatten
                            set)]
    (reduce (fn [acc [i x]]
              (if (contains? mergee-indices i)
                (apply update acc (dec (count acc)) update 1 conj (second x))
                (conj acc x)))
            []
            indexed)))

(defn extract-trailing-blanks
  "Breaks blank lines off from the tail end of :li entries into a separate
   :blank entity."
  [blockrun]
  (let [split (fn [[_ lines]]
                (let [blanks (->> lines reverse (take-while block/blank-line))]
                  (cond-> [[:li (vec (drop-last (count blanks) lines))]]
                    (not-empty blanks) (conj [:blank (vec blanks)]))))]
    (->> blockrun
         (mapcat (ufn/to-fix (comp #{:li} first) split
                                                 vector))
         vec)))

(defn extract-link-reference-definitions
  "Searches blockrun for link reference definitions and extracts them into
   separate entries. Each definition is awarded its own entry. The new entries
   are tagged :adef. The entries which the definitions came from are split and
   each of the parts bears the tag of its originator."
  [blockrun]
  (let [split? (comp #{:p :stxh} first)
        split #(let [[tag lines] %
                     batch (entry/link-reference-definition-batch %)
                     items (->> batch
                                (string/join "\r\n")
                                (re-seq re.link/reference-definition)
                                (map (comp string/split-lines first)))
                     remainder (->> lines (drop (count batch)) vec)]
                 (concat
                   (map vector (repeat :adef) items)
                   (if (not-empty remainder)
                     [(entry/promote [tag remainder])]
                     [])))]
    (->> blockrun
         (coalesce #{:p})
         (mapcat (ufn/to-fix split? split vector))
         vec)))

(defn postprocess
  "Hook for performing transformations after the blockrun has been compiled."
  [blockrun]
  (->> blockrun
       merge-indented-chunks
       (map entry/promote)
       extract-link-reference-definitions
       extract-trailing-blanks
       (coalesce (comp nil? #{:tbr :adef :li :atxh :stxh}))))

(defn from-string
  "Parses the given input into a list of blockrun entries. Each of these entries
   represents a top-level block."
  [string]
  (when string
    (->> string
         tokenize
         (map from-line)
         (reduce add))))

