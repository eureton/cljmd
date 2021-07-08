(ns commonmark.blockrun
  (:require [clojure.string :as string]
            [commonmark.block :as block]
            [commonmark.blockrun.entry :as entry]))

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
        [destination source] (case direction
                               :ltor [y-block x-block]
                               :rtol [x-block y-block])
        mid (some-> destination
                    (update 1 #(apply conj %1 %2) (second source))
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
                                             (-> y first second first))
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
  (let [list-item-lines (->> x last second)
        origin (first list-item-lines)
        lines (->> y first second)
        previous-lines (concat [(last list-item-lines)] lines)
        n (->> (map vector lines previous-lines)
               (take-while #(apply block/belongs-to-list-item? (conj % origin)))
               count)]
    (fuse-split x y n)))

(defmethod add [:bq :_]
  [x y]
  (if (block/belongs-to-block-quote? (->> y first second first)
                                     (->> x last second reverse))
    (fuse-left x y)
    (concat x y)))

(defmethod add [:p :tbr]
  [x y]
  (if (some? (block/setext-heading (->> y first second first)))
    (fuse-split (retag x :last :stxh) y 1)
    (concat x y)))

(defmethod add [:p :li]
  [x y]
  (if (->> y first second first (re-find block/list-item-blank-lead-line-re) some?)
    (fuse-split (retag x :last :stxh) y 1)
    (concat x y)))

(defmethod add [:p :stxh]
  [x y]
  (fuse-left (retag x :last :stxh) y))

(defmethod add [:p :icblk]
  [x y]
  (fuse-left x y))

(defmethod add [:blank :icblk]
  [x y]
  (fuse-right x y))

(defmethod add [:icblk :blank]
  [x y]
  (fuse-left x y))

(defmethod add [:html-block-unpaired :html-block-unpaired]
  [x y]
  (if (->> [(last x) (first y)]
           (map entry/origin)
           (apply block/html-block-pair?))
    (fuse-split (retag x :last :html-block) y 1)
    (fuse-split x y 1)))

(defmethod add [:html-block-unpaired :_]
  [x y]
  (let [origin (->> x last entry/origin)]
    (cond
      (block/html-block-pair? origin origin) (concat x y)
      ; TODO clean this up
      (and (->> y first first (= :bq))
           (->> y first second first block/tagger :content string/blank?)) (fuse-split (retag x :last :html-block) y 1)
      :else (add (fuse-left x (take 1 y))
                 (rest y)))))

(defmethod add :default
  ([] zero)
  ([x] x)
  ([x y]
   (let [x-entry (last x)
         y-entry (first y)
         x-tag (first x-entry)
         y-tag (first y-entry)
         left-handler ((methods add) [x-tag :_])]
     (cond
       (empty? y)      x
       left-handler    (left-handler x y)
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
  "Merges adjacent entries of the same type."
  [blockrun]
  (reduce (fn [acc x]
            (let [left (last acc)]
              (if (= (first left) (first x))
                (-> acc pop (conj (update left 1 (comp vec concat) (second x))))
                (conj acc x))))
          []
          blockrun))

(defn postprocess
  "Hook for performing transformations after the blockrun has been compiled."
  [blockrun]
  (->> blockrun
       (map entry/promote)
       coalesce))

(defn from-string
  "Parses the given input into a list of blockrun entries. Each of these entries
   represents a top-level block."
  [string]
  (->> string
       tokenize
       (map from-line)
       (reduce add)))

