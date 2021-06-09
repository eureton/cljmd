(ns commonmark.tagmap
  (:require [clojure.string :as string]
            [commonmark.block :as block]))

(def zero
  "Identity element of the add binary operation."
  [])

(defn add-df
  "Dispatch function for tagmap/add."
  ([] :default)
  ([x] :default)
  ([x y]
   (let [x-entry (last x)
         y-entry (first y)
         x-tag (first x-entry)
         y-tag (first y-entry)
         fence-pair? (block/fenced-code-block-pair? (-> x-entry second first)
                                                    (-> y-entry second first))
         result (cond
                  (= [:p :tbr] [x-tag y-tag])       [:p :tbr]
                  (= [:p :li] [x-tag y-tag])        [:p :li]
                  (= [:p :stxh] [x-tag y-tag])      [:p :stxh]
                  (= [:p :icblk] [x-tag y-tag])     [:p :icblk]
                  (= [:blank :icblk] [x-tag y-tag]) [:blank :icblk]
                  (= [:icblk :blank] [x-tag y-tag]) [:icblk :blank]
                  fence-pair?                       :fcblk-pair
                  (= x-tag y-tag)                   :same
                  :else                             [x-tag :_])]
     result)))

(defmulti add
  "Binary operation on tagmaps. Serves to compose tagmaps."
  #'add-df)

(defn from-line
  "Returns a tagmap containing a single tagged line."
  [line]
  [[(-> line block/tagger :tag)
    [line]]])

(defn shift
  "Recalculates tagmap after removing the first n lines of the first entry."
  [n tagmap]
  (->> (rest tagmap)
       (map second)
       (concat (->> tagmap first second (drop n)))
       flatten
       (map from-line)
       (reduce add)))

(defn fuse
  "Concatenates tagmaps x and y with the exception of their adjoining entries.
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
          (fuse-left [(last x)] [(update (first y) 1 #(take n %))])
          (shift n y)))

(defmethod add :fcblk-pair
  [x y]
  (fuse-split x y 1))

(defmethod add [:ofcblk :_]
  [x y]
  (let [lines (->> x last second)]
    (if (and (> (count lines) 1)
             (->> lines
                  ((juxt first last))
                  (apply block/fenced-code-block-pair?)))
      (concat x y)
      (fuse-left x y))))

(defmethod add [:li :_]
  [x y]
  (let [list-item-lines (->> x last second)
        origin (first list-item-lines)
        lines (->> y first second)
        previous-lines (concat [(last list-item-lines)] lines)
        n (->> (map vector lines previous-lines)
               (take-while (fn [[line previous]]
                             (block/belongs-to-list-item? line
                                                          {:previous previous
                                                           :origin origin})))
               count)]
    (fuse-split x y n)))

(defmethod add [:p :tbr]
  [x y]
  (if (some? (block/setext-heading (->> y first second first)))
    (fuse-split (concat (butlast x) [(assoc (last x) 0 :stxh)]) y 1)
    (concat x y)))

(defmethod add [:p :li]
  [x y]
  (if (->> y first second first (re-find block/list-item-blank-lead-line-re) some?)
    (fuse-split (concat (butlast x) [(assoc (last x) 0 :stxh)]) y 1)
    (concat x y)))

(defmethod add [:p :stxh]
  [x y]
  (concat (butlast x)
          [(-> (last x)
               (assoc 0 :stxh)
               (update 1 (comp vec concat) (second (first y))))]
          (rest y)))

(defmethod add [:stxh :_]
  [x y]
  (let [setext-lines (-> x last second)]
    (if (and (= 1 (count setext-lines))
             (some? (re-find block/list-item-blank-lead-line-re (first setext-lines))))
      (add (->> x
                ((juxt butlast
                       (comp vector #(assoc % 0 :li) last)))
                (apply concat)) y)
      (concat x y))))

(defmethod add [:p :icblk]
  [x y]
  (fuse-left x y))

(defmethod add [:blank :icblk]
  [x y]
  (fuse-right x y))

(defmethod add [:icblk :blank]
  [x y]
  (fuse-left x y))

(defmethod add :same
  [x y]
  (fuse-left x y))

(defmethod add :default
  ([] zero)
  ([x] x)
  ([x y]
   (concat x y)))

(defn tokenize
  [string]
  (let [lines (string/split-lines (str "_" string "_"))]
    (if (= 1 (count lines))
      [string]
      (->> lines
           ((juxt (comp list #(subs % 1) first)
                  (comp rest butlast)
                  (comp list #(subs % 0 (dec (count %))) last)))
           (reduce concat)))))

(defmulti promote first)

(defmethod promote :pre-stxh
  [[_ lines]]
  [:li lines])

(defmethod promote :default
  [entry]
  entry)

(defn parse
  "Parses the given input into a flat list of hashes. Each of these hashes
   represents a top-level block."
  [string]
  (->> string
       tokenize
       (map from-line)
       (reduce add)
       (map promote)))

