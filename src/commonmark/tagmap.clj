(ns commonmark.tagmap
  (:require [clojure.string :as string]
            [commonmark.block :as block]))

(defn from-line
  "Returns a tagmap containing a single tagged line."
  [line]
  [[(-> line block/tagger :tag)
    [line]]])

(defn fuse
  "Concatenate tagmaps x and y with the exception of their adjoining entries.
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

(defmethod add :fcblk-pair
  [x y]
  (let [x-block (last x)
        y-block (first y)
        the-lines (second y-block)
        new-x-block (update x-block 1 conj (first the-lines))
        tail (->> (rest y)
                  (map second)
                  (concat (rest the-lines))
                  flatten
                  (map from-line)
                  (reduce add))]
    (concat (butlast x)
            [new-x-block]
            tail)))

(defmethod add [:ofcblk :_]
  [x y]
  (let [lines (->> x last second)]
    (if (and (> (count lines) 1)
             (->> lines
                  ((juxt first last))
                  (apply block/fenced-code-block-pair?)))
      (concat x y)
      (fuse-left x y))))

(defmethod add [:p :stxh]
  [x y]
  (concat (butlast x)
          [(-> (last x)
               (assoc 0 :stxh)
               (update 1 #(apply conj %1 %2) (second (first y))))]
          (rest y)))

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

(defn parse
  "Parses the given input into a flat list of hashes. Each of these hashes
   represents a top-level block."
  [string]
  (->> string
       string/split-lines
       (map from-line)
       (reduce add)))

