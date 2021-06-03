(ns commonmark.tagmap
  (:require [clojure.string :as string]
            [commonmark.block :as block]))

(defn from-line
  "Returns a tagmap containing a single tagged line."
  [line]
  [[(-> line block/tagger :tag)
    [line]]])

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
                  (= [:p :stxh] [x-tag y-tag]) [:p :stxh]
                  fence-pair?                  :fcblk-pair
                  (= x-tag y-tag)              :same
                  :else                        [x-tag :_])]
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
  (let [x-block (last x)
        y-block (first y)
        new-x-block (update x-block 1 #(apply conj %1 %2) (second y-block))]
    (concat (butlast x)
            [new-x-block]
            (rest y))))

(defmethod add [:p :stxh]
  [x y]
  (concat (butlast x)
          [(assoc (last x) 0 :stxh)]
          (rest y)))

(defmethod add :same
  [x y]
  (let [x-block (last x)
        y-block (first y)
        mid (some-> x-block
                    (update 1 #(apply conj %1 %2) (second y-block))
                    vector)]
    (concat (butlast x)
            mid
            (rest y))))

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

(let [ls1 ["``` c++"
           "class Foo {"
           "};"]
      ls2 ["``` "
           "- one"
           "- two"]
      stx [" one two three"
           " four five six"
           " ============="]
      st (->> stx
              (map from-line)
              (reduce add)
              )
      t1 (->> ls1
              (map from-line)
              (reduce add)
              )
      t2 (->> ls2
              (map from-line)
              (reduce add)
              )]
; (add t1 t2)
  st
  )

