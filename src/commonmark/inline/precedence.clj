(ns commonmark.inline.precedence
  (:require [commonmark.inline.token :as token]
            [commonmark.util :as util]))

(def priority
  "Integer representing the priorty of the tag. Greater is higher."
  {:sbr 0
   :hbr 1
   :em 2
   :strong 3
   :strong-in-em 4
   :strong-in-strong 5
   :strong-in-strong-in-em 6
   :strong-in-strong-in-strong 7
   :img 8
   :a 8
   :autolink 9
   :html-inline 9
   :cs 9})

(defn html-tag-as-link-destination?
  "True if x and y are links and y is within x, false otherwise."
  [x y]
  (and (= [\< \>] ((juxt first last) (:re/match x)))
       (< (second (token/inner-bounds y)) (:re/start x))
       (>= (:re/end y) (:re/end x))))

(defn link-grouping-within-html-tag?
  "True if all of the following hold:
    * x is a link
    * y is an HTML tag
    * y spans across the grouping bounds of x
   False otherwise."
  [x y]
  (let [[_ link-inner-end] (token/inner-bounds x)]
    (and (< (:re/start y) link-inner-end)
         (> (:re/end y) link-inner-end))))

(defmulti superceded?
  "True if y has higher precedence than x, false otherwise."
  (comp #(map :tag %) vector))

(defmethod superceded? [:a :a]
  [x y]
  (and (not= x y)
       (or (token/within? y x)
           (and (= (:re/end x) (:re/end y))
                (> (:re/start x) (:re/start y))))))

(defmethod superceded? [:html-inline :a]
  [x y]
  (html-tag-as-link-destination? x y))

(defmethod superceded? [:html-inline :img]
  [x y]
  (html-tag-as-link-destination? x y))

(defmethod superceded? [:a :html-inline]
  [x y]
  (link-grouping-within-html-tag? x y))

(defmethod superceded? [:img :html-inline]
  [x y]
  (link-grouping-within-html-tag? x y))

(defmethod superceded? :default
  [x y]
  (let [priority-x (priority (:tag x))
        priority-y (priority (:tag y))]
    (and (not= x y)
         (token/cross? x y)
         (or (< priority-x priority-y)
             (and (= priority-x priority-y)
                  (> (:re/start x) (:re/start y)))))))

(defn reconcile
  "Curates the list of tokens to not contain mutually exclusive items."
  [tokens]
  (let [eliminations (util/graph superceded? tokens)
        eliminated? (every-pred eliminations
                                #(not-every? eliminations (eliminations %)))]
    (->> tokens distinct (remove eliminated?))))

