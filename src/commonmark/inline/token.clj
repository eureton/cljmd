(ns commonmark.inline.token
  (:require [clojure.string :as string]
            [commonmark.ast.common :as common]))

(defmulti inner
  "Nested inline content if the represented entity supports it, nil otherwise."
  :tag
  :hierarchy common/ontology)

(defmethod inner :emphasis
  [{:keys [content]}]
  content)

(defmethod inner :link
  [{:keys [text]}]
  text)

(defmethod inner :default
  [_]
  nil)

(defn inner-bounds
  "A [start end] vector containing :re/start and :re/end when the token has no
   inner content, or the start / end indices of its inner content otherwise."
  [token]
  (let [{:re/keys [match start]} token
        content (or (inner token) match)
        outer-start (+ start (string/index-of match content))]
    [outer-start
     (+ outer-start (count content))]))

(defn within?
  "True if x lies within y, false otherwise."
  [x y]
  (let [{:re/keys [match start]} y
        {inner-start :re/start inner-end :re/end} x
        content (or (inner y) match)
        [outer-start outer-end] (inner-bounds y)]
    (and (>= inner-start outer-start)
         (<= inner-end outer-end))))

(defn before?
  "True if token x ends before token y, false otherwise."
  [x y]
  (<= (:re/end x) (:re/start y)))

(defn after?
  "True if token x begins after token y, false otherwise."
  [x y]
  (>= (:re/start x) (:re/end y)))

(defn translate
  "Adjusts token boundaries by offset."
  [token offset]
  (-> token
      (update :re/start + offset)
      (update :re/end + offset)))

(defn cross?
  "True if either of the following holds, false otherwise:
    * x begins after y and x ends after y
    * y begins after x and y ends after x"
  [x y]
  (let [start-x (:re/start x)
        start-y (:re/start y)
        end-x (:re/end x)
        end-y (:re/end y)]
    (not (or (<= end-x start-y)
             (<= end-y start-x)
             (within? x y)
             (within? y x)))))

