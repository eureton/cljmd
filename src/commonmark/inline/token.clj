(ns commonmark.inline.token
  (:require [flatland.useful.fn :as ufn]))

(defn within?
  "Returns logical true if token x lies within token y, logical false
   otherwise."
  [x y]
  (let [{inner-start :re/start inner-end :re/end} x
        {outer-start :re/start outer-end :re/end} y]
    (and (>= inner-start outer-start)
         (<= inner-end outer-end))))

(defn before?
  "Returns true if token x ends before token y, false otherwise."
  [x y]
  (<= (:re/end x) (:re/start y)))

(defn after?
  "Returns true if token x begins after token y, false otherwise."
  [x y]
  (>= (:re/start x) (:re/end y)))

(defn translate
  "Adjusts token boundaries by offset."
  [token offset]
  (-> token
      (update :re/start + offset)
      (update :re/end + offset)))

