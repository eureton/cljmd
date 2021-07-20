(ns commonmark.inline.token
  (:require [clojure.string :as string]
            [commonmark.ast.common :as common]))

(defn between?
  "True if inner-start and -end lie between outer, false otherwise."
  [inner-start inner-end outer-start outer-end]
  (and (>= inner-start outer-start)
       (<= inner-end outer-end)))

(defmulti within?
  "Returns logical true if token x lies within token y, logical false
   otherwise."
  (fn [_ outer] (:tag outer))
  :hierarchy common/ontology)

(defmethod within? :link
  [x {:as y :keys [text] :re/keys [match start]}]
  (let [{inner-start :re/start inner-end :re/end} x
        outer-start (+ start (string/index-of match text))
        outer-end (+ outer-start (count text))]
    (between? inner-start inner-end outer-start outer-end)))

(defmethod within? :default
  [x y]
  (let [{inner-start :re/start inner-end :re/end} x
        {outer-start :re/start outer-end :re/end} y]
    (between? inner-start inner-end outer-start outer-end)))

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

