(ns cljmd.emphasis.pair)

(def start
  (comp :start first))

(def end
  (comp :end second))

(defn includes?
  "True if y is wholly within x, false otherwise."
  [x y]
  (and (not= x y)
       (> (start y) (start x))
       (< (end y) (end x))))

(defn offshoot?
  "True if all of the following hold:
    * x and y are closed by the same delimiter run
    * x and y are closed by delimiter runs of equal length
    * x begins after y
  False otherwise."
  [x y]
  (and (= (end x) (end y))
       (= (:size (second x)) (:size (second y)))
       (> (start x) (start y))))

(defn stradles-start?
  "True if x begins before y does and x ends before y does, false otherwise."
  [x y]
  (and (< (start x) (start y))
       (< (start y) (end x))
       (< (end x) (end y))))

(defn supercedes-fn?
  "Predicate to test a collection against x."
  [x]
  (some-fn #(includes? % x)
           #(offshoot? % x)
           #(stradles-start? % x)))

(defn arbitrate
  "Resolves conflicts within pairs."
  [pairs]
  (reduce (fn [acc x]
            (cond-> acc
              (some (supercedes-fn? x) acc) (disj x)))
          (set pairs)
          (sort-by (comp :start first) pairs)))

