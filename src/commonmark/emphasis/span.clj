(ns commonmark.emphasis.span
  (:require [clojure.string :as string]
            [clojure.set]
            [flatland.useful.fn :as ufn]
            [commonmark.emphasis.pair :as pair]
            [commonmark.util :as util]
            [commonmark.re.inline :as re.inline]))

(defn delimiter-run
  "RE to match delimiter runs of the given marker."
  [marker]
  (re.inline/delimiter-run (case marker
                             :star "[*]"
                             :lobar "[_]")))

(defn opener
  "RE to match strings which open emphasis with the given marker."
  [marker]
  ((case marker
     :star re.inline/lfdr
     :lobar re.inline/lobar-open-emphasis) (delimiter-run marker)))

(defn closer
  "RE to match strings which close emphasis with the given marker."
  [marker]
  ((case marker
     :star re.inline/rfdr
     :lobar re.inline/lobar-close-emphasis) (delimiter-run marker)))

(defn bounds-match?
  "Predicate to check tokens against a collection of bounded matches coll."
  [coll]
  (every-pred (comp :run :tags)
              (fn [{:keys [start end]}]
                (some #(and (= start (:re/start %))
                            (= end (:re/end %)))
                      coll))))

(defn demarcate
  "Tags the spans with bounds information."
  [spans]
  (reduce (fn [acc x]
            (let [latest (-> acc peek :end (or 0))
                  size (count (:payload x))]
              (conj acc (assoc x :start latest
                                 :end (+ latest size)
                                 :size size))))
          []
          spans))

(defn orientate
  "Tags the spans with :left and :right, as appropriate."
  [spans]
  (let [string (string/join (map :payload spans))
        [lefts rights] (->> [(opener :star) (opener :lobar)
                             (closer :star) (closer :lobar)]
                            (map #(util/bounded-matches % string))
                            (partition 2)
                            (map (ufn/ap concat)))]
    (->> spans
         (map (ufn/to-fix (bounds-match? lefts) #(update % :tags conj :left)))
         (map (ufn/to-fix (bounds-match? rights) #(update % :tags conj :right))))))

(defn from-string
  "Parses string into a sequence of spans."
  [string]
  (let [run (->> [:star :lobar] (map delimiter-run) (apply util/or-re))
        texts (->> (string/split string run (count string))
                   (map #(hash-map :tags #{:text} :payload %)))
        runs (->> (concat (re-seq run string) [""])
                  (map #(hash-map :tags #{:run} :payload %)))]
    (->> (interleave texts runs)
         (remove (comp empty? :payload))
         demarcate
         orientate
         (remove (comp :text :tags))
         )))

(defn pair?
  "True if spans x and y delimit emphasis, false otherwise."
  [x y]
  (let [{x-size :size x-payload :payload} x
        {y-size :size y-payload :payload} y
        x-left? (-> x :tags :left)
        y-left? (-> y :tags :left)
        x-right? (-> x :tags :right)
        y-right? (-> y :tags :right)
        mul3? (comp zero? #(mod % 3))]
    (and x-left?
         y-right?
         (= (first x-payload) (first y-payload))
         (not (and (or x-right? y-left?)
                   (mul3? (+ x-size y-size))
                   (not (and (mul3? x-size)
                             (mul3? y-size))))))))

(defn innermost-pairs
  "A sequence of pairs included in spans, such that no other pair exists within
   any of those returned. A couple of spans x and y qualify as a pair if
   (pair? x y) evaluates to true."
  [spans]
  (loop [spans spans
         result []]
    (if (empty? spans)
      (->> result
           (map reverse)
           (map (juxt first (comp vector second)))
           (map (ufn/ap hash-map))
           (apply merge-with concat)
           (map (fn [[x y]] [(->> y (sort-by (comp - :start)) first) x])))
      (let [head (first spans)
            tail (rest spans)
            candidate (->> tail (drop-while #(not (pair? head %))) first)]
        (recur tail
               (cond-> result
                 candidate (conj [head candidate])))))))

(defn remove-pair
  "Removes the pair defined by left and right from the spans pool and updates
   the bounds information accordingly. If the respective delimiter runs are
   reduced to zero, then left, right and all intermediate spans are removed
   from the pool."
  [spans [left right]]
  (let [min-size (min (:size left) (:size right))]
    (->> spans
         (reduce (fn [acc x]
                   (cond-> acc
                     (or (not-any? #{left} acc)
                         (= right x)
                         (some #{right} acc)) (conj x)))
                 [])
         (map (ufn/to-fix #{left right} #(update % :size - min-size)))
         (remove (comp zero? :size)))))

(defn snip
  "Substring of string corresponding to the bounds defined by left and right."
  [string [left right]]
  (let [min-size (->> [left right] (map :size) (apply min))]
    (subs string
          (+ (:start left) (- (:size left) min-size))
          (- (:end right) (- (:size right) min-size)))))

(defn pairs
  "Set of pairs included in spans."
  ([previous spans]
   (let [inner (pair/arbitrate (innermost-pairs spans))]
     (if (empty? inner)
       (if previous #{previous} #{})
       (->> inner
            (map #(pairs % (remove-pair spans %)))
            (apply clojure.set/union)))))
  ([spans]
   (pairs nil spans)))

(defn outermost
  "Sequence of outermost emphasis spans within string. Spans are represented by
   :match, :start and :end keys within a hash."
  [string]
  (->> string
       from-string
       pairs
       pair/arbitrate
       (map #(snip string %))
       (map (fn [token]
              (let [start (string/index-of string token)]
                {:re/match token
                 :re/start start
                 :re/end (+ start (count token))})))
       (sort-by :re/start)))

