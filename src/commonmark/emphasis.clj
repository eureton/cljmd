(ns commonmark.emphasis
  (:require [clojure.string :as string]
            [clojure.set]
            [flatland.useful.fn :as ufn]
            [commonmark.util :as util]
            [commonmark.re.inline :as re.inline]))

(defn delimiter-run
  ""
  [marker]
  (re.inline/delimiter-run (case marker
                             :star "[*]"
                             :lobar "[_]")))

(defn opener
  ""
  [marker]
  (case marker
    :star re.inline/lfdr
    :lobar re.inline/lobar-open-emphasis))

(defn closer
  ""
  [marker]
  (case marker
    :star re.inline/rfdr
    :lobar re.inline/lobar-close-emphasis))

(defn demarcate
  "Reducer which adds bounds information to the token."
  [acc x]
  (let [latest (-> acc peek :end (or 0))
        size (count (:payload x))]
    (conj acc (assoc x :start latest
                       :end (+ latest size)
                       :size size))))

(defn parse
  "Parses string into a sequence of tokens."
  [string marker]
  (let [run (delimiter-run marker)
        texts (->> (string/split string run (count string))
                   (map #(hash-map :tags #{:text} :payload %)))
        runs (->> (concat (re-seq run string) [""])
                  (map #(hash-map :tags #{:run} :payload %)))
        lefts (util/bounded-matches ((opener marker) run) string)
        rights (util/bounded-matches ((closer marker) run) string)
        bounds-match? (fn [coll]
                        (every-pred (comp :run :tags)
                                    (fn [{:keys [start end]}]
                                      (some #(and (= start (:re/start %))
                                                  (= end (:re/end %)))
                                            coll))))]
    (->> (interleave texts runs)
         (remove (comp empty? :payload))
         (reduce demarcate [])
         (map (ufn/to-fix (bounds-match? lefts) #(update % :tags conj :left)))
         (map (ufn/to-fix (bounds-match? rights) #(update % :tags conj :right)))
         (remove (comp :text :tags)))))

(defn pair?
  "True if tokens x and y delimit emphasis, false otherwise."
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

(defn arbitrate
  "Resolves conflicts within pairs."
  [pairs]
  (let [start (comp :start first)
        end (comp :end second)
        includes? #(and (not= %1 %2)
                        (> (start %2) (start %1))
                        (< (end %2) (end %1)))
        offshoot? #(and (= (end %1) (end %2))
                        (= (:size (second %1)) (:size (second %2)))
                        (> (start %1) (start %2)))]
    (reduce (fn [acc x]
              (cond-> acc
                (some (some-fn #(includes? % x) #(offshoot? % x)) acc) (disj x)))
            (set pairs)
            pairs)))

(defn innermost-pairs
  "A sequence of pairs included in tokens, such that no other pair exists within
   any of those returned. A couple of tokens x and y qualify as a pair if
   (pair? x y) evaluates to true."
  [tokens]
  (loop [tokens tokens
         result []]
    (if (empty? tokens)
      (->> result
           (map reverse)
           (map (juxt first (comp vector second)))
           (map (ufn/ap hash-map))
           (apply merge-with concat)
           (map (fn [[x y]] [(->> y (sort-by (comp - :start)) first) x])))
      (let [head (first tokens)
            tail (rest tokens)
            candidate (->> tail (drop-while #(not (pair? head %))) first)]
        (recur tail
               (cond-> result
                 candidate (conj [head candidate])))))))

(defn remove-pair
  "Removes the pair defined by left and right from the tokens pool and updates
   the bounds information accordingly."
  [tokens [left right]]
  (let [min-size (min (:size left) (:size right))]
    (->> tokens
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
  "Set of pairs included in tokens."
  ([previous tokens]
   (let [inner (innermost-pairs tokens)]
     (if (empty? inner)
       (if previous #{previous} #{})
       (->> inner
            (map #(pairs % (remove-pair tokens %)))
            (apply clojure.set/union)))))
  ([tokens]
   (pairs nil tokens)))

(defn outermost
  "Sequence of outermost emphasis spans within string. Spans are represented by
   :match, :start and :end keys within a hash."
  [string]
  (let [pairs-for (fn [marker] (comp arbitrate pairs #(parse % marker)))]
    (->> string
         ((juxt (pairs-for :star) (pairs-for :lobar)))
         (apply clojure.set/union)
         (map #(snip string %))
         (map (fn [token]
                (let [start (string/index-of string token)]
                  {:re/match token
                   :re/start start
                   :re/end (+ start (count token))})))
         (sort-by :re/start))))

