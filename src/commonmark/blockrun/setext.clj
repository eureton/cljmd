(ns commonmark.blockrun.setext
  (:require [flatland.useful.fn :as ufn]
            [commonmark.block :as block]))

(defn make-map
  "Collects the lines of the given blockrun entry which contain level 1 setext
   underlines in a hash, keyed by index."
  [[_ lines]]
  (->> lines
       (map-indexed vector)
       (filter (comp block/level-1-setext-underline? second))
       flatten
       (apply hash-map)))

(defn redact
  "Removes the lines whose index is contained in the map from entry."
  [index-map [tag lines]]
  (->> lines
       (map-indexed vector)
       (remove (comp index-map first))
       (mapv second)
       (vector tag)))

(defn insert-entry
  "Inserts lines from the map into entry."
  [index-map [tag lines]]
  (loop [index-map index-map
         lines lines]
    (let [index (some (ufn/validator #(<= % (count lines)))
                      (keys index-map))]
      (if (nil? index)
        [tag lines]
        (recur (dissoc index-map index)
               (->> lines
                    (split-at index)
                    (interpose [(index-map index)])
                    (apply concat)
                    vec))))))

(defn insert
  "Inserts lines from the map into blockrun."
  [index-map blockrun]
  (reduce (fn [acc x]
            (let [from (->> acc (map (comp count second)) (apply +))
                  normalized (reduce (fn [acc [index line]]
                                       (cond-> acc
                                         (>= index from) (assoc (- index from)
                                                                line)))
                                     {}
                                     index-map)]
              (conj acc (insert-entry normalized x))))
          []
          blockrun))

