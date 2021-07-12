(ns commonmark.blockrun.entry
  (:require [clojure.string :as string]
            [commonmark.block :as block]))

(def origin
  "Returns the first line."
  (comp first second))

(defmulti content
  "Returns the content of the blockrun entry as a single string. If its contents
   span multiple lines, the lines are joined. If the entry represents a leaf
   block, no other processing is performed. If the entry represents a container
   node, the container whitespace and markers are stripped before joining."
  first)

(defmethod content :li
  [[_ lines]]
  (let [origin (first lines)]
    (->> lines
         rest
         (map #(block/list-item-content % origin))
         (concat [(:content (block/list-item-lead-line origin))])
         (string/join "\r\n"))))

(defmethod content :bq
  [[_ lines]]
  (let [strip #(let [{:keys [tag content]} (block/tagger %)]
                 (if (= :bq tag) content %))]
    (->> lines
         (map strip)
         (string/join "\r\n"))))

(defmethod content :default
  [[_ lines]]
  (string/join "\r\n" lines))

(defn split-lines
  ""
  [f [tag lines]]
  (let [retains? #(fn [coll] (f coll (% coll)))
        without-prefix (->> lines
                            (iterate rest)
                            (drop-while (retains? rest))
                            first)
        aref (->> without-prefix
                  (iterate butlast)
                  (drop-while (retains? butlast))
                  first)]
    (->> [(->> lines (drop-last (count without-prefix)) vec)
          (vec aref)
          (->> without-prefix (drop (count aref)) vec)]
         (remove empty?))))

(defn split-link-reference-definition
  ""
  [x]
  (let [score #(->> %
                    (string/join "\n")
                    block/link-reference-definition
                    ((juxt :label :destination :title))
                    (remove nil?)
                    count)
        undiminished? #(<= (score %1) (score %2))
        [pre aref suf] (split-lines undiminished? x)
        [tag _] x]
    (->> [pre aref suf]
         (remove empty?)
         (map vector [tag :aref tag]))))

(defmulti promote
  "Hook for changing the tag of an entry after the blockrun has been compiled."
  first)

(defmethod promote :html-block-unpaired
  [x]
  (assoc x 0 (if (block/html-block-begin (origin x))
               :html-block
               :p)))

(defmethod promote :default [x] x)

(defmulti fragment
  "Hook for breaking up an entry after the blockrun has been compiled."
  first)

(defmethod fragment :p
  [[_ lines]]
  (let [score #(->> %
                    (string/join "\n")
                    block/link-reference-definition
                    ((juxt :label :destination :title))
                    (remove nil?)
                    count)
        undiminished? #(fn [coll] (<= (score coll) (score (% coll))))
        without-prefix (->> lines
                            (iterate rest)
                            (drop-while (undiminished? rest))
                            first)
        aref (->> without-prefix
                  (iterate butlast)
                  (drop-while (undiminished? butlast))
                  first)]
    (->> [[:p    (->> lines (drop-last (count without-prefix)) vec)]
          [:aref (vec aref)]
          [:p    (->> without-prefix (drop (count aref)) vec)]]
         (remove (comp empty? second)))))

(defmethod fragment :default [x] [x])

