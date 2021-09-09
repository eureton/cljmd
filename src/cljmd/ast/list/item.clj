(ns cljmd.ast.list.item
  (:require [flatland.useful.fn :as ufn]
            [cljmd.util :as util]))

(defn marker-type
  "Classifies as either :bullet or :ordered."
  [marker]
  (when marker
    (if (some? (re-find #"[-+*]" marker))
      :bullet
      :ordered)))

(defn siblings?
  "True if items x and y belong in the same list, false otherwise."
  [x y]
  (let [{{x-tag :tag x-marker :marker} :data} x
        {{y-tag :tag y-marker :marker} :data} y
        x-type (marker-type x-marker)
        y-type (marker-type y-marker)]
    (and (= x-tag :li)
         (= y-tag :li)
         (or (= x-marker y-marker)
             (and (= x-type :ordered)
                  (= y-type :ordered)
                  (= (last x-marker)
                     (last y-marker)))))))

(defn tight?
  "True if the children of item comprise a tight list, false otherwise."
  [item]
  (or (-> item :children count (< 3))
      (->> item
           :children
           (map (comp :tag :data))
           (util/coalesce #(and (= :blank (-> %1 peek peek))
                                (= :blank %2))
                          (fn [_ x] x))
           (partition 3 1)
           (map (ufn/knit (comp nil? #{:blank})
                          (comp some? #{:blank})
                          (comp nil? #{:blank})))
           (not-any? #(every? true? %)))))

