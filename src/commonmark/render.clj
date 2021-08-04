(ns commonmark.render
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.ast :as ast]))

(defn escape
  "Replaces special characters with HTML entities."
  [s]
  (let [smap {\& "&amp;"
              \< "&lt;"
              \> "&gt;"
              \" "&quot;"
              \' "&apos;"}]
    (string/join (replace smap s))))

(defn open
  ""
  [s & attrs]
  (let [attrs-str (->> attrs
                       (partition 2)
                       (map (fn [[n v]] (str n "=\"" (escape v) "\"")))
                       (string/join " "))]
    (str "<" s
         (ufn/fix attrs-str not-empty #(str " " %))
         ">")))

(defn close
  ""
  [s]
  (str "</" s ">"))

(def hierarchy (-> (make-hierarchy)
                   (derive :ofcblk :code-block)
                   (derive :icblk  :code-block)

                   (derive :doc        :bare)
                   (derive :txt        :bare)
                   (derive :html-block :bare)

                   (derive :hbr :compact)
                   (derive :hbr :compact)
                   atom))

(defmulti html
  "HTML representation of the node as a string."
  (comp :tag :data)
  :hierarchy hierarchy
  :default :full)

(def tag-map
  ""
  {:tbr "hr"
   :hbr "br"
   :em "em"
   :strong "strong"
   :cs "code"
   :p "p"
   :bq "blockquote"
   :li "li"})

(def tag
  ""
  (let [heading? (comp #{:atxh :stxh} :tag :data)]
    (ufn/to-fix heading? #(->> % :data :level (str "h"))
                         (comp tag-map :tag :data))))

(def inner
  ""
  (comp (ufn/to-fix coll? (comp string/join #(map html %)))
        (some-fn :children
                 (comp escape :content :data))))

(def full
  ""
  (comp string/join
        (juxt (comp open tag)
              inner
              (comp close tag))))

(defn compact
  ""
  [n]
  (str "<" (tag n) " />"))

(defmethod html :bare [n] (inner n))

(defmethod html :full [n] (full n))

(defmethod html :compact [n] (compact n))

(defmethod html :sbr [_] "\r\n")

(defmethod html :code-block
  [n]
  (str "<pre><code"
       (when-some [info (:info (:data n))]
         (str " class=\"language-" info "\""))
       ">"
       (:content (:data n))
       "</code></pre>"))

(defmethod html :a
  [{:as n {:keys [destination title]} :data}]
  (str (apply open (cond-> ["a" "href" destination]
                     title (conj "title" title)))
       (inner n)
       (close "a")))

(defn tighten
  ""
  [n]
  (let [unwrap (ufn/to-fix (comp #{:p} :tag :data) :children
                           vector)]
    (update n :children #(mapcat unwrap %))))

(defmethod html :list
  [{:as n :keys [data]}]
  (let [tag (case (:type data)
              "bullet" "ul"
              "ordered" "ol")
        start-validator (ufn/validator (every-pred some?
                                                   #(-> %
                                                        (Integer/parseInt)
                                                        (> 1))))
        start (start-validator (:start data))
        tight? (= "true" (:tight data))]
    (str (apply open (cond-> [tag]
                       start (conj "start" start)))
         (inner (cond-> n
                  tight? (update :children (comp vec #(map tighten %)))))
         (close tag))))

(def from-string
  "Removes nodes tagged with :adef."
  (comp html ast/from-string))

(from-string "abc\n===\n\n``` clj\none\ntwo\n```\n\n<pre>one\ntwo\nthree</pre>")
(from-string "[abc](xyz '123')")
(from-string "> [*abc*](xyz)\n123\none\ntwo\nthree")
(from-string "[abc](xyz '123')  \nhello")
(from-string "- one\n- two\n- three")
(from-string "2. one\n2. two\n2. three")
(from-string "- one\n  one-and-a-half\n- two\n- three")
(from-string "- ## one\n  one-and-a-half\n\n\n- two\n- three")
(from-string "2. ## one\n   one-and-a-half\n\n\n2. two\n2. three")

