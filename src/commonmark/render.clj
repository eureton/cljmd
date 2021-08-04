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
  "String representation of an opening HTML tag. The optional attrs parameter
   is expected to be a flat series of name / value strings to make HTML
   attributes of, e.g. (open \"a\" \"href\" \"/url\")"
  [tag & attrs]
  (let [attrs-str (->> attrs
                       (partition 2)
                       (map (fn [[n v]] (str n "=\"" (escape v) "\"")))
                       (string/join " "))]
    (str "<" tag
         (ufn/fix attrs-str not-empty #(str " " %))
         ">")))

(defn close
  "String representation of a closing HTML tag."
  [tag]
  (str "</" tag ">"))

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
  "HTML representation of the AST node as a string."
  (comp :tag :data)
  :hierarchy hierarchy
  :default :full)

(def tag-map
  "Maps AST node tags to HTML tag names."
  {:tbr "hr"
   :hbr "br"
   :em "em"
   :strong "strong"
   :cs "code"
   :p "p"
   :bq "blockquote"
   :li "li"})

(def tag
  "HTML tag name of the given AST node."
  (let [heading? (comp #{:atxh :stxh} :tag :data)]
    (ufn/to-fix heading? #(->> % :data :level (str "h"))
                         (comp tag-map :tag :data))))

(def inner
  "Inner HTML of the given AST node."
  (comp (ufn/to-fix coll? (comp string/join #(map html %)))
        (some-fn :children
                 (comp escape :content :data))))

(def full
  "Outer HTML of the given AST node."
  (comp string/join
        (juxt (comp open tag)
              inner
              (comp close tag))))

(defn compact
  "Compact HTML tag for the given AST node."
  [n]
  (str "<" (tag n) " />"))

(defmethod html :bare [n] (inner n))

(defmethod html :full [n] (full n))

(defmethod html :compact [n] (compact n))

(defmethod html :sbr [_] "\r\n")

(defmethod html :code-block
  [{:as n {:keys [info]} :data}]
  (str "<pre>"
       (apply open (cond-> ["code"]
                     info (conj "class" (str "language-" info))))
       (inner n)
       (close "code")
       "</pre>"))

(defmethod html :a
  [{:as n {:keys [destination title]} :data}]
  (str (apply open (cond-> ["a" "href" destination]
                     title (conj "title" title)))
       (inner n)
       (close "a")))

(defn tighten
  "Replaces the direct :p children of n with their children. Is expected to be
   run on the items of a tight list."
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
  "Transforms Commonmark into HTML."
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

