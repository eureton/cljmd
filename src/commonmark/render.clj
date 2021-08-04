(ns commonmark.render
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [treeduce.core :as tree]
            [commonmark.ast :as ast]))

(defn escape
  "Replaces special characters with HTML entities."
  [s]
  (let [smap {\& "&amp;"
              \< "&lt;"
              \> "&gt;"
              \" "&quot;"}]
    (string/join (replace smap s))))

(defn attributes
  ""
  [xs]
  (->> xs
       (partition 2)
       (map (fn [[n v]] (str n "=\"" (escape v) "\"")))
       (string/join " ")))

(defn open
  "String representation of an opening HTML tag. The optional attrs parameter
   is expected to be a flat series of name / value strings to make HTML
   attributes of, e.g. (open \"a\" \"href\" \"/url\")"
  [tag & attrs]
  (str "<" tag
       (ufn/fix (attributes attrs) not-empty #(str " " %))
       ">"))

(defn close
  "String representation of a closing HTML tag."
  [tag]
  (str "</" tag ">"))

(def hierarchy (-> (make-hierarchy)
                   (derive :ofcblk :code-block)
                   (derive :icblk  :code-block)

                   (derive :doc :bare)
                   (derive :txt :bare)

                   (derive :html-block  :verbatim)
                   (derive :html-inline :verbatim)

                   (derive :hbr :compact)
                   (derive :tbr :compact)
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
   :li "li"
   :img "img"})

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
  [n & attrs]
  (str "<" (tag n)
       (ufn/fix (attributes attrs) not-empty #(str " " %))
       " />"))

(defmethod html :bare [n] (inner n))

(defmethod html :verbatim [n] (:content (:data n)))

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

(defmethod html :img
  [{:as n {:keys [destination title]} :data}]
  (let [alt (tree/reduce (fn [acc {:as x :keys [content]}]
                           (cond-> acc
                             content (str content)))
                         ""
                         n
                         :depth-first)]
    (apply compact (cond-> [n "src" destination "alt" alt]
                     title (conj "title" title)))))

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

