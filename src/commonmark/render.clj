(ns commonmark.render
  (:require [clojure.string :as string]
            [clojure.java.io :as java.io]
            [cheshire.core :as cheshire]
            [flatland.useful.fn :as ufn]
            [treeduce.core :as tree]
            [commonmark.ast :as ast]
            [commonmark.ast.common :refer [leaf?]]
            [commonmark.util :as util]))

(def entity-map
  "Authoritative HTML entity map, as sourced from html.spec.whatwg.org"
  (-> "entities.json" java.io/resource java.io/reader cheshire/parse-stream))

(defn unescape-entities
  "Replaces HTML entities with the corresponding character."
  [s]
  (let [from-decnum (comp str char (ufn/to-fix zero? 0xFFFD))]
    (-> s
        (string/replace #"&#(\d{1,7});" #(-> %
                                             second
                                             Integer/parseInt
                                             from-decnum))
        (string/replace #"&#[xX](\p{XDigit}{1,6});" #(-> %
                                                         second
                                                         (Integer/parseInt 16)
                                                         from-decnum))
        (string/replace #"&\p{Print}+?;" #(get-in entity-map [% "characters"] %)))))

(defn escape-html
  "Replaces HTML special characters with HTML entities."
  [s]
  (let [smap {\& "&amp;"
              \< "&lt;"
              \> "&gt;"
              \" "&quot;"}]
    (string/join (replace smap s))))

(defn encode-uri
  "Percent-encodes non-ASCII characters."
  [uri]
  (string/replace uri #"\P{ASCII}+" #(java.net.URLEncoder/encode % "UTF-8")))

(def render-uri
  "Prepares a URI for rendering as HTML."
  (comp encode-uri unescape-entities))

(defn attributes
  ""
  [xs]
  (->> xs
       (partition 2)
       (map #(apply format "%s=\"%s\"" %))
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
  (ufn/to-fix leaf? (comp escape-html unescape-entities :content :data)
                    (comp string/join #(map html %) :children)))

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

(defmethod html :cs
  [{:as n {:keys [info]} :data}]
  (str (open "code")
       (-> n :data :content escape-html)
       (close "code")))

(defmethod html :code-block
  [{:as n {:keys [info]} :data}]
  (let [render (comp unescape-entities #(str "language-" %))]
    (str "<pre>"
         (apply open (cond-> ["code"]
                       info (conj "class" (render info))))
         (-> n :data :content escape-html)
         (close "code")
         "</pre>")))

(defmethod html :a
  [{:as n {:keys [destination title]} :data}]
  (str (apply open (cond-> ["a" "href" (render-uri destination)]
                     title (conj "title" (unescape-entities title))))
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
    (apply compact (cond-> [n "src" (render-uri destination) "alt" alt]
                       title (conj "title" (unescape-entities title))))))

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

