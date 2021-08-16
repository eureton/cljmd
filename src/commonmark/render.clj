(ns commonmark.render
  (:require [clojure.string :as string]
            [clojure.java.io :as java.io]
            [cheshire.core :as cheshire]
            [flatland.useful.fn :as ufn]
            [treeduce.core :as tree]
            [commonmark.ast :as ast]
            [commonmark.ast.common :refer [ontology]]
            [commonmark.util :as util]
            [commonmark.ast.predicate :as pred]))

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

(def render-uri
  "Prepares a URI for rendering as HTML."
  (comp util/percent-encode-uri unescape-entities))

(def render-text
  "Prepares HTML text for rendering."
  (comp escape-html unescape-entities))

(defn attributes
  ""
  [xs]
  (->> xs
       (partition 2)
       (map #(apply format "%s=\"%s\"" %))
       (string/join " ")))

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
   :a "a"
   :img "img"})

(def tag
  "HTML tag name of the given AST node."
  (ufn/to-fix pred/heading? #(->> % :data :level (str "h"))
                            (comp tag-map :tag :data)))

(defn open-tag
  "String representation of an opening HTML tag. The optional attrs parameter
   is expected to be a flat series of name / value strings to make HTML
   attributes of, e.g. (open-tag \"a\" \"href\" \"/url\")"
  [tag & attrs]
  (when tag
    (str "<" tag
         (ufn/fix (attributes attrs) not-empty #(str " " %))
         ">")))

(defn close-tag
  "String representation of a closing HTML tag."
  [tag]
  (when tag
    (str "</" tag ">")))

(def hierarchy (-> (deref ontology)
                   (derive :ofcblk :code)
                   (derive :icblk  :code)
                   (derive :cs     :code)

                   (derive :ofcblk :code-block)
                   (derive :icblk  :code-block)

                   (derive :doc :bare)
                   (derive :txt :bare)

                   (derive :html-block  :verbatim)
                   (derive :html-inline :verbatim)

                   (derive :hbr :compact)
                   (derive :tbr :compact)
                   (derive :img :compact)
                   atom))

(defmulti open
  "String representation of the opening HTML tag which corresponds to the node."
  (comp :tag :data)
  :hierarchy hierarchy)

(prefer-method open :code-block :block)

(defmethod open :code-block
  [{:as n {:keys [info]} :data}]
  (let [render (comp unescape-entities #(str "language-" %))]
    (str "\n"
         (open-tag "pre")
         (apply open-tag (cond-> ["code"]
                           info (conj "class" (render info)))))))

(defmethod open :li
  [{{:keys [tight?]} :data}]
  (str "\n"
       (open-tag "li")))

(defmethod open :bq
  [_]
  (str "\n"
       (open-tag "blockquote")))

(defmethod open :img
  [{:as n {:keys [destination title]} :data}]
  (let [alt (tree/reduce (fn [acc {:as x :keys [content]}]
                           (cond-> acc
                             content (str content)))
                         ""
                         n
                         :depth-first)]
    (apply open-tag (cond-> ["img" "src" (render-uri destination) "alt" alt]
                      title (conj "title" (render-text title))))))

(defmethod open :block
  [n]
  (str "\n" (open-tag (tag n))))

(defmethod open :inline
  [n]
  (open-tag (tag n)))

(defmulti close
  "String representation of the closing HTML tag which corresponds to the node."
  (comp :tag :data)
  :hierarchy hierarchy)

(prefer-method close :code-block :block)

(defmethod close :code-block
  [_]
  (str (close-tag "code")
       (close-tag "pre")
       "\n"))

(defmethod close :li
  [n]
  (let [{:keys [children] {:keys [tight?]} :data} n
        break? (and children
                    (or (not tight?)
                        (pred/block? (last children))))]
    (str (when break? "\n")
         (close-tag "li"))))

(defmethod close :bq
  [_]
  (str "\n"
       (close-tag "blockquote")))

(defmethod close :block
  [n]
  (str (close-tag (tag n)) "\n"))

(defmethod close :default
  [n]
  (close-tag (tag n)))

(defmulti html
  "HTML representation of the AST node as a string."
  (comp :tag :data)
  :hierarchy hierarchy
  :default :full)

(def inner
  "Inner HTML of the given AST node."
  (let [content (comp :content :data)
        verbatim? (comp #(isa? (deref hierarchy) % :verbatim) :tag :data)]
    (ufn/to-fix (complement pred/leaf?) (comp string/join #(map html %) :children)
                (every-pred content verbatim?) content
                content (comp render-text content)
                "")))

(def full
  "Outer HTML of the given AST node."
  (comp string/join
        (juxt open inner close)))

(defn compact
  "Compact HTML tag for the given AST node."
  [n]
  (string/replace (open n) #">$" " />"))

(defmethod html :bare [n] (inner n))

(defmethod html :full [n] (full n))

(defmethod html :compact [n] (compact n))

(defmethod html :sbr [_] "\r\n")

(defmethod html :code
  [n]
  (str (open n)
       (-> n :data :content escape-html)
       (close n)))

(defmethod html :a
  [{:as n {:keys [destination title]} :data}]
  (str (apply open-tag (cond-> ["a" "href" (render-uri destination)]
                     title (conj "title" (render-text title))))
       (inner n)
       (close-tag "a")))

(defn tighten
  "Replaces the direct :p children of n with their children."
  [n]
  (let [unwrap (ufn/to-fix pred/p? :children
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
                                                        (not= 1))))
        start (some-> data :start start-validator Integer/parseInt)
        tight? (= "true" (:tight data))
        mark #(assoc-in % [:data :tight?] tight?)]
    (str "\n"
         (apply open-tag (cond-> [tag]
                           start (conj "start" start)))
         (inner (cond-> n
                  true (update :children #(mapv mark %))
                  tight? (update :children #(mapv tighten %))))
         "\n"
         (close-tag tag))))

(def normalize
  "Conformance-related adjustments specific to the HTML output step."
  (comp (ufn/to-fix (every-pred not-empty
                                #(not (string/ends-with? % "\n"))) #(str % "\n"))
        #(string/replace % "\r\n" "\n")
        #(string/replace % "<br />" "<br />\n")))

(def from-string
  "Transforms Commonmark into HTML."
  (let [trim #(-> %
                  (string/replace #"^[\r\n]+" "")
                  (string/replace #"[\n]+" "\n"))]
    (comp normalize trim html ast/from-string)))

