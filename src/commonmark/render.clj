(ns commonmark.render
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.ast :as ast]))

(defn open-tag
  ""
  [s]
  (str "<" s ">"))

(defn close-tag
  ""
  [s]
  (str "</" s ">"))

(def ontology (-> (make-hierarchy)
                  (derive :atxh :heading)
                  (derive :stxh :heading)

                  (derive :ofcblk :code-block)
                  (derive :icblk  :code-block)
                  atom))

(defmulti html
  "HTML representation of the node as a string."
  (comp :tag :data)
  :hierarchy ontology)

(defmulti tag
  ""
  (comp :tag :data)
  :hierarchy ontology)

(defmethod tag :heading
  [n]
  (str "h" (:level (:data n))))

(defmethod tag :tbr [_] "hr")

(defmethod tag :hbr [_] "br")

(defmethod tag :em [_] "em")

(defmethod tag :strong [_] "strong")

(defmethod tag :cs [_] "code")

(defmethod tag :p [_] "p")

(def inner
  ""
  (comp (ufn/to-fix coll? (comp string/join #(map html %)))
        (some-fn :children
                 (comp :content :data))))

(def full
  ""
  (comp string/join
        (juxt (comp open-tag tag)
              inner
              (comp close-tag tag))))

(defn compact
  ""
  [n]
  (str "<" (tag n) " />"))

(defmethod html :doc [n] (inner n))

(defmethod html :default [n] (full n))

(defmethod html :txt [n] (inner n))

(defmethod html :html-block [n] (inner n))

(defmethod html :hbr [n] (compact n))

(defmethod html :tbr [n] (compact n))

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
  (str "<a href=\"" destination "\""
       (when title
         (str " title=\"" title "\""))
       ">"
       (inner n)
       "</a>"))

(def from-string
  "Removes nodes tagged with :adef."
  (comp html ast/from-string))

(from-string "abc\n===\n\n``` clj\none\ntwo\n```\n\n<pre>one\ntwo\nthree</pre>")
(from-string "[abc](xyz '123')")

