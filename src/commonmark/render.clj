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

(def children
  ""
  (comp (ufn/to-fix coll? (comp string/join #(map html %)))
        (some-fn :children
                 (comp :content :data))))

(def full
  ""
  (comp string/join
        (juxt (comp open-tag tag)
              children
              (comp close-tag tag))))

(defn compact
  ""
  [n]
  (str "<" (tag n) " />"))

(defmethod html :doc [n] (children n))

(defmethod html :default [n] (full n))

(defmethod html :txt [n] (:content (:data n)))

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

(def from-string
  "Removes nodes tagged with :adef."
  (comp html ast/from-string))

(from-string "abc\n===\n\n``` clj\none\ntwo\n```")

