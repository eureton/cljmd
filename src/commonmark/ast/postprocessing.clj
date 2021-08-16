(ns commonmark.ast.postprocessing
  (:require [clojure.string :as string]
            [clojure.core.incubator :refer [dissoc-in]]
            [flatland.useful.fn :as ufn]
            [treeduce.core :as tree]
            [commonmark.ast.common :refer [block? node update-children]]
            [commonmark.ast.predicate :as pred]
            [commonmark.ast.list :as ast.list]
            [commonmark.ast.list.item :as ast.list.item]
            [commonmark.util :as util]))

(defn block-with-hbr-end?
  "True if the node is a block and its last child is an :hbr whose content
   satisfies pred, false otherwise."
  [pred]
  (every-pred block?
              (comp (every-pred (comp #{:hbr} :tag)
                                (comp pred :content))
                    :data
                    peek
                    :children)))

(def block-with-space-hbr-end?
  "Shorthand for calling block-with-hbr-end? with a predicate which checks for
   space."
  (block-with-hbr-end? #(string/includes? % "  ")))

(def block-with-backslash-hbr-end?
  "Shorthand for calling block-with-hbr-end? with a predicate which checks for
   backslash."
  (block-with-hbr-end? #(string/includes? % "\\")))

(defn hbr-fix
  "Deals with :hbr entities at the end of blocks."
  [ast]
  (let [pop-hbr #(update-children % pop)
        push-bslash #(update % :children conj (node {:tag :txt
                                                     :content "\\"}))]
    (tree/map (ufn/to-fix block-with-space-hbr-end? pop-hbr
                          block-with-backslash-hbr-end? (comp push-bslash
                                                              pop-hbr))
              ast)))

(def empty-text?
  "True if the parameter is a text node whose :content is either nil or \"\"."
  (every-pred (comp #{:txt} :tag :data)
              (comp empty? :content :data)))

(def empty-children?
  "True if the node * either has no children or all its children are empty text
   nodes, false otherwise."
  (some-fn (comp nil? :children)
           (comp empty? #(remove empty-text? %) :children)))

(def empty-paragraph?
  "True if all of the following apply to the parameter:
     * is a paragraph
     * either has no children or all its children are empty text nodes
   Returns false otherwise."
  (every-pred (comp #{:p} :tag :data)
              empty-children?))

(defn empty-p-fix
  "Removes empty :p entities from the AST."
  [ast]
  (tree/map #(update-children % (partial remove empty-paragraph?))
            ast))

(def empty-block?
  "True if all of the following apply to the parameter:
     * is a block
     * either has no children or all its children are empty text nodes
   Returns false otherwise."
  (every-pred block? empty-children?))

(defn empty-block-fix
  "Removes empty :p entities from the AST."
  [ast]
  (tree/map (ufn/to-fix empty-block? #(dissoc % :children))
            ast))

(defn blank-fix
  "Removes :blank entities from the AST."
  [ast]
  (tree/map #(update-children % (comp vec
                                      (partial remove (comp #{:blank}
                                                            :tag
                                                            :data))))
            ast))

(defn unescape
  "Cleans up backslash escapes."
  [string]
  (string/replace string #"\\(\p{Punct})" "$1"))

(defn backslash-fix
  "Clean up backslash escapes from fields which require doing so."
  [ast]
  (let [fix (fn [node & fields]
              (reduce #(cond-> %1
                         ((:data %1) %2) (update-in [:data %2] unescape))
                      node
                      fields))
        txt? (comp #{:txt} :tag :data)
        link? (comp #{:a :img} :tag :data)
        fcblk? (comp #{:ofcblk} :tag :data)]
    (tree/map (ufn/to-fix txt? #(fix % :content)
                          link? #(fix % :destination :title)
                          fcblk? #(fix % :info))
              ast)))

(defn autolink-fix
  "Unfold :autolink into :a with a :txt child."
  [ast]
  (let [autolink? (comp #{:autolink} :tag :data)
        unfold #(-> %
                    (assoc-in [:data :tag] :a)
                    (assoc :children [(node {:tag :txt
                                             :content (-> % :data :text)})])
                    (dissoc-in [:data :text]))]
    (tree/map (ufn/to-fix autolink? unfold)
              ast)))

(defn coalesce-txt
  "Merges adjacent :txt nodes."
  [ast]
  (let [merge? (fn [acc x]
                 (and (-> acc peek peek :data :tag (= :txt))
                      (-> x :data :tag (= :txt))))
        merger #(update-in %1 [:data :content] str (-> %2 :data :content))]
    (tree/map (fn [node]
                (update node :children (comp vec
                                             #(util/coalesce merge? merger %))))
              ast)))

(defn group-list-items
  "Groups adjacent matching :li nodes into :list nodes."
  [ast]
  (let [to-cluster? (fn [acc x]
                      (let [x-tag (-> x :data :tag)
                            last-li (->> acc peek (filter pred/li?) first)]
                        (or (= :blank x-tag)
                            (and (= :li x-tag)
                                 (some? last-li)
                                 (ast.list.item/siblings? last-li x)))))
        is-cluster? #(some pred/li? %)
        to-list (comp vector ast.list/from-items)
        grouper #(->> %
                      (util/cluster to-cluster?)
                      (mapcat (ufn/to-fix is-cluster? to-list)))]
    (tree/map (ufn/to-fix (comp nil? #{:list} :tag :data)
                          #(update % :children grouper))
              ast)))

(defn remove-link-reference-definitions
  "Removes nodes tagged with :adef."
  [ast]
  (tree/map (fn [node]
              (update-children node #(remove pred/adef? %)))
            ast))

(def queue
  "A collection of post-processing fixes to apply to the AST."
  [hbr-fix
   empty-p-fix
   empty-block-fix
   backslash-fix
   autolink-fix
   coalesce-txt
   group-list-items
   remove-link-reference-definitions
   blank-fix])

