(ns cljmd.ast.postprocessing
  (:require [clojure.string :as string]
            [clojure.core.incubator :refer [dissoc-in]]
            [flatland.useful.fn :as ufn]
            [squirrel.core :as squirrel]
            [cljmd.html :as html]
            [cljmd.ast.common :refer [node update-children fix]]
            [cljmd.ast.predicate :as pred]
            [cljmd.ast.list :as ast.list]
            [cljmd.ast.list.item :as ast.list.item]
            [cljmd.util :as util]))

(defn block-with-hbr-end?
  "True if the node is a block and its last child is an :hbr whose content
   satisfies pred, false otherwise."
  [pred]
  (every-pred pred/block?
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
    (squirrel/map (ufn/to-fix block-with-space-hbr-end? pop-hbr
                              block-with-backslash-hbr-end? (comp push-bslash
                                                                  pop-hbr))
                  ast)))

(def empty-text?
  "True if the parameter is a text node whose :content is either nil or \"\"."
  (every-pred pred/txt?
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
  (every-pred pred/p?
              empty-children?))

(defn empty-p-fix
  "Removes empty :p entities from the AST."
  [ast]
  (squirrel/map #(update-children % (partial remove empty-paragraph?))
                ast))

(def empty-block?
  "True if all of the following apply to the parameter:
     * is a block
     * either has no children or all its children are empty text nodes
   Returns false otherwise."
  (every-pred pred/block? empty-children?))

(defn empty-block-fix
  "Removes empty :p entities from the AST."
  [ast]
  (squirrel/map (ufn/to-fix empty-block? #(dissoc % :children))
                ast))

(defn blank-fix
  "Removes :blank entities from the AST."
  [ast]
  (squirrel/map #(update-children % (comp vec
                                          (partial remove pred/blank?)))
                ast))

(defn unescape
  "Cleans up backslash escapes."
  [string]
  (string/replace string #"\\(\p{Punct})" "$1"))

(defn html-entity-fix
  "Replaces HTML entities where necessary."
  [ast]
  (squirrel/map (ufn/to-fix pred/txt? #(fix % html/unescape-entities
                                              :content)
                            pred/link? #(fix % html/unescape-entities
                                               :destination :title)
                            pred/fcblk? #(fix % html/unescape-entities
                                                :info))
                ast))

(defn backslash-fix
  "Cleans up backslash escapes where necessary."
  [ast]
  (squirrel/map (ufn/to-fix pred/txt? #(fix % unescape :content)
                            pred/link? #(fix % unescape :destination :title)
                            pred/fcblk? #(fix % unescape :info))
                ast))

(defn autolink-fix
  "Unfold :autolink into :a with a :txt child."
  [ast]
  (let [unfold #(-> %
                    (assoc-in [:data :tag] :a)
                    (assoc :children [(node {:tag :txt
                                             :content (-> % :data :text)})])
                    (dissoc-in [:data :text]))]
    (squirrel/map (ufn/to-fix pred/autolink? unfold)
                  ast)))

(defn coalesce-txt
  "Merges adjacent :txt nodes."
  [ast]
  (let [merge? (fn [acc x]
                 (and (-> acc peek peek :data :tag (= :txt))
                      (-> x :data :tag (= :txt))))
        merger #(update-in %1 [:data :content] str (-> %2 :data :content))]
    (squirrel/map (fn [node]
                    (update node
                            :children
                            (comp vec
                                  #(util/coalesce merge? merger %))))
                  ast)))

(defn group-list-items
  "Groups adjacent matching :li nodes into :list nodes."
  [ast]
  (let [not-list? (complement pred/list?)
        to-cluster? (fn [acc x]
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
    (squirrel/map (ufn/to-fix not-list? #(update % :children grouper))
                  ast)))

(defn remove-link-reference-definitions
  "Removes nodes tagged with :adef."
  [ast]
  (squirrel/map (fn [node]
                  (update-children node #(remove pred/adef? %)))
                ast))

(defn trim-txt
  "Strips both tabs and spaces from the end of the content of :txt nodes if:
     * it precedes a hard line break
     * it precedes a soft line break
     * it is the last node (i.e. no sibling follows it)"
  [ast]
  (let [trim? (fn [[x y]]
                (and (pred/txt? x)
                     ((some-fn nil? pred/hbr? pred/sbr?) y)))
        trim (fn [[x _]]
               (update-in x [:data :content] string/trimr))]
    (squirrel/map (fn [node]
                    (->> node
                         :children
                         (partition-all 2 1)
                         (map (ufn/to-fix trim? trim first))
                         (assoc node :children)))
                  ast)))

(def queue
  "A collection of post-processing fixes to apply to the AST."
  [hbr-fix
   empty-p-fix
   empty-block-fix
   html-entity-fix
   backslash-fix
   autolink-fix
   coalesce-txt
   group-list-items
   remove-link-reference-definitions
   blank-fix
   trim-txt])

