(ns cljmd.ast.inline
  (:require [clojure.string :as string]
            [squirrel.node :as node :refer [node]]
            [flatland.useful.fn :as ufn]
            [cljmd.inline :as inline]
            [cljmd.inline.token :as token]
            [cljmd.ast.common :refer [branch]]
            [cljmd.ast.predicate :as pred]))

(def degenerate?
  "Returns true if the AST node is degenerate, false otherwise.
   A degenerate node is a non-leaf text node."
  (every-pred pred/txt? node/not-leaf?))

(def hierarchy (-> (deref cljmd.ast.common/ontology)
                   (derive :html-inline :leaf)
                   (derive :cs          :leaf)
                   (derive :hbr         :leaf)
                   (derive :sbr         :leaf)
                   atom))

(defmulti inflate
  "Recursively replaces inline markdown entities with the AST to which they
   belong."
  (fn [input _]
    (cond
      (string? input) :string
      (map? input) (:tag input)))
  :hierarchy hierarchy)

(defn unpack
  "Transforms string into a vector of ASTs, each of which corresponds to an
   inline markdown entity."
  [string tokens]
  (if-some [{:as token :re/keys [start end]} (first tokens)]
    (->> [[(subs string 0 start) (->> tokens
                                      rest
                                      (filter #(token/before? % token)))]
          [token                 (->> tokens
                                      rest
                                      (filter #(token/within? % token))
                                      (map #(token/translate % (- start))))]
          [(subs string end)     (->> tokens
                                      rest
                                      (filter #(token/after? % token))
                                      (map #(token/translate % (- end))))]]
         (remove (comp empty? first))
         (map (ufn/ap inflate))
         (mapcat (ufn/to-fix degenerate? :children list)))
    (when string
      [(node {:tag :txt
              :content string})])))

(defmethod inflate :link
  [{:as input :keys [text] :re/keys [match]} tokens]
  (node (select-keys input [:tag :destination :title])
        (:children (inflate (assoc input :tag :default :content text)
                            tokens))))

(defmethod inflate :autolink
  [input _]
  (node (select-keys input [:tag :destination :text])))

(defmethod inflate :leaf
  [input _]
  (node (select-keys input [:tag :content])))

(defmethod inflate :string
  [input tokens]
  (node {:tag :txt}
        (unpack input tokens)))

(defmethod inflate :deep-emphasis
  [input tokens]
  (let [tags (->> (case (:tag input)
                    :strong-in-em               [:em :strong]
                    :strong-in-strong           [:strong :strong]
                    :strong-in-strong-in-em     [:em :strong :strong]
                    :strong-in-strong-in-strong [:strong :strong :strong])
                    (map #(hash-map :tag %)))]
    (->> (inflate (assoc input :tag :default) tokens)
         :children
         (branch tags))))

(defmethod inflate :default
  [{:keys [tag content] :re/keys [match]} tokens]
  (->> tokens
       (map #(token/translate % (- (string/index-of match content))))
       (unpack content)
       (node {:tag tag})))

(defn from-string
  "Parses string into an AST. Assumes string contains inline Markdown entities.
   Returns an AST whose root node is tagged :doc."
  ([string context]
   (some->> (inline/tokenize string context)
            (sort (comp - (comparator token/within?)))
            (unpack string)
            (node {:tag :doc})))
  ([string]
   (from-string string {})))

