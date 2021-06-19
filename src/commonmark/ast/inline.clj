(ns commonmark.ast.inline
  (:require [clojure.string :as string]
            [commonmark.inline :as inline]
            [commonmark.util :as util]))

(defn tokens
  "Parses string for inline markdown entity tokens. Tokens are collected in a
   hash, under a unique digest key. Hash Collection is recursive, i.e. a hash
   entry may reference another entry."
  [string]
  (loop [s string
         h {}]
    (if-some [{:keys [tag pattern content]} (inline/tagger s)]
      (let [digest (str (hash content))]
        (recur (string/replace-first s pattern digest)
               (assoc h digest {:pattern pattern
                                :tag tag
                                :value content})))
      h)))

(defn digest
  "Replaces tokens in string with their respective key in the tokens hash."
  [string tokens]
  (reduce (fn [acc [digest {:keys [pattern]}]]
            (string/replace-first acc pattern digest))
          string
          tokens))

(defn trim
  "Replaces degenerate nodes in the ast with their children.
   A degenerate node is a non-leaf text node."
  [ast]
  (mapcat (fn [[t v]]
            (if (and (= :txt t)
                     (coll? v))
              v
              [[t v]])) ast))

(defn inflate
  "Recursively replaces in string the keys in the tokens hash with the AST which
   corresponds to the tokenized entity. Returns the children of the root node of
   the AST."
  [string tokens]
  (if-some [[k {:keys [tag value]}] (->> tokens
                                         (filter (comp #(string/includes? string %) key))
                                         first)]
    (->> (util/split string (re-pattern k))
         (map #(vector :txt %))
         (interpose [tag value])
         (remove (comp string/blank? second))
         (map #(update % 1 inflate (dissoc tokens k)))
         trim
         vec)
    [[:txt string]]))

(defn ast
  "Parses string into an AST. Assumes string contains inline Markdown entities."
  [string]
  (let [token-hash (tokens string)]
    [:doc (inflate (digest string token-hash) token-hash)]))

