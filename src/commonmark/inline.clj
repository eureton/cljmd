(ns commonmark.inline
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.emphasis :as emphasis]
            [commonmark.util :as util]
            [commonmark.re.inline :as re.inline]
            [commonmark.re.html :as re.html]
            [commonmark.re.link :as re.link]
            [commonmark.re.common :as re.common]
            [commonmark.inline.token :as token]))

(defn code-span
  [{[_ _ inner] :re/match}]
  {:content (-> inner
                (string/replace #"(?:\r\n|\r|\n)" " ")
                (string/replace #"(?:^ (.*[^ ].*) $)" "$1"))
   :tag :cs})

(defn emphasis
  [{:re/keys [match]}]
  (let [run (str "[" (first match) "]*")
        length-left (->> match (re-find (re-pattern (str "^" run))) count)
        length-right (->> match (re-find (re-pattern (str run "$"))) count)
        min-length (min length-left length-right)]
    {:tag (case min-length
            1 :em
            2 :strong
            3 :strong-in-em
            4 :strong-in-strong
            5 :strong-in-strong-in-em
            6 :strong-in-strong-in-strong)
     :content (subs match min-length (- (count match) min-length))}))

(defn inline-link
  [{[_ text
     destination-wrapped destination-unwrapped _
     single-quoted-title double-quoted-title
     parenthesized-title] :re/match}]
  (let [destination (or destination-wrapped destination-unwrapped)
        title (or single-quoted-title
                  double-quoted-title
                  parenthesized-title)]
    (cond-> {:tag :a :text text}
      destination (assoc :destination destination)
      title (assoc :title title))))

(def inline-image
  (comp #(assoc % :tag :img) inline-link))

(defn link-reference
  [definitions]
  (fn [{:re/keys [match]}]
    (let [label-count (count definitions)
          full-end (+ label-count 2)
          collapsed-end (+ full-end label-count)
          shortcut-end (+ collapsed-end label-count)
          full (subvec match 1 full-end)
          collapsed (subvec match full-end collapsed-end)
          shortcut (subvec match collapsed-end shortcut-end)
          match? #(some some? %)
          linear (juxt first rest)
          nil-text #(vector nil %)
          [text labels] (cond (match? full) (linear full)
                              (match? collapsed) (nil-text collapsed)
                              (match? shortcut) (nil-text shortcut))
          label (some identity labels)]
      (some-> label
              util/normalize-link-label
              definitions
              (select-keys [:title :destination])
              (assoc :text (or text label)
                     :tag :a)))))

(defn image-reference
  [definitions]
  (comp #(assoc % :tag :img) (link-reference definitions)))

(defn autolink
  [{[_ uri email] :re/match}]
  {:tag :autolink
   :text (or uri email)
   :destination (cond email (str "mailto:" email)
                      uri (util/percent-encode-uri uri))})

(defn hard-line-break
  [info]
  {:tag :hbr
   :content (:re/match info)})

(defn soft-line-break
  [info]
  {:tag :sbr
   :content (:re/match info)})

(defn html
  [info]
  {:tag :html-inline
   :content (:re/match info)})

(defn matches-fn
  [claimed f s]
  (f s claimed))

(defn annotate
  [f info]
  (-> (f info)
      (merge (update info :re/match (ufn/to-fix vector? first)))))

(defn sweeper
  "A function which, when called on a string, returns a sequence of tokens.
   Each token represents an inline markdown entity identified by the
   parser in the string. Does not descend into the inner content of the tokens
   it finds. Expects the context of the blockphase parser as parameter."
  [{:keys [definitions] :or {definitions {}}} claimed]
  (let [labels (keys definitions)]
    (->> [[re.inline/code-span                code-span]
          [re.html/tag                        html]
          [re.inline/autolink                 autolink]
          [re.link/inline                     inline-link]
          [re.inline/inline-image             inline-image]
          [(re.link/reference labels)         (link-reference definitions)]
          [(re.inline/image-reference labels) (image-reference definitions)]
          [emphasis/from-string               emphasis]
          [re.inline/hard-line-break          hard-line-break]
          [re.inline/soft-line-break          soft-line-break]]
         (remove #(some nil? %))
         (map (fn [[c f]]
                (fn [string]
                  (some->> string
                           ((if (fn? c)
                              (partial matches-fn claimed)
                              util/bounded-matches) c)
                           (map #(annotate f %))))))
         (apply juxt)
         (comp #(remove nil? %) flatten))))

(def priority
  "Integer representing the priorty of the tag. Greater is higher."
  {:sbr 0
   :hbr 1
   :em 2
   :strong 3
   :strong-in-em 4
   :strong-in-strong 5
   :strong-in-strong-in-em 6
   :strong-in-strong-in-strong 7
   :img 8
   :a 8
   :autolink 9
   :html-inline 9
   :cs 9})

(defn superceded?
  "True if y has higher precedence than x, false otherwise."
  [x y]
  (let [priority-x (priority (:tag x))
        priority-y (priority (:tag y))]
    (and (not= x y)
         (token/cross? x y)
         (or (< priority-x priority-y)
             (and (= priority-x priority-y)
                  (> (:re/start x) (:re/start y)))))))

(defn enforce-precedence
  "Removes tokens which conflict with tokens of higher priority."
  [tokens]
  (reduce (fn [acc x] (remove #(superceded? % x) acc))
          tokens
          tokens))

(defn reconcile
  "Curates the list of tokens to not contain mutually exclusive items."
  [tokens]
  (->> tokens
       distinct
       enforce-precedence))

(defn expander-fn
  "Returns a function which, when given a token, returns a vector consisting of
   that token followed by tokens of its inner content."
  [tokenizer]
  (fn expand [{:as token :re/keys [match start]}]
    (let [content (token/inner token)]
      (->> (when content (tokenizer content))
           (map #(token/translate % (+ start (string/index-of match content))))
           (mapcat expand)
           (concat [token])))))

(defn tokenize
  "Returns inline entities in string as a sequence. Descends recursively into
   the inner content of entities. Expects the context of the blockphase parser
   as an optional parameter."
  ([string context]
   (loop [before []]
     (let [tokenizer (sweeper context before)
           after (->> (tokenizer string)
                      (mapcat (expander-fn tokenizer))
                      (concat before)
                      reconcile)]
       (if (= after before)
         after
         (recur after)))))
  ([string]
   (tokenize string {})))

