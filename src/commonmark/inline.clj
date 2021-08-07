(ns commonmark.inline
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.util :as util]
            [commonmark.re.inline :as re.inline]
            [commonmark.re.html :as re.html]
            [commonmark.re.link :as re.link]
            [commonmark.re.common :as re.common]
            [commonmark.inline.token :as token]))

(defn code-span
  [[_ _ inner]]
  {:content (-> inner
                (string/replace #"(?:\r\n|\r|\n)" " ")
                (string/replace #"(?:^ (.*[^ ].*) $)" "$1"))
   :tag :cs})

(defn emphasis-matcher
  [length tag]
  (let [trim #(subs % length (- (count %) length))]
    #(some->> %
              trim
              (hash-map :tag tag :content))))

(def emphasis
  (emphasis-matcher 1 :em))

(def strong-emphasis
  (emphasis-matcher 2 :strong))

(def double-emphasis
  (emphasis-matcher 3 :strong-in-em))

(defn inline-link
  [[_ img? text
    destination-wrapped destination-unwrapped _
    single-quoted-title double-quoted-title parenthesized-title]]
  (let [destination (or destination-wrapped destination-unwrapped)
        title (or single-quoted-title
                  double-quoted-title
                  parenthesized-title)]
    (cond-> {:text text
             :tag (if img? :img :a)}
      destination (assoc :destination destination)
      title (assoc :title title))))

(defn reference-link
  [definitions]
  (fn [match]
    (let [label-count (count definitions)
          full-end (+ label-count 3)
          collapsed-end (+ full-end label-count 1)
          shortcut-end (+ collapsed-end label-count 1)
          full (subvec match 1 full-end)
          collapsed (subvec match full-end collapsed-end)
          shortcut (subvec match collapsed-end shortcut-end)
          match? #(some some? %)
          linear (juxt first second #(drop 2 %))
          nil-text (juxt first (constantly nil) rest)
          [img? text labels] (cond (match? full) (linear full)
                                   (match? collapsed) (nil-text collapsed)
                                   (match? shortcut) (nil-text shortcut))
          label (some identity labels)]
      (some-> label
              util/normalize-link-label
              definitions
              (select-keys [:title :destination])
              (assoc :text (or text label)
                     :tag (if img? :img :a))))))

(defn autolink
  [[_ uri email]]
  {:tag :autolink
   :text (or uri email)
   :destination (cond email (str "mailto:" email)
                      uri (util/percent-encode-uri uri))})

(defn hard-line-break
  [content]
  {:tag :hbr
   :content content})

(defn soft-line-break
  [content]
  {:tag :sbr
   :content content})

(defn html
  [html]
  {:tag :html-inline
   :content html})

(defn matches
  "Returns a vector of hashes, each of which contains:
     * the RE match, i.e. the output of (re-find re s)
     * the start index (include) of re in s
     * the end index (exclusive) of re in s"
  [re s]
  (let [matcher (re-matcher re s)]
    (loop [result []
           match (re-find matcher)]
      (if (nil? match)
        result
        (recur (conj result {:re/match match
                             :re/start (.start matcher)
                             :re/end (.end matcher)})
               (re-find matcher))))))

(defn annotate
  [f info]
  (-> (f (:re/match info))
      (merge (update info :re/match (ufn/to-fix vector? first)))))

(defn sweeper
  "A function which, when called on a string, returns a sequence of tokens.
   Each token represents an inline markdown entity identified by the
   parser in the string. Does not descend into the inner content of the tokens
   it finds. Expects the context of the blockphase parser as parameter."
  [{:keys [definitions] :or {definitions {}}}]
  (->> [[re.inline/code-span                    code-span]
        [re.html/tag                            html]
        [re.inline/autolink                     autolink]
        [re.link/inline                         inline-link]
        [(re.link/reference (keys definitions)) (reference-link definitions)]
        [(re.inline/emphasis 1)                 emphasis]
        [(re.inline/emphasis 2)                 strong-emphasis]
        [(re.inline/emphasis 3)                 double-emphasis]
        [re.inline/hard-line-break              hard-line-break]
        [re.inline/soft-line-break              soft-line-break]]
       (remove #(some nil? %))
       (map (fn [[re f]]
              (fn [string]
                (some->> string
                         (matches re)
                         (map #(annotate f %))))))
       (apply juxt)
       (comp #(remove nil? %) flatten)))

(defn priority
  "Integer representing the priorty of the tag. Greater is higher."
  [tag]
  (let [tags [:sbr :hbr :strong :em :img :a :autolink :html-inline :cs]]
    (.indexOf tags tag)))

(defn superceded?
  "True if x is lower priority than y, false otherwise."
  [x y]
  (and (not= x y)
       (token/cross? x y)
       (< (priority (:tag x)) (priority (:tag y)))))

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

(defn expander
  "Returns a function which, when given a token, returns a vector consisting of
   that token followed by tokens of its inner content."
  [tokenizer]
  (fn [{:as token :re/keys [match start]}]
    (let [content (token/inner token)]
      (->> (when content (tokenizer content))
           (map #(token/translate % (+ start (string/index-of match content))))
           (concat [token])))))

(defn tokenize
  "Returns inline entities in string as a sequence. Descends recursively into
   the inner content of entities. Expects the context of the blockphase parser
   as an optional parameter."
  ([string context]
   (let [tokenizer (sweeper context)]
     (->> (tokenizer string)
          (mapcat (expander tokenizer))
          reconcile)))
  ([string]
   (tokenize string {})))

