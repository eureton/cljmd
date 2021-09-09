(ns commonmark.inline
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.emphasis :as emphasis]
            [commonmark.util :as util]
            [commonmark.re.inline :as re.inline]
            [commonmark.re.html :as re.html]
            [commonmark.re.link :as re.link]
            [commonmark.inline.token :as token]
            [commonmark.inline.precedence :as precedence]))

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
  [text label definitions]
  (some-> label
          util/normalize-link-label
          definitions
          (select-keys [:title :destination])
          (assoc :text (or text label)
                 :tag :a)))

(defn full-link-reference
  [definitions]
  (fn [{:re/keys [match]}]
    (let [[text & labels] (drop 1 match)]
      (link-reference text (some identity labels) definitions))))

(defn textless-link-reference
  [definitions]
  (fn [{:re/keys [match]}]
    (link-reference nil (some identity (drop 1 match)) definitions)))

(defn full-image-reference
  [definitions]
  (comp #(assoc % :tag :img) (full-link-reference definitions)))

(defn textless-image-reference
  [definitions]
  (comp #(assoc % :tag :img) (textless-link-reference definitions)))

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
  (some-> (f info)
          (merge (update info :re/match (ufn/to-fix vector? first)))))

(defn sweeper
  "A function which, when called on a string, returns a sequence of tokens.
   Each token represents an inline markdown entity identified by the
   parser in the string. Does not descend into the inner content of the tokens
   it finds. Expects the context of the blockphase parser as parameter."
  [{:keys [definitions] :or {definitions {}}} claimed]
  (let [labels (keys definitions)
        full-link-handler (full-link-reference definitions)
        textless-link-handler (textless-link-reference definitions)
        full-image-handler (full-image-reference definitions)
        textless-image-handler (textless-image-reference definitions)]
    (->> [[re.inline/code-span                          code-span]
          [re.html/tag                                  html]
          [re.inline/autolink                           autolink]
          [re.link/inline                               inline-link]
          [re.inline/inline-image                       inline-image]
          [(re.link/full-reference labels)              full-link-handler]
          [(re.link/collapsed-reference labels)         textless-link-handler]
          [(re.link/shortcut-reference labels)          textless-link-handler]
          [(re.inline/full-image-reference labels)      full-image-handler]
          [(re.inline/collapsed-image-reference labels) textless-image-handler]
          [(re.inline/shortcut-image-reference labels)  textless-image-handler]
          [emphasis/from-string                         emphasis]
          [re.inline/hard-line-break                    hard-line-break]
          [re.inline/soft-line-break                    soft-line-break]]
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
                      precedence/reconcile)]
       (if (= after before)
         (sort-by :re/start after)
         (recur after)))))
  ([string]
   (tokenize string {})))

