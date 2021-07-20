(ns commonmark.inline
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [commonmark.util :as util]
            [commonmark.re.html :as re.html]
            [commonmark.re.link :as re.link]
            [commonmark.re.common :as re.common]
            [commonmark.inline.token :as token]))

(def code-span-re
  (let [backtick "((?<!`)`+(?!`))"
        spaced #"[ \n](.*?[^ ].*?)[ \n]"
        non-spaced #"(.*?[^`])"
        same-backtick #"\1(?!`)"]
    (re-pattern (str "(?s)" backtick
                     "(?:" spaced "|" non-spaced ")"
                     same-backtick))))

(defn code-span
  [[_ backtick-string spaced-content non-spaced-content]]
  {:backtick-string backtick-string
   :content (-> (or spaced-content non-spaced-content)
                (string/replace #"(?:\r\n|\r|\n)"  " "))
   :tag :cs})

(defn emphasis-delimeter-re
  [character length]
  (let [escaped (string/escape (str character) {\* "\\*"})]
    (re-pattern (str "(?<!" escaped ")"
                     escaped "{" length "}"
                     "(?!" escaped ")"))))

(defn lfdr-nopunc-re
  [delimeter]
  (re-pattern (str delimeter
                   #"(?!\p{IsWhite_Space}|$)"
                   #"(?!\p{IsPunctuation})")))

(defn lfdr-punc-re
  [delimeter]
  (re-pattern (str #"(?<=^|\p{IsWhite_Space}|\p{IsPunctuation})"
                   delimeter
                   #"(?!\p{IsWhite_Space}|$)"
                   #"(?=\p{IsPunctuation})")))

(defn lfdr-re
  [delimeter]
  (re-pattern (str "(?:"
                     (lfdr-nopunc-re delimeter) "|" (lfdr-punc-re delimeter)
                   ")")))

(defn rfdr-punc-re
  [delimeter]
  (re-pattern (str #"(?<=\p{IsPunctuation})"
                   #"(?<!^|\p{IsWhite_Space})"
                   delimeter
                   #"(?=\p{IsWhite_Space}|\p{IsPunctuation}|$)")))

(defn rfdr-nopunc-re
  [delimeter]
  (re-pattern (str #"(?<!\p{IsPunctuation})"
                   #"(?<!^|\p{IsWhite_Space})"
                   delimeter)))

(defn rfdr-re
  [delimeter]
  (re-pattern (str "(?:"
                     (rfdr-nopunc-re delimeter) "|" (rfdr-punc-re delimeter)
                   ")")))

(defn star-emphasis-re
  [delimeter]
  (let [open (lfdr-re delimeter)
        close (rfdr-re delimeter)]
    (util/balanced-re open close)))

(defn lobar-open-emphasis-re
  [delimeter]
  (let [left (lfdr-re delimeter)
        right (rfdr-re delimeter)
        punctuated-right (rfdr-punc-re delimeter)]
    (re-pattern (str "(?:"
                       "(?=" left ")" "(?!" right ")"
                       "|"
                       "(?=" left ")" "(?=" punctuated-right ")"
                     ")" left))))

(defn lobar-close-emphasis-re
  [delimeter]
  (let [left (lfdr-re delimeter)
        punctuated-left (lfdr-punc-re delimeter)
        right (rfdr-re delimeter)]
    (re-pattern (str "(?:"
                       "(?=" right ")" "(?!" left ")"
                       "|"
                       "(?=" right ")" "(?=" punctuated-left ")"
                     ")" right))))

(defn lobar-emphasis-re
  [delimeter]
  (let [open (lobar-open-emphasis-re delimeter)
        close (lobar-close-emphasis-re delimeter)]
    (util/balanced-re open close)))

(defn emphasis-re
  [length]
  (re-pattern (str "(?s)(?:"
                     (star-emphasis-re (emphasis-delimeter-re \* length)) "|"
                     (lobar-emphasis-re (emphasis-delimeter-re \_ length))
                   ")")))

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

(defn full-reference-link
  [definitions]
  (fn [[img? text & labels]]
    (when-some [info (some->> labels
                              (some identity)
                              util/normalize-link-label
                              definitions)]
      (-> info
          (select-keys [:title :destination])
          (assoc :text text
                 :tag (if img? :img :a))))))

(defn textless-reference-link
  [definitions]
  (fn [[& labels]]
    (let [label (->> labels butlast (some identity))]
      (when-some [info (definitions (util/normalize-link-label label))]
        {:tag :a
         :title (:title info)
         :destination (:destination info)
         :text label}))))

(defn reference-link
  [definitions]
  (let [full-count (-> definitions keys count (+ 3))
        full-items #(subvec % 1 full-count)
        full? (comp #(some some? %) full-items)]
    (fn [match]
      (if (full? match)
        ((full-reference-link definitions) (full-items match))
        ((textless-reference-link definitions) (subvec match full-count))))))

(def autolink-re
  (re-pattern (str (util/non-backslash-re \<) "("
                     re.common/absolute-uri-re "|" re.common/email-address-re
                   ")>")))

(defn autolink
  [[_ uri]]
  {:tag :auto
   :uri uri
   :label (java.net.URLDecoder/decode uri)})

(def hard-line-break-re
  (let [end #"(?:\r\n|\n|\r(?!\n)|$)"]
    (re-pattern (str "(?:" #"(?<=\p{Print})  " end "|"
                           #"\\" end
                     ")"))))

(defn hard-line-break
  [content]
  {:tag :hbr
   :content content})

(def soft-line-break-re
  (re-pattern (str #"(?<=.)(?<!(?:[ ]{2,}|\\))"
                   re.common/line-ending-re
                   #"(?=.)")))

(defn soft-line-break
  [content]
  {:tag :sbr
   :content content})

(defn html
  [html]
  {:tag :html-inline
   :content html})

(defmulti inner
  "Nested inline content if the represented entity supports it, nil otherwise."
  :tag)

(defmethod inner :em
  [{:keys [content]}]
  content)

(defmethod inner :strong
  [{:keys [content]}]
  content)

(defmethod inner :a
  [{:keys [text]}]
  text)

(defmethod inner :img
  [{:keys [text]}]
  text)

(defmethod inner :default
  [_]
  nil)

(defn matches
  "Returns a vector of hashes, each of which contains:
     * the RE match (i.e. the output of (re-find re s)
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
   parser in the string.
   Expects the context of the blockphase parser as parameter."
  [{:keys [definitions] :or {definitions {}}}]
  (->> [[code-span-re                              code-span]
        [re.html/tag-re                            html]
        [autolink-re                               autolink]
        [re.link/inline-re                         inline-link]
        [(re.link/reference-re (keys definitions)) (reference-link definitions)]
        [(emphasis-re 1)                           emphasis]
        [(emphasis-re 2)                           strong-emphasis]
        [hard-line-break-re                        hard-line-break]
        [soft-line-break-re                        soft-line-break]]
       (remove #(some nil? %))
       (map (fn [[re f]]
              (fn [string]
                (some->> string
                         (matches re)
                         (map #(annotate f %))))))
       (apply juxt)
       (comp flatten)
       ))

(defn reconcile
  "Curates the list of tokens to not contain mutually exclusive items."
  [tokens]
  (->> tokens
       distinct))

(defn tokenize
  ([string context]
   (let [tokenizer (sweeper context)]
     (->> (tokenizer string)
          (mapcat (fn [{:as token :re/keys [match start]}]
                    (let [content (inner token)]
                      (->> (when content (tokenizer content))
                           (map #(token/translate %
                                                  (+ start (string/index-of match content))))
                           (concat [token])))))
          reconcile
          (sort (comp - (comparator token/within?))))))
  ([string]
   (tokenize string {})))

