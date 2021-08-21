(ns commonmark.re.inline
  (:require [clojure.string :as string]
            [commonmark.util :as util]
            [commonmark.re.common :as re.common]))

(def code-span
  (let [pre "(?<!`)"
        post "(?!`)"]
    (re-pattern (str "(?s)" pre (re.common/unescaped "(`+)") post
                     "(.*?)"
                     pre #"\1" post))))

(defn delimiter-run
  [character]
  (let [escaped (string/escape (str character) {\* "\\*"})]
    (re-pattern (str "(?<!" escaped ")"
                     (re.common/unescaped escaped) "+"
                     "(?!" escaped ")"))))

(defn lfdr-nopunc
  [delimeter]
  (re-pattern (str delimeter
                   #"(?!\p{IsWhite_Space}|$)"
                   #"(?!\p{IsPunctuation})")))

(defn lfdr-punc
  [delimeter]
  (re-pattern (str #"(?<=^|\p{IsWhite_Space}|\p{IsPunctuation})"
                   delimeter
                   #"(?!\p{IsWhite_Space}|$)"
                   #"(?=\p{IsPunctuation})")))

(defn lfdr
  [delimeter]
  (re-pattern (str "(?:"
                     (lfdr-nopunc delimeter) "|" (lfdr-punc delimeter)
                   ")")))

(defn rfdr-punc
  [delimeter]
  (re-pattern (str #"(?<=\p{IsPunctuation})"
                   #"(?<!^|\p{IsWhite_Space})"
                   delimeter
                   #"(?=\p{IsWhite_Space}|\p{IsPunctuation}|$)")))

(defn rfdr-nopunc
  [delimeter]
  (re-pattern (str #"(?<!\p{IsPunctuation})"
                   #"(?<!^|\p{IsWhite_Space})"
                   delimeter)))

(defn rfdr
  [delimeter]
  (re-pattern (str "(?:"
                     (rfdr-nopunc delimeter) "|" (rfdr-punc delimeter)
                   ")")))

(def star-emphasis
  (let [delimiter (delimiter-run \*)]
    (re.common/between (lfdr delimiter) (rfdr delimiter))))

(defn lobar-open-emphasis
  [delimeter]
  (let [left (lfdr delimeter)
        right (rfdr delimeter)
        punctuated-right (rfdr-punc delimeter)]
    (re-pattern (str "(?:"
                       "(?=" left ")" "(?!" right ")"
                       "|"
                       "(?=" left ")" "(?=" punctuated-right ")"
                     ")" left))))

(defn lobar-close-emphasis
  [delimeter]
  (let [left (lfdr delimeter)
        punctuated-left (lfdr-punc delimeter)
        right (rfdr delimeter)]
    (re-pattern (str "(?:"
                       "(?=" right ")" "(?!" left ")"
                       "|"
                       "(?=" right ")" "(?=" punctuated-left ")"
                     ")" right))))

(def lobar-emphasis
  (let [delimiter (delimiter-run \_)]
    (re.common/between (lobar-open-emphasis delimiter)
                       (lobar-close-emphasis delimiter))))

(def emphasis
  (re-pattern (str "(?s)(?:" star-emphasis "|" lobar-emphasis ")")))

(defn innermost-emphasis-tokens
  [string]
  (->> string
       (re-seq emphasis)
       (map (fn [[whole left-star _ right-star left-lobar _ right-lobar]]
              (let [left-size (count (or left-star left-lobar))
                    right-size (count (or right-star right-lobar))
                    min-size (min left-size right-size)]
                (subs whole
                      (- left-size min-size)
                      (- (count whole) (- right-size min-size))))))))

(defn emphasis-tokens
  ([tokens string]
  (let [inner (innermost-emphasis-tokens string)
        with-inner (apply conj tokens inner)]
    (if (or (empty? inner)
            (every? tokens inner)
            (and (= 1 (count inner))
                 (= string (first inner))))
      with-inner
      (let [uninspected-tokens (remove tokens inner)
            zip (comp str hash)
            unzip (reduce (fn [acc x] (assoc acc (zip x) x)) {} uninspected-tokens)
            replace-rf (fn [f] #(string/replace %1 %2 (f %2)))]
        (->> uninspected-tokens
             ; TODO use the match info instead of string/replace
             (reduce (replace-rf zip) string)
             (emphasis-tokens with-inner)
             (map #(reduce (replace-rf unzip) % (keys unzip)))
             set)))))
  ([string]
   (emphasis-tokens #{} string)))

(defn outermost-emphasis-tokens
  [string]
  (let [tokens (emphasis-tokens string)
        includes? #(and (not= %1 %2)
                        (string/includes? %1 %2))]
    (->> tokens
         (reduce (fn [acc x]
                   (cond-> acc
                     (some #(includes? % x) acc) (disj x)))
                 tokens)
         (map (fn [token]
                (let [start (string/index-of string token)]
                  {:re/match token
                   :re/start start
                   :re/end (+ start (count token))})))
         (sort-by :re/start))))

(def autolink
  (re-pattern (str (re.common/unescaped \<)
                   (util/or-re (str "(" re.common/absolute-uri ")")
                               (str "(" re.common/email-address ")"))
                   ">")))

(def hard-line-break
  (let [end #"(?:\r\n|\n|\r(?!\n)|$)"]
    (re-pattern (str "(?:" #"(?<=\p{Print})  " end "|"
                           #"\\" end
                     ")"))))

(def soft-line-break
  (re-pattern (str #"(?<=.)(?<!(?:[ ]{2,}|\\))"
                   re.common/line-ending
                   #"(?=.)")))

