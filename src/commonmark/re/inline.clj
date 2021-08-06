(ns commonmark.re.inline
  (:require [clojure.string :as string]
            [commonmark.util :as util]
            [commonmark.re.common :as re.common]))

(def code-span
  (let [backtick "((?<!`)`+(?!`))"
        spaced #"([ \n].*?[^ ].*?[ \n])"
        non-spaced #"(.*?[^`])"
        same-backtick #"\1(?!`)"]
    (re-pattern (str "(?s)" (re.common/unescaped backtick)
                     "(?:" spaced "|" non-spaced ")"
                     same-backtick))))

(defn emphasis-delimeter
  [character length]
  (let [escaped (string/escape (str character) {\* "\\*"})]
    (re-pattern (str "(?<!" escaped ")"
                     (re.common/unescaped escaped) "{" length "}"
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

(defn star-emphasis
  [delimeter]
  (let [open (lfdr delimeter)
        close (rfdr delimeter)]
    (util/balanced-re open close)))

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

(defn lobar-emphasis
  [delimeter]
  (let [open (lobar-open-emphasis delimeter)
        close (lobar-close-emphasis delimeter)]
    (util/balanced-re open close)))

(defn emphasis
  [length]
  (re-pattern (str "(?s)(?:"
                     (star-emphasis (emphasis-delimeter \* length)) "|"
                     (lobar-emphasis (emphasis-delimeter \_ length))
                   ")")))

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

