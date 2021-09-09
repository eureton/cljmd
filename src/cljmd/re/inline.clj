(ns cljmd.re.inline
  (:require [clojure.string :as string]
            [flatland.useful.fn :as ufn]
            [cljmd.util :as util]
            [cljmd.re.common :refer :all]
            [cljmd.re.link :as link]))

(def code-span
  (let [pre "(?<!`)"
        post "(?!`)"]
    (re-pattern (str "(?s)" pre (unescaped "(`+)") post
                     "(.*?)"
                     pre #"\1" post))))

(defn delimiter-run
  [char-class]
  (re-pattern (str "(?<!" (unescaped char-class) ")"
                   (unescaped char-class) "+"
                   "(?!" char-class ")")))

(defn lfdr-nopunc
  [delimiter]
  (re-pattern (str delimiter
                   #"(?!\p{IsWhite_Space}|$)"
                   #"(?!\p{IsPunctuation})")))

(defn lfdr-punc
  [delimiter]
  (re-pattern (str #"(?<=^|\p{IsWhite_Space}|\p{IsPunctuation})"
                   delimiter
                   #"(?!\p{IsWhite_Space}|$)"
                   #"(?=\p{IsPunctuation})")))

(def lfdr
  (comp (ufn/ap util/or-re) (juxt lfdr-nopunc lfdr-punc)))

(defn rfdr-punc
  [delimiter]
  (re-pattern (str #"(?<=\p{IsPunctuation})"
                   #"(?<!^|\p{IsWhite_Space})"
                   delimiter
                   #"(?=\p{IsWhite_Space}|\p{IsPunctuation}|$)")))

(defn rfdr-nopunc
  [delimiter]
  (re-pattern (str #"(?<!\p{IsPunctuation})"
                   #"(?<!^|\p{IsWhite_Space})"
                   delimiter)))

(def rfdr
  (comp (ufn/ap util/or-re) (juxt rfdr-nopunc rfdr-punc)))

(defn lobar-open-emphasis
  [delimiter]
  (let [left (lfdr delimiter)
        right (rfdr delimiter)
        punctuated-right (rfdr-punc delimiter)]
    (re-pattern (str (util/or-re (str "(?=" left ")" "(?!" right ")")
                                 (str "(?=" left ")" "(?=" punctuated-right ")"))
                     left))))

(defn lobar-close-emphasis
  [delimiter]
  (let [left (lfdr delimiter)
        punctuated-left (lfdr-punc delimiter)
        right (rfdr delimiter)]
    (re-pattern (str (util/or-re (str "(?=" right ")" "(?!" left ")")
                                 (str "(?=" right ")" "(?=" punctuated-left ")"))
                     right))))

(def autolink
  (re-pattern (str (unescaped \<)
                   (util/or-re (str "(" absolute-uri ")")
                               (str "(" email-address ")"))
                   ">")))

(def hard-line-break
  (let [end #"(?:\r\n|\n|\r(?!\n)|$)"]
    (util/or-re (str #"(?<=\p{Print})  " end)
                (str (unescaped #"\\") end))))

(def soft-line-break
  (re-pattern (str #"(?<=.)(?<!(?:[ ]{2,}|\\))"
                   line-ending
                   #"(?=.)")))

(def image-description
  (re-pattern (str (unescaped \!) balanced-square-brackets)))

(def inline-image
  (re-pattern (str image-description
                   #"\("
                     #"\s*"
                     link/destination "?"
                     "(?:" #"\s+" "(" link/title ")" ")?"
                     #"\s*"
                   #"\)")))

(defn full-image-reference
  [labels]
  (if (empty? labels)
    unmatchable
    (re-pattern (str #"(?u)(?i)" image-description
                     (apply util/or-re (map link/label-matcher labels))))))

(defn collapsed-image-reference
  [labels]
  (if (empty? labels)
    unmatchable
    (re-pattern (str #"(?u)(?i)"
                     (unescaped \!)
                     (apply util/or-re (map link/label-matcher labels))
                     #"\[\]"))))

(defn shortcut-image-reference
  [labels]
  (if (empty? labels)
    unmatchable
    (re-pattern (str #"(?u)(?i)"
                     (unescaped \!)
                     (apply util/or-re (map link/label-matcher labels))
                     #"(?!\[\])"
                     "(?!" link/label ")"))))

