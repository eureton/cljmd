(ns commonmark.re.common
  (:require [clojure.string :as string]
            [commonmark.re.html :as re.html]))

(def line-ending
  #"(?:\r\n|\n|\r(?!\n))")

(def no-backslash-escape
  "Prepend to RE which should not match when preceded by a backslash."
  #"(?<!(?<!\\)\\)")

(defn blank-line
  "Returns a RE which matches lines consisting of zero to limit characters, each
   of which may be either a space or a tab."
  ([limit]
   (re-pattern (str "(?<="
                      "(?:^|" line-ending ")"
                    ")"
                    #"[ \t]" "{0," limit "}"
                    "(?="
                      "(?:" line-ending "|$)"
                    ")")))
  ([] (blank-line 999)))

(defn no-blank-line-till
  [character]
  (re-pattern (str "(?!.*?" line-ending #"[ \t]*" line-ending ".*?" character ")")))

(def absolute-uri
  (re-pattern (str #"\p{Alpha}[\p{Alnum}+.-]{1,31}" ":"
                   #"[\S&&[^\p{Cntrl}<>]]*")))

(def email-address
  (re-pattern (str "[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+"
                   "@"
                   "[a-zA-Z0-9]"
                   "(?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
                   "(?:"
                     #"\.[a-zA-Z0-9]"
                     "(?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
                   ")*")))

