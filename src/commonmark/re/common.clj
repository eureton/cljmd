(ns commonmark.re.common
  (:require [clojure.string :as string]
            [commonmark.re.html :as re.html]))

(def line-ending-re
  #"(?:\r\n|\n|\r(?!\n))")

(defn blank-line-re
  "Returns a RE which matches lines consisting of zero to limit characters, each
   of which may be either a space or a tab."
  ([limit]
   (re-pattern (str "(?<="
                      "(?:^|" line-ending-re ")"
                    ")"
                    #"[ \t]" "{0," limit "}"
                    "(?="
                      "(?:" line-ending-re "|$)"
                    ")")))
  ([] (blank-line-re 999)))

(defn no-blank-line-till
  [character]
  (re-pattern (str "(?!.*?" line-ending-re #"[ \t]*" line-ending-re ".*?" character ")")))

(def absolute-uri-re
  (re-pattern (str #"\p{Alpha}[\p{Alnum}+.-]{1,31}" ":"
                   #"[\S&&[^\p{Cntrl}<>]]*")))

(def email-address-re
  (re-pattern (str "[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+"
                   "@"
                   "[a-zA-Z0-9]"
                   "(?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
                   "(?:"
                     #"\.[a-zA-Z0-9]"
                     "(?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
                   ")*")))

