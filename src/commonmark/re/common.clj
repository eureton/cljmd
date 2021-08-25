(ns commonmark.re.common)

(def line-ending
  #"(?:\r\n|\n|\r(?!\n))")

(def no-preceding-backslash
  "Negative lookbehind assertion that no unescaped backslash precedes."
  #"(?<!(?<!\\)\\)")

(defn unescaped
  "RE to match the input only when it is not preceded by a backslash."
  [x]
  (re-pattern (str "(?:" no-preceding-backslash x ")")))

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

