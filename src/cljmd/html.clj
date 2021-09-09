(ns cljmd.html
  (:require [clojure.string :as string]
            [clojure.java.io :as java.io]
            [cheshire.core :as cheshire]
            [flatland.useful.fn :as ufn]
            [cljmd.re.common :as re.common]))

(def entity-map
  "Authoritative HTML entity map, as sourced from html.spec.whatwg.org"
  (-> "entities.json" java.io/resource java.io/reader cheshire/parse-stream))

(defn unescape-entities
  "Replaces HTML entities with the corresponding character."
  [s]
  (let [from-decnum (comp str char (ufn/to-fix zero? 0xFFFD))
        decim (re.common/unescaped #"&#(\d{1,7});")
        hex (re.common/unescaped #"&#[xX](\p{XDigit}{1,6});")
        alpha (re.common/unescaped #"&\p{Print}+?;")]
    (-> s
        (string/replace decim #(-> % second Integer/parseInt from-decnum))
        (string/replace hex #(-> % second (Integer/parseInt 16) from-decnum))
        (string/replace alpha #(get-in entity-map [% "characters"] %)))))

