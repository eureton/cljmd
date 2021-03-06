(ns cljmd.unicode
  (:require [clojure.string :as string]
            [clojure.set]
            [flatland.useful.fn :as ufn]))

(def folding-mappings
  "Hash of characters which case-fold to more than one character. Keys are the
   characters, values are strings of the characters to fold to.
   Source: https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt"
  (->> {\u00DF [\u0073 \u0073]
        \u0130 [\u0069 \u0307]
        \u0149 [\u02BC \u006E]
        \u01F0 [\u006A \u030C]
        \u0390 [\u03B9 \u0308 \u0301]
        \u03B0 [\u03C5 \u0308 \u0301]
        \u0587 [\u0565 \u0582]
        \u1E96 [\u0068 \u0331]
        \u1E97 [\u0074 \u0308]
        \u1E98 [\u0077 \u030A]
        \u1E99 [\u0079 \u030A]
        \u1E9A [\u0061 \u02BE]
        \u1E9E [\u0073 \u0073]
        \u1F50 [\u03C5 \u0313]
        \u1F52 [\u03C5 \u0313 \u0300]
        \u1F54 [\u03C5 \u0313 \u0301]
        \u1F56 [\u03C5 \u0313 \u0342]
        \u1F80 [\u1F00 \u03B9]
        \u1F81 [\u1F01 \u03B9]
        \u1F82 [\u1F02 \u03B9]
        \u1F83 [\u1F03 \u03B9]
        \u1F84 [\u1F04 \u03B9]
        \u1F85 [\u1F05 \u03B9]
        \u1F86 [\u1F06 \u03B9]
        \u1F87 [\u1F07 \u03B9]
        \u1F88 [\u1F00 \u03B9]
        \u1F89 [\u1F01 \u03B9]
        \u1F8A [\u1F02 \u03B9]
        \u1F8B [\u1F03 \u03B9]
        \u1F8C [\u1F04 \u03B9]
        \u1F8D [\u1F05 \u03B9]
        \u1F8E [\u1F06 \u03B9]
        \u1F8F [\u1F07 \u03B9]
        \u1F90 [\u1F20 \u03B9]
        \u1F91 [\u1F21 \u03B9]
        \u1F92 [\u1F22 \u03B9]
        \u1F93 [\u1F23 \u03B9]
        \u1F94 [\u1F24 \u03B9]
        \u1F95 [\u1F25 \u03B9]
        \u1F96 [\u1F26 \u03B9]
        \u1F97 [\u1F27 \u03B9]
        \u1F98 [\u1F20 \u03B9]
        \u1F99 [\u1F21 \u03B9]
        \u1F9A [\u1F22 \u03B9]
        \u1F9B [\u1F23 \u03B9]
        \u1F9C [\u1F24 \u03B9]
        \u1F9D [\u1F25 \u03B9]
        \u1F9E [\u1F26 \u03B9]
        \u1F9F [\u1F27 \u03B9]
        \u1FA0 [\u1F60 \u03B9]
        \u1FA1 [\u1F61 \u03B9]
        \u1FA2 [\u1F62 \u03B9]
        \u1FA3 [\u1F63 \u03B9]
        \u1FA4 [\u1F64 \u03B9]
        \u1FA5 [\u1F65 \u03B9]
        \u1FA6 [\u1F66 \u03B9]
        \u1FA7 [\u1F67 \u03B9]
        \u1FA8 [\u1F60 \u03B9]
        \u1FA9 [\u1F61 \u03B9]
        \u1FAA [\u1F62 \u03B9]
        \u1FAB [\u1F63 \u03B9]
        \u1FAC [\u1F64 \u03B9]
        \u1FAD [\u1F65 \u03B9]
        \u1FAE [\u1F66 \u03B9]
        \u1FAF [\u1F67 \u03B9]
        \u1FB2 [\u1F70 \u03B9]
        \u1FB3 [\u03B1 \u03B9]
        \u1FB4 [\u03AC \u03B9]
        \u1FB6 [\u03B1 \u0342]
        \u1FB7 [\u03B1 \u0342 \u03B9]
        \u1FBC [\u03B1 \u03B9]
        \u1FC2 [\u1F74 \u03B9]
        \u1FC3 [\u03B7 \u03B9]
        \u1FC4 [\u03AE \u03B9]
        \u1FC6 [\u03B7 \u0342]
        \u1FC7 [\u03B7 \u0342 \u03B9]
        \u1FCC [\u03B7 \u03B9]
        \u1FD2 [\u03B9 \u0308 \u0300]
        \u1FD3 [\u03B9 \u0308 \u0301]
        \u1FD6 [\u03B9 \u0342]
        \u1FD7 [\u03B9 \u0308 \u0342]
        \u1FE2 [\u03C5 \u0308 \u0300]
        \u1FE3 [\u03C5 \u0308 \u0301]
        \u1FE4 [\u03C1 \u0313]
        \u1FE6 [\u03C5 \u0342]
        \u1FE7 [\u03C5 \u0308 \u0342]
        \u1FF2 [\u1F7C \u03B9]
        \u1FF3 [\u03C9 \u03B9]
        \u1FF4 [\u03CE \u03B9]
        \u1FF6 [\u03C9 \u0342]
        \u1FF7 [\u03C9 \u0342 \u03B9]
        \u1FFC [\u03C9 \u03B9]
        \uFB00 [\u0066 \u0066]
        \uFB01 [\u0066 \u0069]
        \uFB02 [\u0066 \u006C]
        \uFB03 [\u0066 \u0066 \u0069]
        \uFB04 [\u0066 \u0066 \u006C]
        \uFB05 [\u0073 \u0074]
        \uFB06 [\u0073 \u0074]
        \uFB13 [\u0574 \u0576]
        \uFB14 [\u0574 \u0565]
        \uFB15 [\u0574 \u056B]
        \uFB16 [\u057E \u0576]
        \uFB17 [\u0574 \u056D]}
       (map (juxt key (comp string/join val)))
       (into {})))

(def reverse-folding-mappings
  "Hash of characters which case-fold to more than one character. Keys are the
   strings of the characters to fold to, values are the characters themselves in
   string format.
   Source: https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt"
  (->> folding-mappings
       (map (comp (ufn/ap hash-map)
                  (juxt val (comp set vector str key))))
       (apply merge-with clojure.set/union)))

(def fold
  "String with Unicode case folded downwards."
  (comp string/lower-case string/upper-case string/lower-case))

(defn unfold
  "Set of strings containing all possible combinations of replacing sequences of
   characters with the character they fold from."
  [string]
  (reduce-kv (fn [acc k v]
               (->> v
                    (map (fn [x]
                           (reduce #(conj %1 (string/replace %2 k x))
                                   #{}
                                   acc)))
                    (apply clojure.set/union acc)))
             #{(string/lower-case string)}
             reverse-folding-mappings))

