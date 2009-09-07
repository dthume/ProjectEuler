(ns com.westoya.euler.string-utils)

(defn char-map
  ""
  [s f]
  (into {} (map #(vector %1 (f %1)) s)))

(def *digit-cache* {\0 0, \1 1, \2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9})

(defn parse-digit
  "Parses a digit from character c"
  [c]
  (if-let [d (get *digit-cache* c)]
    d
    (Integer/parseInt (.toString c))))

(defn digits
  "Returns a sequence of the digits comprising n"
  [n]
  (map parse-digit (.toString n)))

(defn sum-string-digits
  "Returns the sum of digits in n"
  [n]
  (apply + (digits n)))

(defn reverse-string
  "Reverses s"
  [s]
  (.toString (.reverse (StringBuilder. (str s)))))

(defn palindrome?
  "Returns true if the string representation of s is palindromic"
  [s]
  (let [s (str s)]
    (= s (reverse-string s))))
