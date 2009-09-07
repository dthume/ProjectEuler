(ns com.westoya.euler.string-utils)

(defn char-map
  "Creates a map from each char c in s to (f c)"
  [s f]
  (into {} (map #(vector %1 (f %1)) s)))

(def *digit-cache*
     (char-map "0123456789"
	       #(Integer/parseInt (.toString %))))

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
