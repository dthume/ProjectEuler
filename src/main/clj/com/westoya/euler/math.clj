(ns com.westoya.euler.math
  (:use
   [clojure.contrib.lazy-seqs :only (primes)]
   [clojure.contrib.math :only (ceil sqrt)]
   [com.westoya.euler.string-utils :only (digits)]))

(defn pentagonal
  "Returns x if it is a pentagonal number, otherwise nil"
  [x]
  (let [n (/ (+ 1 (sqrt (+ 1 (* 24 x))))
	     6)]
    (when (and (pos? n) (integer? n))
      x)))

(defn hexagonal
  "Returns x if it is a hexagonal number, otherwise nil"
  [x]
  (let [n (/ (+ 1 (sqrt (+ 1 (* 8 x))))
	     4)]
    (when (and (pos? n) (integer? n))
      x)))

(defn factorial
  "Returns the factorial of n (n!)"
  [n]
  (if (or (zero? n) (= 1 n))
    1
    (loop [total n
	   i (dec n)]
      (if (= 1 i)
	total
	(recur (* total i) (dec i))))))

(let [cache (into [] (map #(into #{} (range 1 (inc %)))
			  (range 10)))]
  (defn pandigital
    "Returns n if it is pandigital, otherwise nil"
    [n]
    (let [d (digits n)
	  c (nth cache (count d) nil)]
      (when (and (not (nil? c))
		 (empty? (apply disj c d)))
	n))))

(defn sum-factorials
  "Sum the factorials of a sequence"
  [s]
  (reduce + (map factorial s)))

(defn count-distinct
  "Returns a map whose keys are the distinct items in s, and whose values are
the number of occurrences of each key"
  [s]
  (persistent!
   (reduce (fn [m x] (assoc! m x (inc (m x 0)))) (transient {}) s)))

(defn factors
  "Returns the factors of n"
  [n]
  (cond
    (neg? n) (throw (IllegalArgumentException. "n must be positive"))
    (not (integer? n)) (throw (IllegalArgumentException. "n must be an integer"))
    (zero? n) #{0}
    (= n 1) #{1}
    true (let [limit (ceil (sqrt n))]
	   (loop [facs #{1 n} current 2]
	     (if (> current limit)
	       facs
	       (let [i (/ n current)]
		 (if (integer? i)
		   (recur (conj facs current i) (inc current))
		   (recur facs (inc current)))))))))

(defn proper-divisors
  "Returns the proper divisors of n"
  [n]
  (disj (factors n) n))

(defn abundant-numbers
  []
  (filter #(> (reduce + (proper-divisors %1)) %1)
	  (iterate inc 1)))

(defn prime-factorisation
  "Returns a sequence of the prime factorisation of a number"
  [n]
  (cond
    (neg? n) (throw (IllegalArgumentException. "n must be positive"))
    (not (integer? n)) (throw (IllegalArgumentException. "n must be an integer"))
    (zero? n) [0]
    (= n 1) [1]
    true (let [limit (ceil (sqrt n))]
	   (loop [n n
		  facs (transient [])
		  current (first primes)
		  remaining-primes (rest primes)]
	     (if (= n 1)
	       (persistent! facs)
	       (let [i (/ n current)]
		 (if (integer? i)
		   (recur i (conj! facs current) current remaining-primes)
		   (recur n facs
			  (first remaining-primes)
			  (rest remaining-primes)))))))))

(defn amicable-number?
  "Returns true if n is an amicable-number, else false"
  [n]
  (let [sum-factors (fn [n]
		      (reduce + (filter #(< % n) (factors n))))
	n-sf (sum-factors n)]
    (and (not (= n n-sf))
	 (= n (sum-factors n-sf)))))

(defn word-score
  "Returns the word score of s, i.e. the result of summing the alphabetic
values of the characters in s"
  [s]
  (reduce + (map #(- (int %) 64) s)))
