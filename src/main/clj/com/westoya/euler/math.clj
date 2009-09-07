(ns com.westoya.euler.math
  (:use
   [clojure.contrib.lazy-seqs :only (primes)]
   [clojure.contrib.math :only (ceil sqrt)]))

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
