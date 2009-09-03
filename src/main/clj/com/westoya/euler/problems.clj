(ns com.westoya.euler.problems
  (:use
   [clojure.contrib.combinatorics :only (lex-permutations)]
   [clojure.contrib.duck-streams :only (read-lines)]
   [clojure.contrib.lazy-seqs :only (primes)]
   [clojure.contrib.math :only (expt sqrt ceil)]
   [clojure.contrib.pprint :only (cl-format)]
   [clojure.contrib.seq-utils :only (flatten indexed)]))

; utility functions

(defn natural-numbers
  "Generates a (fresh) lazy sequence of the natural numbers"
  []
  (iterate inc 0))

(defn fibonacci-sequence
  "Generates a (fresh) lazy fibonacci sequence"
  ([]
     (fibonacci-sequence 0 1))
  ([a b]
     (lazy-seq
       (cons a (fibonacci-sequence b (+ a b))))))

(defn triangle-numbers
  "Generates a (fresh) lazy sequence of triangle numbers"
  []
  (map first (iterate
	      (fn [item]
		(let [n (inc (second item))]
		  (vector (* (/ n 2) (inc n)) n)))
	      [1 1])))

(defn pascals-triangle-row
  "Returns a (fresh) lazy sequence of the numbers in row r of
Pascals Triangle"
  ([r]
     (letfn [(ptr-rec [r p c]
	      (if (= c r)
		[1]
		(lazy-seq
		  (let [v (if (zero? c)
			    1
			    (* p (/ (- (inc r) c) c)))]
		    (cons v (ptr-rec r v (inc c)))))))]
       (if (zero? r)
	 [1]
	 (lazy-seq
	   (cons 1 (ptr-rec r 1 1)))))))

(defn pascals-triangle-sequence
  "Generates a (fresh) lazy sequence of rows of Pascals Triangle,
starting at row 0 or row r if supplied"
  ([] (pascals-triangle-sequence 0))
  ([r]
     (map pascals-triangle-row (iterate inc 0))))

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

(defn parse-digit
  "Parses a digit from character c"
  [c]
  (Integer/parseInt (.toString c)))

(defn digits
  "Returns a sequence of the digits comprising n"
  [n]
  (map parse-digit (.toString n)))

(defn sum-string-digits
  [n]
  (apply + (digits n)))

(defn reverse-string
  [s]
  (.toString (.reverse (StringBuilder. (str s)))))

(defn palindrome?
  [s]
  (let [s (str s)]
    (= s (reverse-string s))))

(defn read-triangle
  [f]
  (map #(for [n (re-seq #"[0-9]{2}" %)]
	  (Integer/parseInt n))
       (read-lines f)))

(defn max-sum-triangle
  [triangle]
  (loop [current-maxes (first triangle)
	 current-row (second triangle)
	 remaining (nnext triangle)]
    (if (empty? current-row)
      (reduce max current-maxes)
      (let [calc-max (fn [item]
		       (let [i (first item)
			     v (second item)]
			 (max (+ v (nth current-maxes i 0))
			      (+ v (nth current-maxes (dec i) 0)))))]
	(recur (map calc-max (indexed current-row))
	       (first remaining)
	       (rest remaining))))))

(defn factorial
  [n]
  (if (or (zero? n) (= 1 n))
    1
    (loop [total n
	   i (dec n)]
      (if (= 1 i)
	total
	(recur (* total i) (dec i))))))

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
		  facs (transient (vector))
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

(defn word-score
  [s]
  (reduce + (map #(- (int %) 64) s)))

; problem solutions

(defn problem1
  "Add all the natural numbers below one thousand that are multiples of 3 or 5."
  []
  (reduce +
	 (filter #(or (= 0 (mod %1 3))
		      (= 0 (mod %1 5)))
		 (take 1000 (natural-numbers)))))

(defn problem2
  "Find the sum of all the even-valued terms in the Fibonacci sequence which
do not exceed four million."
  []
  (reduce +
	  (take-while #(< % 4000000)
		      (filter even? (fibonacci-sequence)))))

; Reading blogs about this problem was what led me to project euler,
; by which time I'd read quite a bit about solving it.  The solution
; below is taken directly from http://paste.lisp.org/display/83620
; since there seemed little point in pretending I'd come up with the
; solution on my own
(defn problem3
  "Find the largest prime factor of a composite number."
  []
  (let [div-max (fn [number divisor]
		  (if (zero? (rem number divisor))
		    (recur (/ number divisor) divisor)
		    number))]
    (loop [n 600851475143 div 2]
      (if (> div n)
	(- div 1)
	(recur (div-max n div) (inc div))))))

(defn problem4
  "Find the largest palindrome made from the product of two 3-digit numbers."
  []
  (let [r (range 100 1000)]
    (reduce max
	    (filter palindrome?
		    (for [r1 r r2 r] (* r1 r2))))))

(defn problem5
  "What is the smallest number divisible by each of the numbers 1 to 20?"
  []
  (let [divs [20 19 18 17 16 15 14 13 12 11]]
    (some
     (fn [i]
       (if (every? #(zero? (rem i %)) divs)
	 i false))
     (iterate #(+ % 20) 20))))

(defn problem6
  "What is the difference between the sum of the squares and the square of
the sums?"
  []
  (let [square #(* %1 %1)
	numbers (range 1 101)
	squaresum (long (square (apply + numbers)))
	sumsquare (long (apply + (map square numbers)))]
    (- squaresum sumsquare)))

(defn problem7
  "Find the 10001st prime."
  []
  (nth primes 10000))

(defn problem8
  "Discover the largest product of five consecutive digits in the 1000-digit
number."
  []
  (let [f "d:/gitrepo/ProjectEuler/src/main/resources/problem8.txt"
	s (first (read-lines f))
	sumseq #(* %1 (parse-digit %2))]
    (reduce max
	    (map #(reduce sumseq 1 %)
		 (partition 5 1 s)))))

(defn problem10
  "Calculate the sum of all the primes below two million."
  []
  (reduce + (take-while #(< % 2000000) primes)))

(defn problem11
  "What is the greatest product of four numbers on the same straight line in
the 20 by 20 grid?"
  []
  (let [f "d:/gitrepo/ProjectEuler/src/main/resources/problem11.txt"
	grid-size (int 20)
	empty (repeat grid-size 0)
	axis (range grid-size)
	grid (concat
	      (map (fn [r]
		     (concat (map #(BigInteger. %)(re-seq #"[0-9]+" r))
			     [0 0 0]))
		   (read-lines f))
	      [empty]
	      [empty]
	      [empty])
	value-at (fn [point]
		   (nth (nth grid (first point) empty) (second point) 0))
	reduce-line #(reduce * (map value-at %1))
	lines-from-point
	(fn [[i j]]
	  [[[i j] [(+ i 1) j] [(+ i 2) j] [(+ i 3) j]]
	   [[i j] [i (+ j 1)] [i (+ j 2)] [i (+ j 3)]]
	   [[i j] [(+ i 1) (+ j 1)] [(+ i 2) (+ j 2)] [(+ i 3) (+ j 3)]]
	   [[i j] [(- i 1) (+ j 1)] [(- i 2) (+ j 2)] [(- i 3) (+ j 3)]]])
	points (for [i axis j axis] [i j])]

    (reduce max
	    (map reduce-line
		 (mapcat lines-from-point points)))))

(defn problem12
  "What is the value of the first triangle number to have over five hundred
divisors?"
  []
  (let [num-factors
	(fn [n]
	  (reduce *
		  (map inc
		       (vals (count-distinct (prime-factorisation n))))))]
    (some #(when (> (num-factors %1) 500)
	     %1)
	  (triangle-numbers))))

(defn problem13
  "Find the first ten digits of the sum of one-hundred 50-digit numbers."
  []
  (let [f "d:/gitrepo/ProjectEuler/src/main/resources/problem13.txt"
	numbers (map #(BigInteger. %) (read-lines f))]
    (apply str
	   (map parse-digit
		(take 10 (.toString (reduce + numbers)))))))

(defn problem14
  "Find the longest sequence using a starting number under one million."
  []
  (let [starting-numbers (range 1 1000000)
	gen-sequence (fn [n]
		       (loop [i n
			      s (list)]
			 (cond (= i 1) (vector (count s) n)
			       (even? i) (recur (/ i 2) (conj s i))
			       (odd? i) (recur (inc (* 3 i)) (conj s i)))))]
    (second (reduce #(if (> (first %1) (first %2)) %1 %2)
		    (map gen-sequence starting-numbers)))))

(defn problem15
  "Starting in the top left corner in a 20 by 20 grid, how many routes are
there to the bottom right corner?"
  []
  (nth (pascals-triangle-row 40) 20))

(defn problem16
  "What is the sum of the digits of the number 2^1000?"
  []
  (sum-string-digits (.pow (BigInteger. "2") 1000)))

(defn problem18
  "Find the maximum sum travelling from the top of the triangle to the base."
  []
  (max-sum-triangle
   (read-triangle "d:/gitrepo/ProjectEuler/src/main/resources/problem18.txt")))

(defn problem20
  "Find the sum of digits in 100!"
  []
  (let [total (apply * (range 1 101))]
    (sum-string-digits total)))

(defn problem22
  "What is the total of all the name scores in the file of first names?"
  []
  (let [f "d:/gitrepo/ProjectEuler/src/main/resources/problem22.txt"]
    (reduce +
	    (map (fn [s]
		   (* (inc (first s))
		      (reduce + (map #(- (int %) 64) (second s)))))
		 (indexed (sort (re-seq #"[A-Za-z]+" (slurp f))))))))

(defn problem24
  "What is the millionth lexicographic permutation of the digits
0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?"
  []
  (nth (lex-permutations (range 10)) 999999))

(defn problem25
  "What is the first term in the Fibonacci sequence to contain 1000 digits?"
  []
  (some #(if (= 1000 (count (.toString (second %1))))
	   (first %1)
	   nil)
	(indexed (fibonacci-sequence))))

(defn problem29
  "How many distinct terms are in the sequence generated by ab for
2 <= a <= 100 and 2 <= b <= 100?"
  []
  (let [r (range 2 101)]
    (count
     (distinct
      (for [a r b r] (expt a b))))))

(defn problem36
  "Find the sum of all numbers less than one million, which are palindromic in
base 10 and base 2."
  []
  (reduce +
	  (filter #(and (palindrome? %1)
			(palindrome? (cl-format nil "~b" %1)))
		  (range 1000000))))

(defn problem42
  "How many triangle words does the list of common English words contain?"
  []
  (let [f "d:/gitrepo/ProjectEuler/src/main/resources/problem42.txt"
	words (re-seq #"[A-Za-z]+" (slurp f))
	tri-nums  (into (hash-set) (take 100 (triangle-numbers)))]
    (count
     (filter tri-nums
	     (map word-score words)))))

(defn problem45
  "After 40755, what is the next triangle number that is also pentagonal
and hexagonal?"
  []
  (let [tri-nums (drop 285 (triangle-numbers))]
    (some #(and (hexagonal %1) (pentagonal %1))
	  tri-nums)))

(defn problem47
  "Find the first four consecutive integers to have four distinct primes factors."
  []
  (let [r (for [i (iterate inc 0)
		:when (= (count (distinct (prime-factorisation i))) 4)]
	    i)]
    (loop [n r]
      (let [i (first n)]
	(if (and (= (+ i 1) (nth n 1))
		 (= (+ i 2) (nth n 2))
		 (= (+ i 3) (nth n 3)))
	  i
	  (recur (rest n)))))))

(defn problem48
  "Find the last ten digits of 11 + 22 + ... + 10001000."
  []
  (let [total (.toString
	       (reduce + (map #(expt %1 %1) (range 1 1001))))]
    (apply str (drop (- (count total) 10) total))))

(defn problem52
  "Find the smallest positive integer, x, such that 2x, 3x, 4x,
5x, and 6x, contain the same digits in some order."
  []
  (let [digit-set #(apply hash-set (digits %))
	same-digits
	(fn [n]
	  (let [n-digits (digit-set n)]
	    (if (and
		 (= n-digits (digit-set (* 2 n)))
		 (= n-digits (digit-set (* 3 n)))
		 (= n-digits (digit-set (* 4 n)))
		 (= n-digits (digit-set (* 5 n)))
		 (= n-digits (digit-set (* 6 n))))
	      n
	      nil)))]
    (some same-digits (iterate inc 1))))

(defn problem53
  "How many values of C(n,r), for 1 <= n <= 100, exceed one-million?"
  []
  (let [n-range (range 1 101)
	r-for-n (fn [n] (for [r (range 1 (inc n))] [n r]))
	total-combinations
	(fn [pair]
	  (let [n (first pair)
		r (second pair)]
	    (/ (factorial n)
	       (* (factorial r)
		  (factorial (- n r))))))]
    (count
     (filter #(> % 1000000)
	     (map total-combinations
		  (mapcat r-for-n n-range))))))

(defn problem55
  "How many Lychrel numbers are there below ten-thousand?"
  []
  (let [reverse-num #(BigInteger. (reverse-string %))]
    (count
     (filter identity
	     (for [n (range 10000)]
	       (loop [n n i 50]
		 (if (zero? i)
		   true
		   (let [r (+ n (reverse-num n))]
		     (if (palindrome? r)
		       false
		       (recur r (dec i)))))))))))

(defn problem56
  "Considering natural numbers of the form, ab, finding the maximum
digital sum."
  []
  (let [r (range 100)]
    (reduce max
	    (map sum-string-digits
		 (for [a r b r] (expt a b))))))

(defn problem67
  "Using an efficient algorithm find the maximal sum in the triangle?"
  []
  (max-sum-triangle
   (read-triangle "d:/gitrepo/ProjectEuler/src/main/resources/problem67.txt")))

; I confess I googled for help on this one after my first few attempts proved
; to be *way* too slow.  The solution here is basically a clojure implementation
; of http://blog.dreamshire.com/2009/04/09/project-euler-problem-206-solution/
(defn problem206
  "Concealed Square"
  []
  (let [pattern #"1[0-9]2[0-9]3[0-9]4[0-9]5[0-9]6[0-9]7[0-9]8[0-9]9"]
    (loop [n (.toBigInteger (BigDecimal. (inc (sqrt 19293949596979899))))]
      (if (re-matches pattern (str (* n n)))
	(* 10 n)
	(recur (- n 2))))))

(comment

  (set! *warn-on-reflection* true)

  (println (problem1))
  (println (problem2))
  (println (problem3))
  (println (problem4))
  (println (problem5))
  (println (problem6))
  (println (problem7))
  (println (problem8))
  (println (problem10))
  (println (problem11))
  (println (problem12))
  (println (problem13))
  (println (problem14))
  (println (problem15))
  (println (problem16))
  (println (problem18))
  (println (problem20))
  (println (problem22))
  (println (problem24))
  (println (problem25))
  (println (problem29))
  (println (problem36))
  (println (problem42))
  (println (problem45))
  (println (problem47))
  (println (problem48))
  (println (problem52))
  (println (problem53))
  (println (problem55))
  (println (problem56))
  (println (problem67))
  (println (problem206))

)
