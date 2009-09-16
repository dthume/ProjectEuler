(ns com.westoya.euler.problems
  (:import
   [java.util Calendar Date])
  (:use
   [clojure.contrib.combinatorics :only (lex-permutations)]
   [clojure.contrib.duck-streams :only (read-lines)]
   [clojure.contrib.lazy-seqs :only (primes)]
   [clojure.contrib.math :only (expt sqrt ceil floor)]
   [clojure.contrib.pprint :only (cl-format pprint)]
   [clojure.contrib.seq-utils :only (flatten indexed rotations)]
   [com.westoya.euler.lazy-seqs]
   [com.westoya.euler.math]
   [com.westoya.euler.string-utils]))

; utility functions

(defn read-triangle
  "Read a number triangle from file f, of the form used by Project Euler"
  [f]
  (map #(for [n (re-seq #"[0-9]{2}" %)]
	  (Integer/parseInt n))
       (read-lines f)))

(defn max-sum-triangle
  "Find the maximum route from top to bottom of triangle, which should be of the
form returned by read-triangle"
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
	grid-size 20
	line-size 4
	line (range line-size)
	empty (repeat grid-size 0)
	axis (range grid-size)
	grid (concat
	      (map (fn [r]
		     (concat (map #(BigInteger. %)(re-seq #"[0-9]+" r))
			     (repeat (dec line-size) 0)))
		   (read-lines f))
	      (repeat (dec line-size) empty))
	value-at (fn [[i j]] (nth (nth grid i empty) j 0))
	reduce-line #(reduce * (map value-at %1))
	lines-from-point (fn [[i j]]
			   (vector
			    (for [n line] [(+ i n) j])
			    (for [n line] [i (+ j n)])
			    (for [n line] [(+ i n) (+ j n)])
			    (for [n line] [(- i n) (+ j n)])))
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

(defn problem17
  "How many letters would be needed to write all the numbers in words from
1 to 1000?"
  []
  (let [to-words #(cl-format nil "~r" %)
	remove-punc #(.replaceAll % "[,\\s-]" "")
	add-and (fn [s n]
		  (if (and (> n 100) (not (zero? (mod n 100))))
		    (str s "AND")
		    s))
	transform-n #(-> %1 to-words remove-punc (add-and %1))
	total (int 1001)]
    (loop [i (int 1)
	   buf (StringBuilder.)]
      (if (= i total)
	(.length (.toString buf))
	(recur (inc i) (.append buf (transform-n i)))))))

(defn problem18
  "Find the maximum sum travelling from the top of the triangle to the base."
  []
  (max-sum-triangle
   (read-triangle "d:/gitrepo/ProjectEuler/src/main/resources/problem18.txt")))

(defn problem19
  "How many Sundays fell on the first of the month during the twentieth century?"
  []
  (let [target (doto (Calendar/getInstance)
		 (.set 2000 11 31 0 0))]
    (loop [cal (doto (Calendar/getInstance)
		 (.set 1901 0 1 0 0))
	   total 0]
      (if (.after cal target)
	total
	(let [increment (if (= Calendar/SUNDAY (.get cal Calendar/DAY_OF_WEEK))
			  1 0)]
	  (recur (doto cal (.add Calendar/MONTH 1)) (+ total increment)))))))

(defn problem20
  "Find the sum of digits in 100!"
  []
  (let [total (apply * (range 1 101))]
    (sum-string-digits total)))

(defn problem21
  "Evaluate the sum of all amicable pairs under 10000."
  []
  (reduce + (filter amicable-number? (range 10000))))

(defn problem22
  "What is the total of all the name scores in the file of first names?"
  []
  (let [f "d:/gitrepo/ProjectEuler/src/main/resources/problem22.txt"]
    (reduce +
	    (map (fn [s]
		   (* (inc (first s))
		      (reduce + (map #(- (int %) 64) (second s)))))
		 (indexed (sort (re-seq #"[A-Za-z]+" (slurp f))))))))

(defn problem23
  "Find the sum of all the positive integers which cannot be written as the sum of two
abundant numbers."
  []
  (let [abundant-list (abundant-numbers)
	abundant-table (into #{} (take-while #(< % 28150) abundant-list))
	source-numbers (take-while #(< % 28124) (iterate inc 1))
	nas (fn [n]
	      (not (some abundant-table
			 (map #(- n %1) (take-while #(< %1 n) abundant-list)))))]
    (reduce + (filter nas source-numbers))))

(defn problem24
  "What is the millionth lexicographic permutation of the digits
0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?"
  []
  (apply str (nth (lex-permutations (range 10)) 999999)))

(defn problem25
  "What is the first term in the Fibonacci sequence to contain 1000 digits?"
  []
  (some #(if (= 1000 (count (.toString (second %1))))
	   (first %1)
	   nil)
	(indexed (fibonacci-sequence))))

(defn problem27
  "Find a quadratic formula that produces the maximum number of primes for consecutive values of n."
  []
  (let [prime-cache primes
	is-prime (memoize (fn [n]
			    (loop [c (first prime-cache)
				   p (rest prime-cache)]
			      (cond
			       (= n c) true
			       (< n c) false
			       true (recur (first p) (rest p))))))
	n-range (iterate inc 0)
	a-range (range -999 1000)
	b-range (range -999 1000)
	ab (for [a a-range b b-range] [a b])
	count-primes (fn [[a b]]
		       (loop [n (first n-range) ns (rest n-range)
			      c 0]
			 (if (not (is-prime (+ (* n n) (* n a) b)))
			   [c [a b]]
			   (recur (first ns) (rest ns) (inc c)))))]
    (apply *
	   (second 
	    (reduce #(if (> (first %1) (first %2)) %1 %2)
		    (map count-primes ab))))))

(defn problem28
 "What is the sum of both diagonals in a 1001 by 1001 spiral?"
 []
 (let [row-size 1001
       odd-numbers (iterate #(+ 2 %) 1)
       r-range (take (ceil (/ row-size 2)) odd-numbers)
       sum-square (fn [[i v]]
		    (let [v2 (* v v)]
		      (* 2 (+ v2 (- v2 (* 6 i))))))]
   (inc (reduce +
		(map sum-square
		     (rest (indexed r-range)))))))

(defn problem29
  "How many distinct terms are in the sequence generated by ab for
2 <= a <= 100 and 2 <= b <= 100?"
  []
  (let [r (range 2 101)]
    (count
     (distinct
      (for [a r b r] (expt a b))))))

(defn problem30
  "Find the sum of all the numbers that can be written as the sum of fifth
powers of their digits."
  []
  (let [digit-map {\0 0,    \1 1,    \2 32,    \3 243,   \4 1024,
		   \5 3125, \6 7776, \7 16807, \8 32768, \9 59049}
	parse-digit-expt #(get digit-map %)
	sum-number #(reduce + (map parse-digit-expt (.toString %)))]
    (reduce +
	    (filter #(= %1 (sum-number %1))
		    (range 2 354294)))))

(defn problem31
  ""
  ([]
     (problem31 #{1 2 5 10 20 50 100 200} 200))
  ([coins target]
     (cond
       (zero? target) 1
       (neg? target) 0
       (empty? coins) 0
       true (let [m (reduce max coins)
		  rm (disj coins m)
		  c (floor (/ target m))]
	      (reduce +
		      (for [i (range (inc c))]
			(problem31 rm (- target (* c i)))))))))

;(println (problem31))

; Note: googling for "factorions" indicated that there were only 4:
; 1, 2, 145 and 40585, which made the rest easy without any code at all.
; The solution below is a brute force attempt, and runs well outside of
; the preferred 1 minute. 
(defn problem34
  "Find the sum of all numbers which are equal to the sum of the factorial of
their digits."
  []
  (let [target (int 2540160)]
    (reduce +
	    (loop [current (int 3)
		   found (transient [])]
	      (if (> current target)
		(persistent! found)
		(if (= current (int (sum-factorials (digits current))))
		  (recur (inc current) (conj! found current))
		  (recur (inc current) found)))))))

(defn problem35
  "How many circular primes are there below one million?"
  []
  (let [p-range (into #{} (take-while #(< % 1000000) primes))
	parse-num-seq #(Integer/parseInt (apply str %))
	circular-prime? (fn [p]
			  (every? p-range
				  (map parse-num-seq (rotations (digits p)))))]
    (count (filter circular-prime? p-range))))

(defn problem36
  "Find the sum of all numbers less than one million, which are palindromic in
base 10 and base 2."
  []
  (reduce +
	  (filter #(and (palindrome? %1)
			(palindrome? (cl-format nil "~b" %1)))
		  (range 1000000))))

(defn problem40
  "Finding the nth digit of the fractional part of the irrational number."
  []
  (let [n (mapcat digits (rest (natural-numbers)))]
    (reduce * (map #(nth n %) [0 9 99 999 9999 99999 999999]))))

(defn problem42
  "How many triangle words does the list of common English words contain?"
  []
  (let [f "d:/gitrepo/ProjectEuler/src/main/resources/problem42.txt"
	words (re-seq #"[A-Za-z]+" (slurp f))
	tri-nums  (into #{} (take 100 (triangle-numbers)))]
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
  (do
    (set! *warn-on-reflection* true)
    (add-classpath "file:D:/gitrepo/ProjectEuler/src/main/clj/"))

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
  (println (problem17))
  (println (problem18))
  (println (problem19))
  (println (problem20))
  (println (problem21))
  (println (problem22))
  (println (problem23))
  (println (problem24))
  (println (problem25))
  (println (problem27))
  (println (problem28))
  (println (problem29))
  (println (problem30))
  (println (problem34))
  (println (problem35))
  (println (problem36))
  (println (problem40))
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
