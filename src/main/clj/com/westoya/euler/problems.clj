(ns com.westoya.euler.problems
  (:use [clojure.contrib.lazy-seqs :only (primes)]))

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

(defn parse-digit
  "Parses a digit from character c"
  [c]
  (Integer/parseInt (.toString c)))

(defn sum-string-digits
  [s]
  (apply + (map parse-digit (.toString s))))

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

(defn problem3
  "Find the largest prime factor of a composite number."
  [n]
  ; check 2 and 3, then only check
  ; 6n - 1 and 6n + 1 to see if:
  ; a) they are factors
  ; b) they are prime
  )

(defn problem4
  "What is the difference between the sum of the squares and the square of
the sums?"
  []
  (let [square #(* %1 %1)
	numbers (range 1 101)
	squaresum (long (square (apply + numbers)))
	sumsquare (long (apply + (map square numbers)))]
    (- squaresum sumsquare)))

(defn problem5
  "What is the smallest number divisible by each of the numbers 1 to 20?"
  []
  (let [divs [20 19 18 17 16 15 14 13 12 11]]
    (some
     (fn [i]
       (if (every? #(zero? (rem i %)) divs)
	 i false))
     (iterate #(+ % 20) 20))))

(defn problem8
  "Discover the largest product of five consecutive digits in the 1000-digit
number."
  []
  (let [s "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
	sumseq #(* %1 (parse-digit %2))]
    (reduce max
	    (map #(reduce sumseq 1 %)
		 (partition 5 1 s)))))

(defn problem16
  "What is the sum of the digits of the number 2^1000?"
  []
  (sum-string-digits (.pow (BigInteger. "2") 1000)))

(defn problem20
  "Find the sum of digits in 100!"
  []
  (let [total (apply * (range 1 101))]
    (sum-string-digits total)))

(.pow (BigInteger. "2") 1000)

(comment
  
  (println (problem1))
  (println (problem2))
  (println (problem3 600851475143))
  (println (problem4))
  (println (problem5))
  (println (problem8))
  (println (problem16))
  (println (problem20))
  
)
