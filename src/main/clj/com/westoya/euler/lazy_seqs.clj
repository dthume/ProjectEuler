(ns com.westoya.euler.lazy-seqs)

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
		  (let [v (* p (/ (- (inc r) c) c))]
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

(defn left-truncations
  "Returns a lazy sequence of the left truncations of s, i.e.
s, (rest s), (rest (rest s)) etc."
  [s]
  (if-let [s (seq s)]
    (lazy-seq (cons s (left-truncations (rest s))))))

(defn right-truncations
  "Returns a lazy sequence of the right truncations of s, i.e.
s, (butlast s), (butlast (butlast s)) etc. Note that the use
of butlast means that the second item in this sequence will
cause s itself to be fully realised"
  [s]
  (if-let [s (seq s)]
    (lazy-seq (cons s (right-truncations (butlast s))))))

(defn truncations
  "Returns a lazy sequence of the distinct left and right
truncations of s"
  [s]
  (distinct
   (lazy-cat (left-truncations s) (right-truncations s))))