(ns com.westoya.euler.problems)

(defn natural-numbers [] (iterate inc 0))

(defn fibonacci-sequence
  ([]
     (fibonacci-sequence 0 1))
  ([a b]
     (lazy-seq
       (cons a (fibonacci-sequence b (+ a b))))))

(defn problem1
  []
  (apply +
	 (filter #(or (= 0 (mod %1 3))
		      (= 0 (mod %1 5)))
		 (take 1000 (natural-numbers)))))

(defn problem2
  []
  (apply +
	 (take-while
	  #(< % 4000000)
	  (filter even? (fibonacci-sequence)))))

