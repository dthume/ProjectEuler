(ns com.westoya.euler.problems)

(defn natural-numbers [] (iterate inc 0))

(def *natural-numbers* (natural-numbers))

(defn problem1
  []
  (reduce +
	  (filter #(or (= 0 (mod %1 3))
		       (= 0 (mod %1 5)))
		  (take 1000 *natural-numbers*))))

