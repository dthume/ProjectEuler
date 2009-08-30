(ns com.westoya.euler.problems
  (:use
   [clojure.contrib.combinatorics :only (lex-permutations)]
   [clojure.contrib.duck-streams :only (read-lines)]
   [clojure.contrib.lazy-seqs :only (primes)]
   [clojure.contrib.math :only (expt)]
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

(defn parse-digit
  "Parses a digit from character c"
  [c]
  (Integer/parseInt (.toString c)))

(defn sum-string-digits
  [s]
  (apply + (map parse-digit (.toString s))))

(defn reverse-string
  [s]
  (.toString (.reverse (StringBuilder. s))))

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
  (let [s "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
	sumseq #(* %1 (parse-digit %2))]
    (reduce max
	    (map #(reduce sumseq 1 %)
		 (partition 5 1 s)))))

(defn problem10
  "Calculate the sum of all the primes below two million."
  []
  (reduce + (take-while #(< % 2000000) primes)))

(defn problem13
  "Find the first ten digits of the sum of one-hundred 50-digit numbers."
  []
  (let [numbers [37107287533902102798797998220837590246510135740250
		 46376937677490009712648124896970078050417018260538
		 74324986199524741059474233309513058123726617309629
		 91942213363574161572522430563301811072406154908250
		 23067588207539346171171980310421047513778063246676
		 89261670696623633820136378418383684178734361726757
		 28112879812849979408065481931592621691275889832738
		 44274228917432520321923589422876796487670272189318
		 47451445736001306439091167216856844588711603153276
		 70386486105843025439939619828917593665686757934951
		 62176457141856560629502157223196586755079324193331
		 64906352462741904929101432445813822663347944758178
		 92575867718337217661963751590579239728245598838407
		 58203565325359399008402633568948830189458628227828
		 80181199384826282014278194139940567587151170094390
		 35398664372827112653829987240784473053190104293586
		 86515506006295864861532075273371959191420517255829
		 71693888707715466499115593487603532921714970056938
		 54370070576826684624621495650076471787294438377604
		 53282654108756828443191190634694037855217779295145
		 36123272525000296071075082563815656710885258350721
		 45876576172410976447339110607218265236877223636045
		 17423706905851860660448207621209813287860733969412
		 81142660418086830619328460811191061556940512689692
		 51934325451728388641918047049293215058642563049483
		 62467221648435076201727918039944693004732956340691
		 15732444386908125794514089057706229429197107928209
		 55037687525678773091862540744969844508330393682126
		 18336384825330154686196124348767681297534375946515
		 80386287592878490201521685554828717201219257766954
		 78182833757993103614740356856449095527097864797581
		 16726320100436897842553539920931837441497806860984
		 48403098129077791799088218795327364475675590848030
		 87086987551392711854517078544161852424320693150332
		 59959406895756536782107074926966537676326235447210
		 69793950679652694742597709739166693763042633987085
		 41052684708299085211399427365734116182760315001271
		 65378607361501080857009149939512557028198746004375
		 35829035317434717326932123578154982629742552737307
		 94953759765105305946966067683156574377167401875275
		 88902802571733229619176668713819931811048770190271
		 25267680276078003013678680992525463401061632866526
		 36270218540497705585629946580636237993140746255962
		 24074486908231174977792365466257246923322810917141
		 91430288197103288597806669760892938638285025333403
		 34413065578016127815921815005561868836468420090470
		 23053081172816430487623791969842487255036638784583
		 11487696932154902810424020138335124462181441773470
		 63783299490636259666498587618221225225512486764533
		 67720186971698544312419572409913959008952310058822
		 95548255300263520781532296796249481641953868218774
		 76085327132285723110424803456124867697064507995236
		 37774242535411291684276865538926205024910326572967
		 23701913275725675285653248258265463092207058596522
		 29798860272258331913126375147341994889534765745501
		 18495701454879288984856827726077713721403798879715
		 38298203783031473527721580348144513491373226651381
		 34829543829199918180278916522431027392251122869539
		 40957953066405232632538044100059654939159879593635
		 29746152185502371307642255121183693803580388584903
		 41698116222072977186158236678424689157993532961922
		 62467957194401269043877107275048102390895523597457
		 23189706772547915061505504953922979530901129967519
		 86188088225875314529584099251203829009407770775672
		 11306739708304724483816533873502340845647058077308
		 82959174767140363198008187129011875491310547126581
		 97623331044818386269515456334926366572897563400500
		 42846280183517070527831839425882145521227251250327
		 55121603546981200581762165212827652751691296897789
		 32238195734329339946437501907836945765883352399886
		 75506164965184775180738168837861091527357929701337
		 62177842752192623401942399639168044983993173312731
		 32924185707147349566916674687634660915035914677504
		 99518671430235219628894890102423325116913619626622
		 73267460800591547471830798392868535206946944540724
		 76841822524674417161514036427982273348055556214818
		 97142617910342598647204516893989422179826088076852
		 87783646182799346313767754307809363333018982642090
		 10848802521674670883215120185883543223812876952786
		 71329612474782464538636993009049310363619763878039
		 62184073572399794223406235393808339651327408011116
		 66627891981488087797941876876144230030984490851411
		 60661826293682836764744779239180335110989069790714
		 85786944089552990653640447425576083659976645795096
		 66024396409905389607120198219976047599490197230297
		 64913982680032973156037120041377903785566085089252
		 16730939319872750275468906903707539413042652315011
		 94809377245048795150954100921645863754710598436791
		 78639167021187492431995700641917969777599028300699
		 15368713711936614952811305876380278410754449733078
		 40789923115535562561142322423255033685442488917353
		 44889911501440648020369068063960672322193204149535
		 41503128880339536053299340368006977710650566631954
		 81234880673210146739058568557934581403627822703280
		 82616570773948327592232845941706525094512325230608
		 22918802058777319719839450180888072429661980811197
		 77158542502016545090413245809786882778948721859617
		 72107838435069186155435662884062257473692284509516
		 20849603980134001723930671666823555245252804609722
		 53503534226472524250874054075591789781264330331690]]
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

(defn problem48
  "Find the last ten digits of 11 + 22 + ... + 10001000."
  []
  (let [total (.toString
	       (reduce + (map #(expt %1 %1) (range 1 1001))))]
    (apply str (drop (- (count total) 10) total))))

(defn problem53
  "How many values of C(n,r), for 1 <= n <= 100, exceed one-million?"
  []
  (let [n-range (range 1 101)
	r-for-n (fn [n]
		  (map #(vector n %1) (range 1 (inc n))))
	total-combinations
	(fn [pair]
	  (let [n (long (first pair))
		r (long (second pair))]
	    (/ (factorial n)
	       (* (factorial r)
		  (factorial (- n r))))))]
    (count
     (filter #(> % 1000000)
	     (map total-combinations
		  (mapcat r-for-n n-range))))))

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
  (println (problem13))
  (println (problem14))
  (println (problem16))
  (println (problem18))
  (println (problem20))
  (println (problem22))
  (println (problem24))
  (println (problem25))
  (println (problem29))
  (println (problem36))
  (println (problem48))
  (println (problem53))
  (println (problem56))
  (println (problem67))

)
