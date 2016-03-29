(ns solutions-4clojure.easy
  (:require [clojure.set]))

;; 19. Last Element
;; Write a function which returns the last element in a sequence.
;; (= (__ [1 2 3 4 5]) 5)
;; (= (__ '(5 4 3)) 3)
;; (= (__ ["b" "c" "d"]) "d")
(defn last-element [s]
  (nth s (dec (count s))))

;; 20. Penultimate Element
;; Write a function which returns the second to last element from a sequence.
;; (= (__ (list 1 2 3 4 5)) 4)
;; (= (__ ["a" "b" "c"]) "b")
;; (= (__ [[1 2] [3 4]]) [1 2])
(defn penultimate-element [s]
  (nth s (- (count s) 2)))

;; 21. Nth Element
;; Write a function which returns the Nth element from a sequence.
;; (= (__ '(4 5 6 7) 2) 6)
;; (= (__ [:a :b :c] 0) :a)
;; (= (__ [1 2 3 4] 1) 2)
;; (= (__ '([1 2] [3 4] [5 6]) 2) [5 6])
(defn nth-element [s n]
  (last (take (inc n) s)))

;; 22. Count a Sequence
;; Write a function which returns the total number of elements in a sequence.
;; (= (__ '(1 2 3 3 1)) 5)
;; (= (__ "Hello World") 11)
;; (= (__ [[1 2] [3 4] [5 6]]) 3)
;; (= (__ '(13)) 1)
;; (= (__ '(:a :b :c)) 3)
(defn count-a-sequence [s]
  (reduce (fn [c _] (inc c)) 0 s))

;; 23. Reverse a Sequence
;; Write a function which reverses a sequence.
;; (= (__ [1 2 3 4 5]) [5 4 3 2 1])
;; (= (__ (sorted-set 5 7 2 7)) '(7 5 2))
;; (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])
(defn reverse-a-sequence [s]
  (reduce #(cons %2 %1) '() s))

;; 24. Sum It All Up
;; Write a function which returns the sum of a sequence of numbers.
;; (= (__ [1 2 3]) 6)
;; (= (__ (list 0 -2 5 5)) 8)
;; (= (__ #{4 2 1}) 7)
;; (= (__ '(0 0 -1)) -1)
;; (= (__ '(1 10 3)) 14)
(defn sum-it-all-up [s]
  (reduce + s))

;; 25. Find the odd numbers
;; Write a function which returns only the odd numbers from a sequence.
;; (= (__ #{1 2 3 4 5}) '(1 3 5))
;; (= (__ [4 2 1 6]) '(1))
;; (= (__ [2 2 4 6]) '())
;; (= (__ [1 1 1 3]) '(1 1 1 3))
(defn find-the-odd-numbers [s]
  (filter odd? s))

;; .26 Fibonacci Sequence
;; Write a function which returns the first X fibonacci numbers.
;; (= (__ 3) '(1 1 2))
;; (= (__ 6) '(1 1 2 3 5 8))
;; (= (__ 8) '(1 1 2 3 5 8 13 21))
(defn fibonacci-sequence [n]
  (letfn [(fib [a b] (lazy-seq (cons b (fib b (+ a b)))))]
    (take n (fib 0 1))))

;; 27. Palindrome Detector
;; Write a function which returns true if the given sequence is a palindrome.
;; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
;; (false? (__ '(1 2 3 4 5)))
;; (true? (__ "racecar"))
;; (true? (__ [:foo :bar :foo]))
;; (true? (__ '(1 1 3 3 1 1)))
;; (false? (__ '(:a :b :c)))
(defn palindrome-detector [s]
  (= (seq s) (reverse s)))

;; 29. Get the Caps
;; Write a function which takes a string and returns a new string containing only the capital letters.
;; (= (__ "HeLlO, WoRlD!") "HLOWRD")
;; (empty? (__ "nothing"))
;; (= (__ "$#A(*&987Zf") "AZ")
(defn get-the-caps [s]
  (apply str (re-seq #"[A-Z]+" s)))

;; 32. Duplicate a Sequence
;; Write a function which duplicates each element of a sequence.
;; (= (__ [1 2 3]) '(1 1 2 2 3 3))
;; (= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
;; (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
;; (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(defn duplicate-a-sequence [s]
  (reduce #(conj (conj %1 %2) %2) [] s))

;; 38. Maximum value
;; Write a function which takes a variable number of parameters and returns the maximum value.
;; (= (__ 1 8 3 4) 8)
;; (= (__ 30 20) 30)
;; (= (__ 45 67 11) 67)
(defn maximum-value [& params]
  (reduce
    (fn [x y]
      (if (< x y) y x))
    params))

;; 48. Intro to some
;; The some function takes a predicate function and a collection. It returns the first logical true value of (predicate x) where x is an item in the collection.
;; (= __ (some #{2 7 6} [5 6 7 8]))
;; (= __ (some #(when (even? %) %) [5 6 7 8]))
(def intro-to-some 6)

;; 28. Flatten a Sequence
;; Write a function which flattens a sequence.
;; (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
;; (= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
;; (= (__ '((((:a))))) '(:a))
(defn flatten-a-sequence [s]
  (reduce (fn myflatten [collection element]
            (if (sequential? element)
              (reduce myflatten collection element)
              (conj collection element))) [] s))

;; 30. Compress a Sequence
;; Write a function which removes consecutive duplicates from a sequence.
;; (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
;; (= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
;; (= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))
(defn compress-a-sequence [s]
  (map #(first %) (partition-by identity s)))

;; 31. Pack a Sequence
;; Write a function which packs consecutive duplicates into sub-lists.
;; (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
;; (= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
;; (= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))
(defn pack-a-sequence [s]
  (partition-by identity s))

;; 33. Replicate a Sequence
;; Write a function which replicates each element of a sequence a variable number of times.
;; (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
;; (= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
;; (= (__ [4 5 6] 1) '(4 5 6))
;; (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
;; (= (__ [44 33] 2) [44 44 33 33])
(defn replicate-a-sequence [s n]
  (apply concat (map #(repeat n %) s)))

;; 34. Implement range
;; Write a function which creates a list of all integers in a given range.
;; (= (__ 1 4) '(1 2 3))
;; (= (__ -2 2) '(-2 -1 0 1))
;; (= (__ 5 8) '(5 6 7))
(defn implement-range [start end]
  (take (- end start) (iterate inc start)))

;; 39. Interleave Two Seqs
;; Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.
;; (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
;; (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
;; (= (__ [1 2 3 4] [5]) [1 5])
;; (= (__ [30 20] [25 15]) [30 25 20 15])
(defn interleave-two-seqs [a b]
  (if (<= (count a) (count b))
    (flatten (map-indexed (fn [index item] [item (nth b index)]) a))
    (flatten (map-indexed (fn [index item] [item (nth b index)]) (take (count b) a)))))

;; 40. Interpose a Seq
;; Write a function which separates the items of a sequence by an arbitrary value.
;; (= (__ 0 [1 2 3]) [1 0 2 0 3])
;; (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
;; (= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])
(defn interpose-a-seq [v s]
  (rest (mapcat #(list v %) s)))

;; 41. Drop Every Nth Item
;; Write a function which drops every Nth item from a sequence.
;; (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
;; (= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
;; (= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6])
(defn drop-every-nth-item [s n]
  (keep-indexed #(if (not= (mod %1 n) (dec n)) %2) s))

;; 42. Factorial Fun
;; Write a function which calculates factorials.
;; (= (__ 1) 1)
;; (= (__ 3) 6)
;; (= (__ 5) 120)
;; (= (__ 8) 40320)
(defn factorial-fun [n]
  (reduce * (range 1 (+ n 1))))

;; 45. Intro to Iterate
;; The iterate function can be used to produce an infinite lazy sequence.
;; (= __ (take 5 (iterate #(+ 3 %) 1)))
(def intro-to-iterate [1 4 7 10 13])

;; 47. Contain Yourself
;; The contains? function checks if a KEY is present in a given collection. This often leads beginner clojurians to use it incorrectly with numerically indexed collections like vectors and lists.
;; (contains? #{4 5 6} __)
;; (contains? [1 1 1 1 1] __)
;; (contains? {4 :a 2 :b} __)
;; (not (contains? [1 2 4] __))
(def contain-yourself 4)

;; 49. Split a sequence
;; Write a function which will split a sequence into two parts.
;; Do not use split-at.
;; (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
;; (= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
;; (= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])
(defn split-a-sequence [n s]
  (list (take n s) (drop n s)))

;; 51. Advanced Destructuring
;; Here is an example of some more sophisticated destructuring.
;; (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))
(def advanced-destructuring [1 2 3 4 5])

;; 61. Map Construction
;; Write a function which takes a vector of keys and a vector of values and constructs a map from them.
;; (= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
;; (= (__ [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
;; (= (__ [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})
(defn map-construction [keys values]
  (apply assoc {} (interleave keys values)))

;; 62. Re-implement Iterate
;; Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
;; (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
;; (= (take 100 (__ inc 0)) (take 100 (range)))
;; (= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))
(defn re-implement-iterate [f x]
  (cons x (lazy-seq (re-implement-iterate f (f x)))))

;; 63. Group a Sequence
;; Given a function f and a sequence s, write a function which returns a map. The keys should be the values of f applied to each item in s. The value at each key should be a vector of corresponding items in the order they appear in s.
;; (= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
;; (= (__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
;;    {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
;; (= (__ count [[1] [1 2] [3] [1 2 3] [2 3]])
;;    {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})
(defn group-a-seequence [f vals]
  (into {}
        (map #(vector (f (first %)) (vec %))
             (partition-by f (sort vals)))))

;; 66. Greatest Common Divisor
;; Given two integers, write a function which returns the greatest common divisor.
;; (= (__ 2 4) 2)
;; (= (__ 10 5) 5)
;; (= (__ 5 7) 1)
;; (= (__ 1023 858) 33)
(defn greatest-common-divisor [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))

;; 81. Set Intersection
;; Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set has in common.
;; (= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
;; (= (__ #{0 1 2} #{3 4 5}) #{})
;; (= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})
(defn set-intersection [a b]
  (set (filter a b)))

;; 83. A Half-Truth
;; Write a function which takes a variable number of booleans. Your function should return true if some of the parameters are true, but not all of the parameters are true. Otherwise your function should return false.
;; (= false (__ false false))
;; (= true (__ true false))
;; (= false (__ true))
;; (= true (__ false true false))
;; (= false (__ true true true))
;; (= true (__ true true true false))
(defn a-half-truth [& booleans]
  (= (set booleans) #{true false}))

;; 88. Symmetric Difference
;; Write a function which returns the symmetric difference of two sets. The symmetric difference is the set of items belonging to one but not both of the two sets.
;; (= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
;; (= (__ #{:a :b :c} #{}) #{:a :b :c})
;; (= (__ #{} #{4 5 6}) #{4 5 6})
;; (= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})
(defn symmetric-difference [a b]
  (clojure.set/difference
    (clojure.set/union a b)
    (clojure.set/intersection a b)))

;; 90. Cartesian Product
;; Write a function which calculates the Cartesian product of two sets.
;; (= (__ #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
;;   #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
;;     ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
;;     ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})
;; (= (__ #{1 2 3} #{4 5})
;;   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
;; (= 300 (count (__ (into #{} (range 10))
;;                  (into #{} (range 30)))))
(defn cartesian-product [a b]
  (into #{}
        (for [x a y b] (vector x y))))

;; 95. To Tree, or not to Tree
;; Write a predicate which checks whether or not a given sequence represents a binary tree. Each node in the tree must have a value, a left child, and a right child.
;; (= (__ '(:a (:b nil nil) nil))
;;    true)
;; (= (__ '(:a (:b nil nil)))
;;    false)
;; (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]])
;;    true)
;; (= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]])
;;    false)
;; (= (__ [1 [2 [3 [4 nil nil] nil] nil] nil])
;;    true)
;; (= (__ [1 [2 [3 [4 false nil] nil] nil] nil])
;;    false)
;; (= (__ '(:a nil ()))
;;    false)
(defn to-tree-or-not-to-tree? [s]
  (and
    (sequential? s)
    (= (count s) 3)
    (let [left (second s)
          right (last s)]
      (and
        (or
          (nil? left)
          (to-tree-or-not-to-tree? left))
        (or
          (nil? right)
          (to-tree-or-not-to-tree? right))))))

;; 96. Beauty is Symmetry
;; Let us define a binary tree as "symmetric" if the left half of the tree is the mirror image of the right half of the tree. Write a predicate to determine whether or not a given binary tree is symmetric. (see To Tree, or not to Tree for a reminder on the tree representation we're using).
;; (= (__ '(:a (:b nil nil) (:b nil nil))) true)
;; (= (__ '(:a (:b nil nil) nil)) false)
;; (= (__ '(:a (:b nil nil) (:c nil nil))) false)
;; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;;         [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
;;    true)
;; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;;         [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
;;    false)
;; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;;         [2 [3 nil [4 [6 nil nil] nil]] nil]])
;;    false)
(defn beauty-is-symmetry [s] {:pre [(sequential? s)
                                    (= (count s) 3)]}
  (let [left (second s)
        right (last s)
        mirror (fn mirror [s] {:pre [(= (count s) 3)]}
                 (let [left (second s)
                       right (last s)]
                   (if (and (nil? left) (nil? right))
                     s
                     (list
                       (first s)
                       (if (sequential? right) (mirror right) right)
                       (if (sequential? left) (mirror left) left)))))]
    (=
      left
      (if (sequential? right)
        (mirror right)
        right))))

;; 97. Pascal's Triangle
;; Pascal's triangle is a triangle of numbers computed using the following rules:
;; - The first row is 1.
;; - Each successive row is computed by adding together adjacent numbers in the row above, and adding a 1 to the beginning and end of the row.
;; Write a function which returns the nth row of Pascal's Triangle.
;; (= (__ 1) [1])
;; (= (map __ (range 1 6))
;;    [     [1]
;;         [1 1]
;;        [1 2 1]
;;       [1 3 3 1]
;;      [1 4 6 4 1]])
;; (= (__ 11)
;;    [1 10 45 120 210 252 210 120 45 10 1])
(defn pascals-triangle [n]
  (last
    (take n
          (iterate
            (fn next-row [previous-row]
              (into []
                    (map (fn [e] (reduce + e))
                         (partition 2 1
                                    (conj (into [0] previous-row) 0)))))
            [1]))))


;; 99. Product Digits
;; Write a function which multiplies two numbers and returns the result as a sequence of its digits.
;; (= (__ 1 1) [1])
;; (= (__ 99 9) [8 9 1])
;; (= (__ 999 99) [9 8 9 0 1])
(defn product-digits [a b]
  (map #(Character/getNumericValue %)
       (str (* a b))))

;; 100. Least Common Multiple
;; Write a function which calculates the least common multiple. Your function should accept a variable number of positive integers or ratios.
;; (== (__ 2 3) 6)
;; (== (__ 5 3 7) 105)
;; (== (__ 1/3 2/5) 2)
;; (== (__ 3/4 1/6) 3/2)
;; (== (__ 7 5/7 2 3/5) 210)
(defn least-common-multiple [& n]
  (letfn [(gcd [a b]
            (if (= b 0)
              a
              (recur b (mod a b))))
          (lcm [a b]
            (/ (* a b) (gcd a b)))]
    (reduce lcm n)))

;; 107. Simple closures
;; Lexical scope and first-class functions are two of the most basic building blocks of a functional language like Clojure. When you combine the two together, you get something very powerful called lexical closures. With these, you can exercise a great deal of control over the lifetime of your local bindings, saving their values for use later, long after the code you're running now has finished.
;; It can be hard to follow in the abstract, so let's build a simple closure. Given a positive integer n, return a function (f x) which computes xn. Observe that the effect of this is to preserve the value of n for use outside the scope in which it is defined.
;; (= 256 ((__ 2) 16),
;;   ((__ 8) 2))
;; (= [1 8 27 64] (map (__ 3) [1 2 3 4]))
;; (= [1 2 4 8 16] (map #((__ %) 2) [0 1 2 3 4]))
(defn simple-closures [n]
  (fn exp [x]
    (long (Math/pow x n))))

;; 118. Re-implement Map
;; Map is one of the core elements of a functional programming language. Given a function f and an input sequence s, return a lazy sequence of (f x) for each element x in s.
;; (= [3 4 5 6 7]
;;   (__ inc [2 3 4 5 6]))
;; (= (repeat 10 nil)
;;   (__ (fn [_] nil) (range 10)))
;; (= [1000000 1000001]
;;   (->> (__ inc (range))
;;        (drop (dec 1000000))
;;      (take 2)))
(defn re-implement-map [f c]
  (if (not (empty? c))
    (lazy-seq
      (cons (f (first c))
            (re-implement-map f (rest c))))))

;; 120. Sum of square of digits
;; Write a function which takes a collection of integers as an argument. Return the count of how many elements are smaller than the sum of their squared component digits. For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared.
;; (= 8 (__ (range 10)))
;; (= 19 (__ (range 30)))
;; (= 50 (__ (range 100)))
;; (= 50 (__ (range 1000)))
(defn sum-of-square-digits [c]
  (count
    (filter #(< (first %) (second %))
            (map (fn [d]
                   (vector (first d)
                           (reduce + (map #(* % %) (second d)))))
                 (map (fn [e]
                        (vector e (map #(Character/digit % 10) (str e))))
                      c)))))

;; 122. Read a binary number
;; Convert a binary number, provided in the form of a string, to its numerical value.
;; (= 0     (__ "0"))
;; (= 7     (__ "111"))
;; (= 8     (__ "1000"))
;; (= 9     (__ "1001"))
;; (= 255   (__ "11111111"))
;; (= 1365  (__ "10101010101"))
;; (= 65535 (__ "1111111111111111"))
(defn read-a-binary-number [s]
  (int
    (reduce +
            (map-indexed #(* %2 (Math/pow 2 %1))
                         (map #(Character/digit % 10)
                              (reverse s))))))

;; 126. Through the Looking Class
;; Enter a value which satisfies the following:
;; (let [x __]
;;   (and (= (class x) x) x))
(def through-the-looking-class java.lang.Class)

;; 128. Recognize Playing Cards
;; A standard American deck of playing cards has four suits - spades, hearts, diamonds, and clubs - and thirteen cards in each suit. Two is the lowest rank, followed by other integers up to ten; then the jack, queen, king, and ace.
;; It's convenient for humans to represent these cards as suit/rank pairs, such as H5 or DQ: the heart five and diamond queen respectively. But these forms are not convenient for programmers, so to write a card game you need some way to parse an input string into meaningful components. For purposes of determining rank, we will define the cards to be valued from 0 (the two) to 12 (the ace)
;; Write a function which converts (for example) the string "SJ" into a map of {:suit :spade, :rank 9}. A ten will always be represented with the single character "T", rather than the two characters "10".
;; (= {:suit :diamond :rank 10} (__ "DQ"))
;; (= {:suit :heart :rank 3} (__ "H5"))
;; (= {:suit :club :rank 12} (__ "CA"))
;; (= (range 13) (map (comp :rank __ str)
;;                    '[S2 S3 S4 S5 S6 S7
;;                      S8 S9 ST SJ SQ SK SA]))
(defn recognize-playing-cards [s]
  {:suit ({\S :spades \D :diamond \H :heart \C :club}
           (first s))
   :rank ({\2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6 \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12}
           (second s))})

;; 135. Infix Calculator
;; Your friend Joe is always whining about Lisps using the prefix notation for math. Show him how you could easily write a function that does math using the infix notation. Is your favorite language that flexible, Joe? Write a function that accepts a variable length mathematical expression consisting of numbers and the operations +, -, *, and /. Assume a simple calculator that does not do precedence and instead just calculates left to right.
;; (= 7  (__ 2 + 5))
;; (= 42 (__ 38 + 48 - 2 / 2))
;; (= 8  (__ 10 / 2 - 1 * 2))
;; (= 72 (__ 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))
(defn infix-calculator [a op b & more]
  (letfn [(infix [a op b] (op a b))]
    (if (zero? (count more))
      (infix a op b)
      (recur (infix a op b) (first more) (second more) (drop 2 more)))))

;; 143. dot product
;; Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length.
;; (= 0 (__ [0 1 0] [1 0 0]))
;; (= 3 (__ [1 1 1] [1 1 1]))
;; (= 32 (__ [1 2 3] [4 5 6]))
;; (= 256 (__ [2 5 6] [100 10 1]))
(defn dot-product [a b]
  (reduce + (map * a b)))

;; 146. Trees into tables
;; Because Clojure's for macro allows you to "walk" over multiple sequences in a nested fashion, it is excellent for transforming all sorts of sequences. If you don't want a sequence as your final output (say you want a map), you are often still best-off using for, because you can produce a sequence and feed it into a map, for example.
;; For this problem, your goal is to "flatten" a map of hashmaps. Each key in your output map should be the "path" that you would have to take in the original map to get to a value, so for example {1 {2 3}} should result in {[1 2] 3}. You only need to flatten one level of maps: if one of the values is a map, just leave it alone.
;; That is, (get-in original [k1 k2]) should be the same as (get result [k1 k2])
;; (= (__ '{a {p 1, q 2}
;;          b {m 3, n 4}})
;;    '{[a p] 1, [a q] 2
;;      [b m] 3, [b n] 4})
;; (= (__ '{[1] {a b c d}
;;          [2] {q r s t u v w x}})
;;    '{[[1] a] b, [[1] c] d,
;;      [[2] q] r, [[2] s] t,
;;      [[2] u] v, [[2] w] x})
;; (= (__ '{m {1 [a b c] 3 nil}})
;;    '{[m 1] [a b c], [m 3] nil})
(defn trees-into-tables [m]
  (into {}
        (apply concat
               (for [[k v] m]
                 (map #(vector [k (first %)] (second %)) v)))))

;; 147. Pascal's Trapezoid
;; Write a function that, for any given input vector of numbers, returns an infinite lazy sequence of vectors, where each next one is constructed from the previous following the rules used in Pascal's Triangle. For example, for [3 1 2], the next row is [3 4 3 2].
;; Beware of arithmetic overflow! In clojure (since version 1.3 in 2011), if you use an arithmetic operator like + and the result is too large to fit into a 64-bit integer, an exception is thrown. You can use +' to indicate that you would rather overflow into Clojure's slower, arbitrary-precision bigint.
;; (= (second (__ [2 3 2])) [2 5 5 2])
;; (= (take 5 (__ [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
;; (= (take 2 (__ [3 1 2])) [[3 1 2] [3 4 3 2]])
;; (= (take 100 (__ [2 4 2])) (rest (take 101 (__ [2 2]))))
(defn pascals-trapezoid [row]
  (iterate
    (fn next-row [previous-row]
      (into []
            (map (fn [e] (reduce +' e))
                 (partition 2 1
                            (conj (into [0] previous-row) 0)))))
    row))

;; 153. Pairwise Disjoint Sets
;; Given a set of sets, create a function which returns true if no two of those sets have any elements in common and false otherwise. Some of the test cases are a bit tricky, so pay a little more attention to them.
;; Such sets are usually called pairwise disjoint or mutually disjoint.
;; (= (__ #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
;;    true)
;; (= (__ #{#{:a :b :c :d :e}
;;          #{:a :b :c :d}
;;          #{:a :b :c}
;;          #{:a :b}
;;          #{:a}})
;;    false)
;; (= (__ #{#{[1 2 3] [4 5]}
;;          #{[1 2] [3 4 5]}
;;          #{[1] [2] 3 4 5}
;;          #{1 2 [3 4] [5]}})
;;    true)
;; (= (__ #{#{'a 'b}
;;          #{'c 'd 'e}
;;          #{'f 'g 'h 'i}
;;          #{''a ''c ''f}})
;;    true)
;; (= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
;;          #{#{:x :y :z} #{:x :y} #{:z} #{}}
;;          #{'[:x :y :z] [:x :y] [:z] [] {}}})
;;    false)
;; (= (__ #{#{(= "true") false}
;;          #{:yes :no}
;;          #{(class 1) 0}
;;          #{(symbol "true") 'false}
;;          #{(keyword "yes") ::no}
;;          #{(class '1) (int \0)}})
;;    false)
;; (= (__ #{#{distinct?}
;;          #{#(-> %) #(-> %)}
;;          #{#(-> %) #(-> %) #(-> %)}
;;          #{#(-> %) #(-> %) #(-> %)}})
;;    true)
;; (= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
;;          #{'+ '* mapcat (comment mapcat)}
;;          #{(do) set contains? nil?}
;;          #{, , , #_, , empty?}})
;;    false)
(defn pairwise-disjoint-sets [s]
  (=
    (count
      (apply concat '() s))
    (count
      (apply clojure.set/union s))))

;; 157. Indexing Sequences
;; Transform a sequence into a sequence of pairs containing the original elements along with their index.
;; (= (__ [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
;; (= (__ [0 1 3]) '((0 0) (1 1) (3 2)))
;; (= (__ [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])
(defn indexing-sequences [s]
  (map-indexed (fn [index element] [element index]) s))

;; 166. Comparisons
;; For any orderable data type it's possible to derive all of the basic comparison operations (<, ≤, =, ≠, ≥, and >) from a single operation (any operator but = or ≠ will work). Write a function that takes three arguments, a less than operator for the data and two items to compare. The function should return a keyword describing the relationship between the two items. The keywords for the relationship between x and y are as follows:
;; x = y → :eq
;; x > y → :gt
;; x < y → :lt
;; (= :gt (__ < 5 1))
;; (= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))
;; (= :lt (__ (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
;; (= :gt (__ > 0 2
(defn comparisons [f l r]
  (cond
    (= (f l r) (f r l)) :eq
    (f l r) :lt
    :else :gt))

;; 173. Intro to Destructuring 2
;; Sequential destructuring allows you to bind symbols to parts of sequential things (vectors, lists, seqs, etc.): (let [bindings* ] exprs*) Complete the bindings so all let-parts evaluate to 3.
;; (= 3
;;    (let [[__] [+ (range 3)]] (apply __))
;;    (let [[[__] b] [[+ 1] 2]] (__ b))
;;    (let [[__] [inc 2]] (__)))
; f x
