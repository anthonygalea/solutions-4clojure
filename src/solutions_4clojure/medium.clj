(ns solutions-4clojure.medium)

;; 43. Reverse Interleave
;; Write a function which reverses the interleave process into x number of subsequences.
;(= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
;(= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
;(= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))
(defn reverse-interleave [s n]
  ((fn reverse-interleave [s n i]
     (if (= i 1)
       (list (take-nth n s))
       (cons (take-nth n s) (reverse-interleave (rest s) n (dec i)))))
   s n n))

;; 44. Rotate Sequence
;; Write a function which can rotate a sequence in either direction.
;; (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
;; (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
;; (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
;; (= (__ 1 '(:a :b :c)) '(:b :c :a))
;; (= (__ -4 '(:a :b :c)) '(:c :a :b))
(defn rotate-sequence [n s]
  (let [c (mod n (count s))]
    (concat (drop c s) (take c s))))

;; 46. Flipping out
;; Write a higher-order function which flips the order of the arguments of an input function.
;; (= 3 ((__ nth) 2 [1 2 3 4 5]))
;; (= true ((__ >) 7 8))
;; (= 4 ((__ quot) 2 8))
;; (= [1 2 3] ((__ take) [1 2 3 4 5] 3))
(defn flipping-out [f]
  (fn [a b]
    (f b a)))

;; 50. Split by Type
;; Write a function which takes a sequence consisting of items with different types and splits them up into a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).
;; (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
;; (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
;; (= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})
(defn split-by-type [v]
  (vals (group-by #(type %) v)))

;; 54. Partition a Sequence
;; Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.
;; (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
;; (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
;; (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))
(defn partition-a-sequence [n s]
  (map #(take n (drop % s))
       (range 0
              (* (quot (count s) n) n)
              n)))

;; 55. Count Occurrences
;; Write a function which returns a map containing the number of occurrences of each distinct item in a sequence.
;; (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
;; (= (__ [:b :a :b :a :b]) {:a 2, :b 3})
;; (= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})
(defn count-occurrences [s]
  (into {} (map #(vector (first %) (count (second %))) (group-by identity s))))

;; 56. Find Distinct Items
;; Write a function which removes the duplicates from a sequence. Order of the items must be maintained.
;; (= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
;; (= (__ [:a :a :b :b :c :c]) [:a :b :c])
;; (= (__ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
;; (= (__ (range 50)) (range 50))
(defn find-distinct-items [s]
  (sort-by #(.indexOf s %)
           (keys (group-by identity s))))

;; 58. Function Composition
;; Write a function which allows you to create function compositions. The parameter list should take a variable number of functions, and create a function that applies them from right-to-left.
;; (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
;; (= 5 ((__ (partial + 3) second) [1 2 3 4]))
;; (= true ((__ zero? #(mod % 8) +) 3 5 7 9))
;; (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))
(defn function-composition [& functions]
  (fn [& args]
    (first
      (reduce (fn [result function]
                (list (apply function result)))
              args
              (reverse functions)))))

;; 59. Juxtaposition
;; Take a set of functions and return a new function that takes a variable number of arguments and returns a sequence containing the result of applying each function left-to-right to the argument list.
;; (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
;; (= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello"))
;; (= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))
(defn juxtaposition [& functions]
  (fn [& parameters]
    (map #(apply % parameters) functions)))

;; 60. Sequence Reductions
;; Write a function which behaves like reduce, but returns each intermediate value of the reduction. Your function must accept either two or three arguments, and the return sequence must be lazy.
;; (= (take 5 (__ + (range))) [0 1 3 6 10])
;; (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
;; (= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)
(defn sequence-reductions
  ([f s]
   (sequence-reductions f (first s) (rest s)))
  ([f i s]
   (cons i
         (lazy-seq
           (if (not (empty? s))
             (sequence-reductions f
                                  (f i (first s))
                                  (rest s)))))))

;; 65. Black Box Testing
;; Clojure has many sequence types, which act in subtly different ways. The core functions typically convert them into a uniform "sequence" type and work with them that way, but it can be important to understand the behavioral and performance differences so that you know which kind is appropriate for your application.
;; Write a function which takes a collection and returns one of :map, :set, :list, or :vector - describing the type of collection it was given.
;; You won't be allowed to inspect their class or use the built-in predicates like list? - the point is to poke at them and understand their behavior.
;; (= :map (__ {:a 1, :b 2}))
;; (= :list (__ (range (rand-int 20))))
;; (= :vector (__ [1 2 3 4 5 6]))
;; (= :set (__ #{10 (rand-int 5)}))
;; (= [:map :set :vector :list] (map __ [{} #{} [] ()]))
(defn black-box-testing [s]
  (let [result (conj (empty s) [1 2] [1 2] [1 3])]
    (cond
      (= 1 (count result)) :map
      (= 2 (count result)) :set
      (= [1 2] (first result)) :vector
      :else :list)))

;; 67. Prime Numbers
;; Write a function which returns the first x number of prime numbers.
;; (= (__ 2) [2 3])
;; (= (__ 5) [2 3 5 7 11])
;; (= (last (__ 100)) 541)
(defn prime-numbers [n]
  (take n
        (filter #(.isProbablePrime (BigInteger/valueOf %) 10)
                (range))))

;; 69. Merge with a Function
;; Write a function which takes a function f and a variable number of maps. Your function should return a map that consists of the rest of the maps conj-ed onto the first. If a key occurs in more than one map, the mapping(s) from the latter (left-to-right) should be combined with the mapping in the result by calling (f val-in-result val-in-latter)
;; (= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
;;    {:a 4, :b 6, :c 20})
;; (= (__ - {1 10, 2 20} {1 3, 2 10, 3 15})
;;    {1 7, 2 10, 3 15})
;; (= (__ concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
;; {:a [3 4 5], :b [6 7], :c [8 9]})
(defn merge-with-a-function [f & m]
  (into {}
        (map (fn [e]
               (if (> (count (val e)) 1)
                 [(key e) (reduce f (map second (val e)))]
                 [(key e) (second (first (val e)))]))
             (group-by first (apply concat m)))))

;; 70. Word Sorting
;; Write a function that splits a sentence up into a sorted list of words. Capitalization should not affect sort order and punctuation should be ignored.
;; (= (__  "Have a nice day.")
;;    ["a" "day" "Have" "nice"])
;; (= (__  "Clojure is a fun language!")
;;    ["a" "Clojure" "fun" "is" "language"])
;; (= (__  "Fools fall for foolish follies.")
;;    ["fall" "follies" "foolish" "Fools" "for"])
(defn word-sorting [s]
  (into []
        (sort-by clojure.string/lower-case
                 (clojure.string/split (apply str
                                              (take (dec (count s)) s))
                                       #"\s"))))

;; 74. Filter Perfect Squares
;; Given a string of comma separated integers, write a function which returns a new comma separated string that only contains the numbers which are perfect squares.
;; (= (__ "4,5,6,7,8,9") "4,9")
;; (= (__ "15,16,25,36,37") "16,25,36")
(defn filter-perfect-squares [s]
  (letfn [(perfect-square? [n]
            (== (Math/sqrt n) (int (Math/sqrt n))))]
    (clojure.string/join ","
                         (filter perfect-square?
                                 (map read-string (clojure.string/split s #","))))))

;; 75. Euler's Totient Function
;; Two numbers are coprime if their greatest common divisor equals 1. Euler's totient function f(x) is defined as the number of positive integers less than x which are coprime to x. The special case f(1) equals 1. Write a function which calculates Euler's totient function.
;; (= (__ 1) 1)
;; (= (__ 10) (count '(1 3 7 9)) 4)
;; (= (__ 40) 16)
;; (= (__ 99) 60)
(defn eulers-totient-function [n]
  (letfn [(gcd [a b]
            (if (= b 0)
              a
              (recur b (mod a b))))]
    (count
      (filter #(= 1 (gcd n %)) (range n)))))

;; 76. Intro to Trampoline
;; The trampoline function takes a function f and a variable number of parameters. Trampoline calls f with any parameters that were supplied. If f returns a function, trampoline calls that function with no arguments. This is repeated, until the return value is not a function, and then trampoline returns that non-function value. This is useful for implementing mutually recursive algorithms in a way that won't consume the stack.
;; (= __
;;    (letfn
;;      [(foo [x y] #(bar (conj x y) y))
;;       (bar [x y] (if (> (last x) 10)
;;                    x
;;                    #(foo x (+ 2 y))))]
;;      (trampoline foo [] 1)))
(def intro-to-trampoline [1 3 5 7 9 11])

;; 77. Anagram Finder
;; Write a function which finds all the anagrams in a vector of words. A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y. Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other. Each sub-set should have at least two words. Words without any anagrams should not be included in the result.
;; (= (__ ["meat" "mat" "team" "mate" "eat"])
;;    #{#{"meat" "team" "mate"}})
;; (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
;;    #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})
(defn anagram-finder [s]
  (->> (group-by sort s)
       (filter #(> (count (val %)) 1))
       (map #(set (val %)))
       (set)))

;; 78. Reimplement Trampoline
;; Reimplement the function described in "Intro to Trampoline".
;; (= (letfn [(triple [x] #(sub-two (* 3 x)))
;;            (sub-two [x] #(stop?(- x 2)))
;;            (stop? [x] (if (> x 50) x #(triple x)))]
;;      (__ triple 2))
;;    82)
;; (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
;;            (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
;;      (map (partial __ my-even?) (range 6)))
;;    [true false true false true false])
(defn reimplement-trampoline
  ([f]
   (let [r (f)]
     (if (fn? r)
       (recur r)
       r)))
  ([f & args]
   (reimplement-trampoline #(apply f args))))

;; 80. Perfect Numbers
;; A number is "perfect" if the sum of its divisors equal the number itself. 6 is a perfect number because 1+2+3=6. Write a function which returns true for perfect numbers and false otherwise.
;; (= (__ 6) true)
;; (= (__ 7) false)
;; (= (__ 496) true)
;; (= (__ 500) false)
;; (= (__ 8128) true)
(defn perfect-numbers [n]
  (= n
     (reduce +
             (filter #(zero? (mod n %))
                     (range 1 (inc (/ n 2)))))))

;; 85. Power Set
;; Write a function which generates the power set of a given set. The power set of a set x is the set of all subsets of x, including the empty set and x itself.
;; (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
;; (= (__ #{}) #{#{}})
;; (= (__ #{1 2 3})
;;    #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
;; (= (count (__ (into #{} (range 10)))) 1024)
(defn power-set [s]
  (reduce (fn [result next]
            (clojure.set/union result
                               (map #(conj % next)
                                    result)))
          #{#{}}
          s))

;; 86. Happy numbers
;; Happy numbers are positive integers that follow a particular formula: take each individual digit, square it, and then sum the squares to get a new number. Repeat with the new number and eventually, you might get to a number whose squared sum is 1. This is a happy number. An unhappy number (or sad number) is one that loops endlessly. Write a function that determines if a number is happy or not.
;; (= (__ 7) true)
;; (= (__ 986543210) true)
;; (= (__ 2) false)
;; (= (__ 3) false)
(defn happy-numbers [x]
  {:pre [(pos? x)]}
  (letfn [(sum-of-square-digits [n]
            (->> (str n)
                 (map #(Character/digit % 10))
                 (map #(* % %))
                 (reduce +)))]
    (loop [r #{}
           i x]
      (let [s (sum-of-square-digits i)]
        (cond
          (= s 1) true
          (contains? r s) false
          :else (recur (conj r s) s))))))

;; 93. Partially Flatten a Sequence
;; Write a function which flattens any nested combination of sequential things (lists, vectors, etc.), but maintains the lowest level sequential items. The result should be a sequence of sequences with only one level of nesting.
;; (= (__ [["Do"] ["Nothing"]])
;;    [["Do"] ["Nothing"]])
;; (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
;;    [[:a :b] [:c :d] [:e :f]])
;; (= (__ '((1 2)((3 4)((((5 6)))))))
;;    '((1 2)(3 4)(5 6)))
(defn partially-flatten-a-sequence [s]
  (reduce (fn [result x]
            (concat result
                    (if (every? #(not (coll? %)) x)
                      (vector x)
                      (partially-flatten-a-sequence x))))
          [] s))

;; 98. Equivalence Classes
;; A function f defined on a domain D induces an equivalence relation on D, as follows: a is equivalent to b with respect to f if and only if (f a) is equal to (f b). Write a function with arguments f and D that computes the equivalence classes of D with respect to f.
;; (= (__ #(* % %) #{-2 -1 0 1 2})
;;    #{#{0} #{1 -1} #{2 -2}})
;; (= (__ #(rem % 3) #{0 1 2 3 4 5 })
;;    #{#{0 3} #{1 4} #{2 5}})
;; (= (__ identity #{0 1 2 3 4})
;;    #{#{0} #{1} #{2} #{3} #{4}})
;; (= (__ (constantly true) #{0 1 2 3 4})
;;    #{#{0 1 2 3 4}})
(defn equivalence-classes [f d]
  (set
    (map #(set (map first %))
         (vals
           (group-by second
                     (map #(list % (f %)) d))))))

;; 102. intoCamelCase
;; When working with java, you often need to create an object with fieldsLikeThis, but you'd rather work with a hashmap that has :keys-like-this until it's time to convert. Write a function which takes lower-case hyphen-separated strings and converts them to camel-case strings.
;; (= (__ "something") "something")
;; (= (__ "multi-word-key") "multiWordKey")
;; (= (__ "leaveMeAlone") "leaveMeAlone")
(defn into-camel-case [s]
  (if-not (nil? (re-find #"-" s))
    (let [split (clojure.string/split s #"-")]
      (str
        (first split)
        (clojure.string/join
          (map clojure.string/capitalize (rest split)))))
    s))


;; 105. Identify keys and values
;; Given an input sequence of keywords and numbers, create a map such that each key in the map is a keyword, and the value is a sequence of all the numbers (if any) between it and the next keyword in the sequence.
;; (= {} (__ []))
;; (= {:a [1]} (__ [:a 1]))
;; (= {:a [1], :b [2]} (__ [:a 1, :b 2]))
;; (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))
(defn identify-keys-and-values [s]
  (into {}
        (map #(vector (first %) (into [] (rest %)))
             (let [new (atom false)]
               (partition-by #(if (keyword? %)
                               (reset! new (not @new))
                               @new)
                             s)))))

;; 108. Lazy Searching
;; Given any number of sequences, each sorted from smallest to largest, find the smallest single number which appears in all of the sequences. The sequences may be infinite, so be careful to search lazily.
;; (= 3 (__ [3 4 5]))
;; (= 4 (__ [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
;; (= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13]))
;; (= 64 (__ (map #(* % % %) (range)) ;; perfect cubes
;;           (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
;;           (iterate inc 20))) ;; at least as large as 20
(defn lazy-searching [& sequences]
  (if (apply = (map first sequences))
    (ffirst sequences)
    (let [sorted-sequences (sort-by first sequences)]
      (apply lazy-searching
             (cons (rest (first sorted-sequences))
                   (rest sorted-sequences))))))

;; 110. Sequence of pronunciations
;; Write a function that returns a lazy sequence of "pronunciations" of a sequence of numbers. A pronunciation of each element in the sequence consists of the number of repeating identical numbers and the number itself. For example, [1 1] is pronounced as [2 1] ("two ones"), which in turn is pronounced as [1 2 1 1] ("one two, one one").
;; Your function should accept an initial sequence of numbers, and return an infinite lazy sequence of pronunciations, each element being a pronunciation of the previous element.
;; (= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))
;; (= [3 1 2 4] (first (__ [1 1 1 4 4])))
;; (= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))
;; (= 338 (count (nth (__ [3 2]) 15)))
(defn sequence-of-pronounciations [s]
  (let [n (flatten
            (map #(vector (count %) (first %))
                 (partition-by identity s)))]
    (lazy-seq
      (cons n (sequence-of-pronounciations n)))))

;; 114. Global take-while
;; take-while is great for filtering sequences, but it limited: you can only examine a single item of the sequence at a time. What if you need to keep track of some state as you go over the sequence?
;; Write a function which accepts an integer n, a predicate p, and a sequence. It should return a lazy sequence of items in the list up to, but not including, the nth item that satisfies the predicate.
;; (= [2 3 5 7 11 13]
;;    (__ 4 #(= 2 (mod % 3))
;;        [2 3 5 7 11 13 17 19 23]))
;; (= ["this" "is" "a" "sentence"]
;;    (__ 3 #(some #{\i} %)
;;        ["this" "is" "a" "sentence" "i" "wrote"]))
;; (= ["this" "is"]
;;    (__ 1 #{"a"}
;;        ["this" "is" "a" "sentence" "i" "wrote"]))
(defn global-take-while [n p [x & xs]]
  (let [n-next (if (p x)
                 (dec n)
                 n)]
    (if (zero? n-next)
      '()
      (lazy-seq (cons x (global-take-while n-next p xs))))))

;; 115. The Balance of N
;; A balanced number is one whose component digits have the same sum on the left and right halves of the number. Write a function which accepts an integer n, and returns true iff n is balanced.
;; (= true (__ 11))
;; (= true (__ 121))
;; (= false (__ 123))
;; (= true (__ 0))
;; (= false (__ 88099))
;; (= true (__ 89098))
;; (= true (__ 89089))
;; (= (take 20 (filter __ (range)))
;;    [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])
(defn the-balance-of-n [n]
  (letfn [(sum [s]
            (reduce + (map #(Character/getNumericValue %) s)))]
    (let [s (.toString n)
          half (quot (count s) 2)]
      (=
        (sum (take half s))
        (sum (take-last half s))))))

;; 132. Insert between two items
;; Write a function that takes a two-argument predicate, a value, and a collection; and returns a new collection where the value is inserted between every two items that satisfy the predicate.
;; (= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))
;; (= '(2) (__ > :more [2]))
;; (= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))
;; (empty? (__ > :more ()))
;; (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
;;    (take 12 (->> [0 1]
;;                  (iterate (fn [[a b]] [b (+ a b)]))
;;                  (map first) ; fibonacci numbers
;;                  (__ (fn [a b] ; both even or both odd
;;                        (= (mod a 2) (mod b 2)))
;;                      :same))))
(defn insert-between-two-items [p v s]
  (if (empty? s)
    []
    (flatten
      (concat [(first s)]
              (map #(if (apply p %)
                     (vector v (second %))
                     (second %))
                   (partition 2 1 s))))))

;; 137. Digits and bases
;; Write a function which returns a sequence of digits of a non-negative number (first argument) in numerical system with an arbitrary base (second argument). Digits should be represented with their integer values, e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16.
;; (= [1 2 3 4 5 0 1] (__ 1234501 10))
;; (= [0] (__ 0 11))
;; (= [1 0 0 1] (__ 9 2))
;; (= [1 0] (let [n (rand-int 100000)](__ n n)))
;; (= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42))
(defn digits-and-bases [n base]
  {:pre [(>= n 0)]}
  (letfn [(step [r n base]
            (if (zero? n)
              r
              (step (conj r (mod n base))
                    (quot n base)
                    base)))]
    (if (zero? n)
      '(0)
      (step '() n base))))

;; 144. Oscilrate
;; Write an oscillating iterate: a function that takes an initial value and a variable number of functions. It should return a lazy sequence of the functions applied to the value in order, restarting from the first function after it hits the end.
;; (= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
;; (= (take 5 (__ 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
;; (= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])
(defn oscilrate [v & fs]
  (reductions (fn [v f] (f v)) v (cycle fs)))

;; 158. Decurry
;; Write a function that accepts a curried function of unknown arity n. Return an equivalent function of n arguments.
;; (= 10 ((__ (fn [a]
;;              (fn [b]
;;                (fn [c]
;;                  (fn [d]
;;                    (+ a b c d))))))
;;         1 2 3 4))
;; (= 24 ((__ (fn [a]
;;              (fn [b]
;;                (fn [c]
;;                  (fn [d]
;;                    (* a b c d))))))
;;         1 2 3 4))
;; (= 25 ((__ (fn [a]
;;              (fn [b]
;;                (* a b))))
;;         5 5))
(defn decurry [f]
  (fn [& args]
    (reduce #(%1 %2) f args)))

