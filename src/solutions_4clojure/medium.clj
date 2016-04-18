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
