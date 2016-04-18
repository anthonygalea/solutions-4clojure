(ns solutions-4clojure.medium-test
  (:require [clojure.test :refer :all]
            [solutions-4clojure.medium :refer :all]))

(deftest reverse-interleave-test
  (is (= (reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
  (is (= (reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
  (is (= (reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))))

(deftest rotate-sequence-test
  (is (= (rotate-sequence 2 [1 2 3 4 5]) '(3 4 5 1 2)))
  (is (= (rotate-sequence -2 [1 2 3 4 5]) '(4 5 1 2 3)))
  (is (= (rotate-sequence 6 [1 2 3 4 5]) '(2 3 4 5 1)))
  (is (= (rotate-sequence 1 '(:a :b :c)) '(:b :c :a)))
  (is (= (rotate-sequence -4 '(:a :b :c)) '(:c :a :b))))

(deftest flipping-out-test
  (is (= 3 ((flipping-out nth) 2 [1 2 3 4 5])))
  (is (= true ((flipping-out >) 7 8)))
  (is (= 4 ((flipping-out quot) 2 8)))
  (is (= [1 2 3] ((flipping-out take) [1 2 3 4 5] 3))))

(deftest split-by-type-test
  (is (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
  (is (= (set (split-by-type [:a "foo" "bar" :b])) #{[:a :b] ["foo" "bar"]}))
  (is (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))

(deftest partition-a-sequence-test
  (is (= (partition-a-sequence 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
  (is (= (partition-a-sequence 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
  (is (= (partition-a-sequence 3 (range 8)) '((0 1 2) (3 4 5)))))

(deftest count-occurrences-test
  (is (= (count-occurrences [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
  (is (= (count-occurrences [:b :a :b :a :b]) {:a 2, :b 3}))
  (is (= (count-occurrences '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})))

(deftest find-distinct-items-test
  (is (= (find-distinct-items [1 2 1 3 1 2 4]) [1 2 3 4]))
  (is (= (find-distinct-items [:a :a :b :b :c :c]) [:a :b :c]))
  (is (= (find-distinct-items '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
  (is (= (find-distinct-items (range 50)) (range 50))))

(deftest function-composition-test
  (is (= [3 2 1] ((function-composition rest reverse) [1 2 3 4])))
  (is (= 5 ((function-composition (partial + 3) second) [1 2 3 4])))
  (is (= true ((function-composition zero? #(mod % 8) +) 3 5 7 9)))
  (is (= "HELLO" ((function-composition #(.toUpperCase %) #(apply str %) take) 5 "hello world"))))

(deftest juxtaposition-test
  (is (= [21 6 1] ((juxtaposition + max min) 2 3 5 1 6 4)))
  (is (= ["HELLO" 5] ((juxtaposition #(.toUpperCase %) count) "hello")))
  (is (= [2 6 4] ((juxtaposition :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))))

(deftest sequence-reductions-test
  (is (= (take 5 (sequence-reductions + (range))) [0 1 3 6 10]))
  (is (= (sequence-reductions conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
  (is (= (last (sequence-reductions * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)))

(deftest black-box-testing-test
  (is (= :map (black-box-testing {:a 1, :b 2})))
  (is (= :list (black-box-testing (range (rand-int 20)))))
  (is (= :vector (black-box-testing [1 2 3 4 5 6])))
  (is (= :set (black-box-testing #{10 (rand-int 5)})))
  (is (= [:map :set :vector :list] (map black-box-testing [{} #{} [] ()]))))

(deftest prime-numbers-test
  (is (= (prime-numbers 2) [2 3]))
  (is (= (prime-numbers 5) [2 3 5 7 11]))
  (is (= (last (prime-numbers 100)) 541)))

(deftest merge-with-a-function-test
  (is (= (merge-with-a-function * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
         {:a 4, :b 6, :c 20}))
  (is (= (merge-with-a-function - {1 10, 2 20} {1 3, 2 10, 3 15})
         {1 7, 2 10, 3 15}))
  (is (= (merge-with-a-function concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
         {:a [3 4 5], :b [6 7], :c [8 9]})))

(deftest word-sorting-test
  (is (= (word-sorting "Have a nice day.")
         ["a" "day" "Have" "nice"]))
  (is (= (word-sorting "Clojure is a fun language!")
         ["a" "Clojure" "fun" "is" "language"]))
  (is (= (word-sorting "Fools fall for foolish follies.")
         ["fall" "follies" "foolish" "Fools" "for"])))

(deftest filter-perfect-squares-test
  (is (= (filter-perfect-squares "4,5,6,7,8,9") "4,9"))
  (is (= (filter-perfect-squares "15,16,25,36,37") "16,25,36")))

(deftest eulers-totient-function-test
  (is (= (eulers-totient-function 1) 1))
  (is (= (eulers-totient-function 10) (count '(1 3 7 9)) 4))
  (is (= (eulers-totient-function 40) 16))
  (is (= (eulers-totient-function 99) 60)))

(deftest intro-to-trampoline-test
  (is (= intro-to-trampoline
         (letfn
           [(foo [x y] #(bar (conj x y) y))
            (bar [x y] (if (> (last x) 10)
                         x
                         #(foo x (+ 2 y))))]
           (trampoline foo [] 1)))))

(deftest anagram-finder-test
  (is (= (anagram-finder ["meat" "mat" "team" "mate" "eat"])
         #{#{"meat" "team" "mate"}}))
  (is (= (anagram-finder ["veer" "lake" "item" "kale" "mite" "ever"])
         #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})))