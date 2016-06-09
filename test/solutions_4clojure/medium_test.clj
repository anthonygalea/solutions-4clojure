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

(deftest reimplement-trampoline-test
  (is (= (letfn [(triple [x] #(sub-two (* 3 x)))
                 (sub-two [x] #(stop? (- x 2)))
                 (stop? [x] (if (> x 50) x #(triple x)))]
           (reimplement-trampoline triple 2))
         82))
  (is (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
                 (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
           (map (partial reimplement-trampoline my-even?) (range 6)))
         [true false true false true false])))

(deftest perfect-numbers-test
  (is (= (perfect-numbers 6) true))
  (is (= (perfect-numbers 7) false))
  (is (= (perfect-numbers 496) true))
  (is (= (perfect-numbers 500) false))
  (is (= (perfect-numbers 8128) true)))

(deftest power-set-test
  (is (= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
  (is (= (power-set #{}) #{#{}}))
  (is (= (power-set #{1 2 3})
         #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
  (is (= (count (power-set (into #{} (range 10)))) 1024)))

(deftest happy-numbers-test
  (is (= (happy-numbers 7) true))
  (is (= (happy-numbers 986543210) true))
  (is (= (happy-numbers 2) false))
  (is (= (happy-numbers 3) false)))

(deftest partially-flatten-a-sequence-test
  (is (= (partially-flatten-a-sequence [["Do"] ["Nothing"]])
         [["Do"] ["Nothing"]]))
  (is (= (partially-flatten-a-sequence [[[[:a :b]]] [[:c :d]] [:e :f]])
         [[:a :b] [:c :d] [:e :f]]))
  (is (= (partially-flatten-a-sequence '((1 2) ((3 4) ((((5 6)))))))
         '((1 2) (3 4) (5 6)))))

(deftest equivalence-classes-test
  (is (= (equivalence-classes #(* % %) #{-2 -1 0 1 2})
         #{#{0} #{1 -1} #{2 -2}}))
  (is (= (equivalence-classes #(rem % 3) #{0 1 2 3 4 5})
         #{#{0 3} #{1 4} #{2 5}}))
  (is (= (equivalence-classes identity #{0 1 2 3 4})
         #{#{0} #{1} #{2} #{3} #{4}}))
  (is (= (equivalence-classes (constantly true) #{0 1 2 3 4})
         #{#{0 1 2 3 4}})))

(deftest into-camel-case-test
  (is (= (into-camel-case "something") "something"))
  (is (= (into-camel-case "multi-word-key") "multiWordKey"))
  (is (= (into-camel-case "leaveMeAlone") "leaveMeAlone")))

(deftest generating-k-combinations-test
  (is (= (generating-k-combinations 1 #{4 5 6}) #{#{4} #{5} #{6}}))
  (is (= (generating-k-combinations 10 #{4 5 6}) #{}))
  (is (= (generating-k-combinations 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
  (is (= (generating-k-combinations 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                                      #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
  (is (= (generating-k-combinations 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))
  (is (= (generating-k-combinations 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                                                   #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})))

(deftest identify-keys-and-values-test
  (is (= {} (identify-keys-and-values [])))
  (is (= {:a [1]} (identify-keys-and-values [:a 1])))
  (is (= {:a [1], :b [2]} (identify-keys-and-values [:a 1, :b 2])))
  (is (= {:a [1 2 3], :b [], :c [4]} (identify-keys-and-values [:a 1 2 3 :b :c 4]))))

(deftest lazy-searching-test
  (is (= 3 (lazy-searching [3 4 5])))
  (is (= 4 (lazy-searching [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
  (is (= 7 (lazy-searching (range) (range 0 100 7/6) [2 3 5 7 11 13])))
  (is (= 64 (lazy-searching (map #(* % % %) (range))        ;; perfect cubes
                            (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                            (iterate inc 20)))))            ;; at least as large as 20

(deftest sequence-of-pronunciations-test
  (is (= [[1 1] [2 1] [1 2 1 1]] (take 3 (sequence-of-pronounciations [1]))))
  (is (= [3 1 2 4] (first (sequence-of-pronounciations [1 1 1 4 4]))))
  (is (= [1 1 1 3 2 1 3 2 1 1] (nth (sequence-of-pronounciations [1]) 6)))
  (is (= 338 (count (nth (sequence-of-pronounciations [3 2]) 15)))))

(deftest global-take-while-test
  (is (= [2 3 5 7 11 13]
         (global-take-while 4 #(= 2 (mod % 3))
                            [2 3 5 7 11 13 17 19 23])))
  (is (= ["this" "is" "a" "sentence"]
         (global-take-while 3 #(some #{\i} %)
                            ["this" "is" "a" "sentence" "i" "wrote"])))
  (is (= ["this" "is"]
         (global-take-while 1 #{"a"}
                            ["this" "is" "a" "sentence" "i" "wrote"]))))

(deftest the-balance-of-n-test
  (is (= true (the-balance-of-n 11)))
  (is (= true (the-balance-of-n 121)))
  (is (= false (the-balance-of-n 123)))
  (is (= true (the-balance-of-n 0)))
  (is (= false (the-balance-of-n 88099)))
  (is (= true (the-balance-of-n 89098)))
  (is (= true (the-balance-of-n 89089)))
  (is (= (take 20 (filter the-balance-of-n (range)))
         [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])))

(deftest prime-sandwich-test
  (is (= false (prime-sandwich? 4)))
  (is (= true (prime-sandwich? 563)))
  (is (= 1103 (nth (filter prime-sandwich? (range)) 15))))

(deftest universal-computation-engine-test
  (is (= 2 ((universal-computation-engine '(/ a b))
            '{b 8 a 16})))
  (is (= 8 ((universal-computation-engine '(+ a b 2))
            '{a 2 b 4})))
  (is (= [6 0 -4]
         (map (universal-computation-engine '(* (+ 2 a)
                                                (- 10 b)))
              '[{a 1 b 8}
                {b 5 a -2}
                {a 2 b 11}])))
  (is (= 1 ((universal-computation-engine '(/ (+ x 2)
                                              (* 3 (+ y 1))))
            '{x 4 y 1}))))

(deftest insert-between-two-items-test
  (is (= '(1 :less 6 :less 7 4 3) (insert-between-two-items < :less [1 6 7 4 3])))
  (is (= '(2) (insert-between-two-items > :more [2])))
  (is (= [0 1 :x 2 :x 3 :x 4] (insert-between-two-items #(and (pos? %) (< % %2)) :x (range 5))))
  (is (empty? (insert-between-two-items > :more ())))
  (is (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
         (take 12 (->> [0 1]
                       (iterate (fn [[a b]] [b (+ a b)]))
                       (map first)                          ; fibonacci numbers
                       (insert-between-two-items (fn [a b]  ; both even or both odd
                                                   (= (mod a 2) (mod b 2)))
                                                 :same))))))

(deftest digits-and-bases-test
  (is (= [1 2 3 4 5 0 1] (digits-and-bases 1234501 10)))
  (is (= [0] (digits-and-bases 0 11)))
  (is (= [1 0 0 1] (digits-and-bases 9 2)))
  (is (= [1 0] (let [n (rand-int 100000)] (digits-and-bases n n))))
  (is (= [16 18 5 24 15 1] (digits-and-bases Integer/MAX_VALUE 42))))

(deftest oscilrate-test
  (is (= (take 3 (oscilrate 3.14 int double)) [3.14 3 3.0]))
  (is (= (take 5 (oscilrate 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
  (is (= (take 12 (oscilrate 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])))

(deftest the-big-divide-test
  (is (= 0 (the-big-divide 3 17 11)))
  (is (= 23 (the-big-divide 10 3 5)))
  (is (= 233168 (the-big-divide 1000 3 5)))
  (is (= "2333333316666668" (str (the-big-divide 100000000 3 5))))
  (is (= "110389610389889610389610"
         (str (the-big-divide (* 10000 10000 10000) 7 11))))
  (is (= "1277732511922987429116"
         (str (the-big-divide (* 10000 10000 10000) 757 809))))
  (is (= "4530161696788274281"
         (str (the-big-divide (* 10000 10000 1000) 1597 3571)))))

(deftest decurry-test
  (is (= 10 ((decurry (fn [a]
                        (fn [b]
                          (fn [c]
                            (fn [d]
                              (+ a b c d))))))
             1 2 3 4)))
  (is (= 24 ((decurry (fn [a]
                        (fn [b]
                          (fn [c]
                            (fn [d]
                              (* a b c d))))))
             1 2 3 4)))
  (is (= 25 ((decurry (fn [a]
                        (fn [b]
                          (* a b))))
             5 5))))

(deftest intervals-test
  (is (= (intervals [1 2 3]) [[1 3]]))
  (is (= (intervals [10 9 8 1 2 3]) [[1 3] [8 10]]))
  (is (= (intervals [1 1 1 1 1 1 1]) [[1 1]]))
  (is (= (intervals []) []))
  (is (= (intervals [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
         [[1 4] [6 6] [9 11] [13 17] [19 19]])))

