(ns solutions-4clojure.easy-test
  (:require [clojure.test :refer :all]
            [solutions-4clojure.easy :refer :all]))

(deftest last-element-test
  (is (= (last-element [1 2 3 4 5]) 5))
  (is (= (last-element '(5 4 3)) 3))
  (is (= (last-element ["b" "c" "d"]) "d")))

(deftest penultimate-element-test
  (is (= (penultimate-element (list 1 2 3 4 5)) 4))
  (is (= (penultimate-element ["a" "b" "c"]) "b"))
  (is (= (penultimate-element [[1 2] [3 4]]) [1 2])))

(deftest nth-element-test
  (is (= (nth-element '(4 5 6 7) 2) 6))
  (is (= (nth-element [:a :b :c] 0) :a))
  (is (= (nth-element [1 2 3 4] 1) 2))
  (is (= (nth-element '([1 2] [3 4] [5 6]) 2) [5 6])))

(deftest count-a-sequence-test
  (is (= (count-a-sequence '(1 2 3 3 1)) 5))
  (is (= (count-a-sequence "Hello World") 11))
  (is (= (count-a-sequence [[1 2] [3 4] [5 6]]) 3))
  (is (= (count-a-sequence '(13)) 1))
  (is (= (count-a-sequence '(:a :b :c)) 3)))

(deftest reverse-a-sequence-test
  (is (= (reverse-a-sequence [1 2 3 4 5]) [5 4 3 2 1]))
  (is (= (reverse-a-sequence (sorted-set 5 7 2 7)) '(7 5 2)))
  (is (= (reverse-a-sequence [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]])))

(deftest sum-it-all-up-test
  (is (= (sum-it-all-up [1 2 3]) 6))
  (is (= (sum-it-all-up (list 0 -2 5 5)) 8))
  (is (= (sum-it-all-up #{4 2 1}) 7))
  (is (= (sum-it-all-up '(0 0 -1)) -1))
  (is (= (sum-it-all-up '(1 10 3)) 14)))

(deftest find-the-odd-numbers-test
  (is (= (find-the-odd-numbers #{1 2 3 4 5}) '(1 3 5)))
  (is (= (find-the-odd-numbers [4 2 1 6]) '(1)))
  (is (= (find-the-odd-numbers [2 2 4 6]) '()))
  (is (= (find-the-odd-numbers [1 1 1 3]) '(1 1 1 3))))

(deftest fibonacci-sequence-test
  (is (= (fibonacci-sequence 3) '(1 1 2)))
  (is (= (fibonacci-sequence 6) '(1 1 2 3 5 8)))
  (is (= (fibonacci-sequence 8) '(1 1 2 3 5 8 13 21))))

(deftest palindrom-detector-test
  (is (false? (palindrome-detector '(1 2 3 4 5))))
  (is (true? (palindrome-detector "racecar")))
  (is (true? (palindrome-detector [:foo :bar :foo])))
  (is (true? (palindrome-detector '(1 1 3 3 1 1))))
  (is (false? (palindrome-detector '(:a :b :c)))))

(deftest get-the-caps-test
  (is (= (get-the-caps "HeLlO, WoRlD!") "HLOWRD"))
  (is (empty? (get-the-caps "nothing")))
  (is (= (get-the-caps "$#A(*&987Zf") "AZ")))

(deftest duplicate-a-sequence-test
  (is (= (duplicate-a-sequence [1 2 3]) '(1 1 2 2 3 3)))
  (is (= (duplicate-a-sequence [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
  (is (= (duplicate-a-sequence [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
  (is (= (duplicate-a-sequence [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))))

(deftest maximum-value-test
  (is (= (maximum-value 1 8 3 4) 8))
  (is (= (maximum-value 30 20) 30))
  (is (= (maximum-value 45 67 11) 67)))

(deftest intro-to-some-test
  (is (= intro-to-some (some #{2 7 6} [5 6 7 8])))
  (is (= intro-to-some (some #(when (even? %) %) [5 6 7 8]))))

(deftest flatten-a-sequence-test
  (is (= (flatten-a-sequence '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
  (is (= (flatten-a-sequence ["a" ["b"] "c"]) '("a" "b" "c")))
  (is (= (flatten-a-sequence '((((:a))))) '(:a))))

(deftest compress-a-sequence-test
  (is (= (apply str (compress-a-sequence "Leeeeeerrroyyy")) "Leroy"))
  (is (= (compress-a-sequence [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
  (is (= (compress-a-sequence [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))

(deftest pack-a-sequence-test
  (is (= (pack-a-sequence [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
  (is (= (pack-a-sequence [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
  (is (= (pack-a-sequence [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))))

(deftest replicate-a-sequence-test
  (is (= (replicate-a-sequence [1 2 3] 2) '(1 1 2 2 3 3)))
  (is (= (replicate-a-sequence [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
  (is (= (replicate-a-sequence [4 5 6] 1) '(4 5 6)))
  (is (= (replicate-a-sequence [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
  (is (= (replicate-a-sequence [44 33] 2) [44 44 33 33])))

(deftest implement-range-test
  (is (= (implement-range 1 4) '(1 2 3)))
  (is (= (implement-range -2 2) '(-2 -1 0 1)))
  (is (= (implement-range 5 8) '(5 6 7))))

(deftest interleave-two-seqs-test
  (is (= (interleave-two-seqs [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
  (is (= (interleave-two-seqs [1 2] [3 4 5 6]) '(1 3 2 4)))
  (is (= (interleave-two-seqs [1 2 3 4] [5]) [1 5]))
  (is (= (interleave-two-seqs [30 20] [25 15]) [30 25 20 15])))

(deftest interpose-a-seq-test
  (is (= (interpose-a-seq 0 [1 2 3]) [1 0 2 0 3]))
  (is (= (apply str (interpose-a-seq ", " ["one" "two" "three"])) "one, two, three"))
  (is (= (interpose-a-seq :z [:a :b :c :d]) [:a :z :b :z :c :z :d])))

(deftest drop-every-nth-item-test
  (is (= (drop-every-nth-item [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
  (is (= (drop-every-nth-item [:a :b :c :d :e :f] 2) [:a :c :e]))
  (is (= (drop-every-nth-item [1 2 3 4 5 6] 4) [1 2 3 5 6])))

(deftest factorial-fun-test
  (is (= (factorial-fun 1) 1))
  (is (= (factorial-fun 3) 6))
  (is (= (factorial-fun 5) 120))
  (is (= (factorial-fun 8) 40320)))

(deftest intro-to-iterate-test
  (is (= intro-to-iterate (take 5 (iterate #(+ 3 %) 1)))))

(deftest contain-yourself-test
  (is (contains? #{4 5 6} contain-yourself))
  (is (contains? [1 1 1 1 1] contain-yourself))
  (is (contains? {4 :a 2 :b} contain-yourself))
  (is (not (contains? [1 2 4] contain-yourself))))

(deftest split-a-sequence-test
  (is (= (split-a-sequence 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (is (= (split-a-sequence 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
  (is (= (split-a-sequence 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

(deftest advanced-destructuring-test
  (is (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] advanced-destructuring] [a b c d]))))

(deftest map-construction-test
  (is (= (map-construction [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (is (= (map-construction [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (is (= (map-construction [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

(deftest re-implement-iterate-test
  (is (= (take 5 (re-implement-iterate #(* 2 %) 1)) [1 2 4 8 16]))
  (is (= (take 100 (re-implement-iterate inc 0)) (take 100 (range))))
  (is (= (take 9 (re-implement-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

(deftest group-a-sequence-test
  (is (= (group-a-seequence #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
  (is (= (group-a-seequence #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
         {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
  (is (= (group-a-seequence count [[1] [1 2] [3] [1 2 3] [2 3]])
         {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

(deftest greatest-common-divisor-test
  (is (= (greatest-common-divisor 2 4) 2))
  (is (= (greatest-common-divisor 10 5) 5))
  (is (= (greatest-common-divisor 5 7) 1))
  (is (= (greatest-common-divisor 1023 858) 33)))

(deftest set-intersection-test
  (is (= (set-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3}))
  (is (= (set-intersection #{0 1 2} #{3 4 5}) #{}))
  (is (= (set-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})))

(deftest a-half-truth-test
  (is (= false (a-half-truth false false)))
  (is (= true (a-half-truth true false)))
  (is (= false (a-half-truth true)))
  (is (= true (a-half-truth false true false)))
  (is (= false (a-half-truth true true true)))
  (is (= true (a-half-truth true true true false))))

(deftest symmetric-difference-test
  (is (= (symmetric-difference #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
  (is (= (symmetric-difference #{:a :b :c} #{}) #{:a :b :c}))
  (is (= (symmetric-difference #{} #{4 5 6}) #{4 5 6}))
  (is (= (symmetric-difference #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})))

(deftest cartesian-product-test
  (is (= (cartesian-product #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
         #{["ace" "♠"] ["ace" "♥"] ["ace" "♦"] ["ace" "♣"]
           ["king" "♠"] ["king" "♥"] ["king" "♦"] ["king" "♣"]
           ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
  (is (= (cartesian-product #{1 2 3} #{4 5})
         #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
  (is (= 300 (count (cartesian-product (into #{} (range 10))
                                       (into #{} (range 30)))))))

(deftest to-tree-or-not-to-tree?-test
  (is (= (to-tree-or-not-to-tree? '(:a (:b nil nil) nil))
         true))
  (is (= (to-tree-or-not-to-tree? '(:a (:b nil nil)))
         false))
  (is (= (to-tree-or-not-to-tree? [1 nil [2 [3 nil nil] [4 nil nil]]])
         true))
  (is (= (to-tree-or-not-to-tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
         false))
  (is (= (to-tree-or-not-to-tree? [1 [2 [3 [4 nil nil] nil] nil] nil])
         true))
  (is (= (to-tree-or-not-to-tree? [1 [2 [3 [4 false nil] nil] nil] nil])
         false))
  (is (= (to-tree-or-not-to-tree? '(:a nil ()))
         false)))

(deftest beauty-is-symmetry-test
  (is (= (beauty-is-symmetry '(:a (:b nil nil) (:b nil nil))) true))
  (is (= (beauty-is-symmetry '(:a (:b nil nil) nil)) false))
  (is (= (beauty-is-symmetry '(:a (:b nil nil) (:c nil nil))) false))
  (is (= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                              [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]]))
      true)
  (is (= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                              [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
         false))
  (is (= (beauty-is-symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                              [2 [3 nil [4 [6 nil nil] nil]] nil]])
         false)))

(deftest pascals-triangle-test
  (is (= (pascals-triangle 1) [1]))
  (is (= (map pascals-triangle (range 1 6))
         [[1]
          [1 1]
          [1 2 1]
          [1 3 3 1]
          [1 4 6 4 1]]))
  (is (= (pascals-triangle 11)
         [1 10 45 120 210 252 210 120 45 10 1])))

(deftest product-digits-test
  (is (= (product-digits 1 1) [1]))
  (is (= (product-digits 99 9) [8 9 1]))
  (is (= (product-digits 999 99) [9 8 9 0 1])))

(deftest least-common-multiple-test
  (is (== (least-common-multiple 2 3) 6))
  (is (== (least-common-multiple 5 3 7) 105))
  (is (== (least-common-multiple 1/3 2/5) 2))
  (is (== (least-common-multiple 3/4 1/6) 3/2))
  (is (== (least-common-multiple 7 5/7 2 3/5) 210)))

(deftest simple-closures-test
  (is (= 256 ((simple-closures 2) 16),
         ((simple-closures 8) 2)))
  (is (= [1 8 27 64] (map (simple-closures 3) [1 2 3 4])))
  (is (= [1 2 4 8 16] (map #((simple-closures %) 2) [0 1 2 3 4]))))

(deftest re-implement-map-test
  (is (= [3 4 5 6 7]
         (re-implement-map inc [2 3 4 5 6])))
  (is (= (repeat 10 nil)
         (re-implement-map (fn [_] nil) (range 10))))
  (is (= [1000000 1000001]
         (->> (re-implement-map inc (range))
              (drop (dec 1000000))
              (take 2)))))

(deftest sum-of-square-digits-test
  (is (= 8 (sum-of-square-digits (range 10))))
  (is (= 19 (sum-of-square-digits (range 30))))
  (is (= 50 (sum-of-square-digits (range 100))))
  (is (= 50 (sum-of-square-digits (range 1000)))))

(deftest read-a-binary-number-test
  (is (= 0 (read-a-binary-number "0")))
  (is (= 7 (read-a-binary-number "111")))
  (is (= 8 (read-a-binary-number "1000")))
  (is (= 9 (read-a-binary-number "1001")))
  (is (= 255 (read-a-binary-number "11111111")))
  (is (= 1365 (read-a-binary-number "10101010101")))
  (is (= 65535 (read-a-binary-number "1111111111111111"))))

(deftest through-the-looking-class-test
  (is (let [x through-the-looking-class]
        (and (= (class x) x) x))))

(deftest recognize-playing-cards-test
  (is (= {:suit :diamond :rank 10} (recognize-playing-cards "DQ")))
  (is (= {:suit :heart :rank 3} (recognize-playing-cards "H5")))
  (is (= {:suit :club :rank 12} (recognize-playing-cards "CA")))
  (is (= (range 13) (map (comp :rank recognize-playing-cards str)
                         '[S2 S3 S4 S5 S6 S7
                           S8 S9 ST SJ SQ SK SA]))))

(deftest infix-calculator-test
  (is (= 7 (infix-calculator 2 + 5)))
  (is (= 42 (infix-calculator 38 + 48 - 2 / 2)))
  (is (= 8 (infix-calculator 10 / 2 - 1 * 2)))
  (is (= 72 (infix-calculator 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))))

(deftest dot-product-test
  (is (= 0 (dot-product [0 1 0] [1 0 0])))
  (is (= 3 (dot-product [1 1 1] [1 1 1])))
  (is (= 32 (dot-product [1 2 3] [4 5 6])))
  (is (= 256 (dot-product [2 5 6] [100 10 1]))))

(deftest trees-into-tables-test
  (is (= (trees-into-tables '{a {p 1, q 2}
                              b {m 3, n 4}})
         '{[a p] 1, [a q] 2
           [b m] 3, [b n] 4}))
  (is (= (trees-into-tables '{[1] {a b c d}
                              [2] {q r s t u v w x}})
         '{[[1] a] b, [[1] c] d,
           [[2] q] r, [[2] s] t,
           [[2] u] v, [[2] w] x}))
  (is (= (trees-into-tables '{m {1 [a b c] 3 nil}})
         '{[m 1] [a b c], [m 3] nil})))

(deftest pascals-trapezoid-test
  (is (= (second (pascals-trapezoid [2 3 2])) [2 5 5 2]))
  (is (= (take 5 (pascals-trapezoid [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
  (is (= (take 2 (pascals-trapezoid [3 1 2])) [[3 1 2] [3 4 3 2]]))
  (is (= (take 100 (pascals-trapezoid [2 4 2])) (rest (take 101 (pascals-trapezoid [2 2]))))))

(deftest pairwise-disjoint-sets-test
  (is (= (pairwise-disjoint-sets #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
         true))
  (is (= (pairwise-disjoint-sets #{#{:a :b :c :d :e}
                                   #{:a :b :c :d}
                                   #{:a :b :c}
                                   #{:a :b}
                                   #{:a}})
         false))
  (is (= (pairwise-disjoint-sets #{#{[1 2 3] [4 5]}
                                   #{[1 2] [3 4 5]}
                                   #{[1] [2] 3 4 5}
                                   #{1 2 [3 4] [5]}})
         true))
  (is (= (pairwise-disjoint-sets #{#{'a 'b}
                                   #{'c 'd 'e}
                                   #{'f 'g 'h 'i}
                                   #{''a ''c ''f}})
         true))
  (is (= (pairwise-disjoint-sets #{#{'(:x :y :z) '(:x :y) '(:z) '()}
                                   #{#{:x :y :z} #{:x :y} #{:z} #{}}
                                   #{'[:x :y :z] [:x :y] [:z] [] {}}})
         false))
  (is (= (pairwise-disjoint-sets #{#{(= "true") false}
                                   #{:yes :no}
                                   #{(class 1) 0}
                                   #{(symbol "true") 'false}
                                   #{(keyword "yes") ::no}
                                   #{(class '1) (int \0)}})
         false))
  (is (= (pairwise-disjoint-sets #{#{distinct?}
                                   #{#(-> %) #(-> %)}
                                   #{#(-> %) #(-> %) #(-> %)}
                                   #{#(-> %) #(-> %) #(-> %)}})
         true))
  (is (= (pairwise-disjoint-sets #{#{(#(-> *)) + (quote mapcat) #_nil}
                                   #{'+ '* mapcat (comment mapcat)}
                                   #{(do) set contains? nil?}
                                   #{,,, #_,, empty?}})
         false)))

(deftest indexing-sequences-test
  (is (= (indexing-sequences [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
  (is (= (indexing-sequences [0 1 3]) '((0 0) (1 1) (3 2))))
  (is (= (indexing-sequences [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])))

(deftest comparisons-test
  (is (= :gt (comparisons < 5 1)))
  (is (= :eq (comparisons (fn [x y] (< (count x) (count y))) "pear" "plum")))
  (is (= :lt (comparisons (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
  (is (= :gt (comparisons > 0 2))))

(deftest intro-to-desctructuring-2-test
  (is (= 3
         (let [[f x] [+ (range 3)]] (apply f x))
         (let [[[f x] b] [[+ 1] 2]] (f x b))
         (let [[f x] [inc 2]] (f x)))))