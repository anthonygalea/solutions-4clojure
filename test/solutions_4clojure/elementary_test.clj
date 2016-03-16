(ns solutions-4clojure.elementary-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer :all]
            [solutions-4clojure.elementary :refer :all]))

(deftest nothing-but-the-truth-test
  (is (= nothing-but-the-truth true)))

(deftest simple-math-test
  (is (= (- 10 (* 2 3)) simple-math)))

(deftest intro-to-strings-test
  (is (= intro-to-strings (.toUpperCase "hello world"))))

(deftest intro-to-lists-test
  (is (= (list :a :b :c) '(:a :b :c))))

(deftest lists-conj-test
  (is (= lists-conj (conj '(2 3 4) 1)))
  (is (= lists-conj (conj '(3 4) 2 1))))

(deftest intro-to-vectors-test
  (is (= intro-to-vectors (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))))

(deftest vectors-conj-test
  (is (= vectors-conj (conj [1 2 3] 4)))
  (is (= vectors-conj (conj [1 2] 3 4))))

(deftest intro-to-sets-test
  (is (= intro-to-sets (set '(:a :a :b :c :c :c :c :d :d))))
  (is (= intro-to-sets (clojure.set/union #{:a :b :c} #{:b :c :d}))))

(deftest sets-conj-test
  (is (= #{1 2 3 4} (conj #{1 4 3} sets-conj))))

(deftest intro-to-maps-test
  (is (= intro-to-maps ((hash-map :a 10, :b 20, :c 30) :b)))
  (is (= intro-to-maps (:b {:a 10, :b 20, :c 30}))))

(deftest maps-conj-test
  (is (= {:a 1, :b 2, :c 3} (conj {:a 1} maps-conj [:c 3]))))

(deftest intro-to-sequences-test
  (is (= intro-to-sequences (first '(3 2 1))))
  (is (= intro-to-sequences (second [2 3 4])))
  (is (= intro-to-sequences (last (list 1 2 3)))))

(deftest sequences-rest-test
  (is (= sequences-rest (rest [10 20 30 40]))))

(deftest intro-to-functions-test
  (is (= intro-to-functions ((fn add-five [x] (+ x 5)) 3)))
  (is (= intro-to-functions ((fn [x] (+ x 5)) 3)))
  (is (= intro-to-functions (#(+ % 5) 3)))
  (is (= intro-to-functions ((partial + 5) 3))))

(deftest double-down-test
  (is (= (double-down 2) 4))
  (is (= (double-down 3) 6))
  (is (= (double-down 11) 22))
  (is (= (double-down 7) 14)))

(deftest hello-world-test
  (is (= (hello-world "Dave") "Hello, Dave!"))
  (is (= (hello-world "Jenn") "Hello, Jenn!"))
  (is (= (hello-world "Rhea") "Hello, Rhea!")))

(deftest sequences-map-test
  (is (= sequences-map (map #(+ % 5) '(1 2 3)))))

(deftest sequences-filter-test
  (is (= sequences-filter (filter #(> % 5) '(3 4 5 6 7)))))

(deftest local-bindings-test
  (is (= local-bindings (let [x 5] (+ 2 x))))
  (is (= local-bindings (let [x 3, y 10] (- y x))))
  (is (= local-bindings (let [x 21] (let [y 3] (/ x y))))))

(deftest let-it-be-test
  (is (= 10 (let [x 7, y 3, z 1] (+ x y))))
  (is (= 4 (let [x 7, y 3, z 1] (+ y z))))
  (is (= 1 (let [x 7, y 3, z 1] z))))

(deftest regular-expressions-test
  (is (= regular-expressions (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))))

(deftest intro-to-destructuring-test
  (is (= [2 4] (let [[a b c d e] [0 1 2 3 4]] [c e]))))

(deftest simple-recursion-test
  (is (= simple-recursion ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))))

(deftest intro-to-reduce-test
  (is (= 15 (reduce intro-to-reduce [1 2 3 4 5])))
  (is (= 0 (reduce intro-to-reduce [])))
  (is (= 6 (reduce intro-to-reduce 1 [2 3]))))

(deftest recurring-theme-test
  (is (= recurring-theme)
      (loop [x 5
             result []]
        (if (> x 0)
          (recur (dec x) (conj result (+ 2 x)))
          result))))

(deftest rearranging-code-1-test
  (is (= (rearranging-code-1 (sort (rest (reverse [2 5 4 1 3 6]))))))
  (is (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (rearranging-code-1)) 5))

(deftest rearranging-code-2-test
  (is (= (rearranging-code-2 (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))))
  (is (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (rearranging-code-2)) 11))

(deftest a-nil-key-test
  (true? (a-nil-key :a {:a nil :b 2}))
  (false? (a-nil-key :b {:a nil :b 2}))
  (false? (a-nil-key :c {:a nil :b 2})))

(deftest for-the-win-test
  (is (= for-the-win (for [x (range 40)
                           :when (= 1 (rem x 4))]
                       x)))
  (is (= for-the-win (for [x (iterate #(+ 4 %) 0)
                           :let [z (inc x)]
                           :while (< z 40)]
                       z)))
  (is (= for-the-win (for [[x y] (partition 2 (range 20))]
                       (+ x y)))))

(deftest map-defaults-test
  (is (= (map-defaults 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
  (is (= (map-defaults "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
  (is (= (map-defaults [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})))

(deftest subset-and-superset-test
  (is (clojure.set/superset? subset-and-superset #{2}))
  (is (clojure.set/subset? #{1} subset-and-superset))
  (is (clojure.set/superset? subset-and-superset #{1 2}))
  (is (clojure.set/subset? #{1 2} subset-and-superset)))

(deftest logical-falsity-and-truth-test
  (is (= logical-falsity-and-truth (if-not false 1 0)))
  (is (= logical-falsity-and-truth (if-not nil 1 0)))
  (is (= logical-falsity-and-truth (if true 1 0)))
  (is (= logical-falsity-and-truth (if [] 1 0)))
  (is (= logical-falsity-and-truth (if [0] 1 0)))
  (is (= logical-falsity-and-truth (if 0 1 0)))
  (is (= logical-falsity-and-truth (if 1 1 0))))
