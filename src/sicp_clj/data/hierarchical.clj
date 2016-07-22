(ns sicp-clj.data.hierarchical
  (:require [clojure.test :refer :all]
            [sicp-clj.procedures.elements :refer [square]]
            [sicp-clj.procedures.processes :refer [prime?]]))

(defn list-ref [items n]
  (let [[x & xs] items]
    (if (zero? n)
      x
      (recur xs (dec n)))))

(defn list-length [items]
  (if (empty? items)
    0
    (inc (list-length (next items)))))

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1)
          (append (next list1) list2))))

(defn my-last [lst]
  (when (seq lst)
    (let [[x & xs] lst]
      (if (empty? xs)
        x
        (recur xs)))))

(defn my-reverse [lst]
  (letfn [(iter [rev lst]
            (if-let [[x & xs] lst]
              (recur (cons x rev) xs)
              rev))]
    (iter '() lst)))

(defn cc [amount coins]
  (cond
    (zero? amount) 1
    (or (neg? amount) (empty? coins)) 0
    :else (+ (cc amount (rest coins))
             (cc (- amount (first coins)) coins))))

(defn same-parity [& lst]
  (when-let [[x & xs] lst]
    (cons x
          (filter #(even? (- % x)) xs))))

(defn count-leaves [x]
  (if (seq? x)
    (reduce + (map count-leaves x))
    1))

(defn deep-reverse [lst]
  (if (not (seq? lst))
    lst
    (reverse (map deep-reverse lst))))

(defn fringe [x]
  (if (not (seq? x))
    (list x)
    (mapcat fringe x)))

(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn total-weight [mobile]
  (letfn [(branch-weight [[length structure]]
            (if (number? structure)
              structure
              (mobile-weight structure)))
          (mobile-weight [[left right]]
            (+ (branch-weight left)
               (branch-weight right)))]
    (mobile-weight mobile)))

;TODO Exercise 2.29 3. (balanced mobile)

(defn tree-map [f tree]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

(deftest tree-map-test
  (is (= (tree-map square
                   (list 1
                         (list 2 (list 3 4) 5)
                         (list 6 7)))
         '(1 (4 (9 16) 25) (36 49)))))

;TODO Exercise 2.32

(defn accumulate [op initial sequence]
  (if-let [[x & xs] sequence]
    (op x
        (accumulate op initial xs))
    initial))

(deftest accumulate-test
  (is (= 15 (accumulate + 0 [1 2 3 4 5])))
  (is (= 120 (accumulate * 1 [1 2 3 4 5])))
  (is (= '(1 2 3 4 5) (accumulate cons nil [1 2 3 4 5]))))

(defn reduce-n [op init seqs]
  (apply map
    (fn [& s]
      (reduce op init s))
    seqs))

(deftest reduce-n-test
  (is (= '(22 26 30)
         (reduce-n + 0
                   '((1 2 3)
                      (4 5 6)
                      (7 8 9)
                      (10 11 12))))))

(defn horner-eval [x coeffs]
  (accumulate
    (fn [coeff value]
      (+ coeff (* x value)))
    0
    coeffs))

(deftest horner-eval-test
  (is (= 79 (horner-eval 2 [1 3 0 5 0 1]))))

(defn dot-product [v w]
  (reduce + (map * v w)))

(defn matrix-*-vector [m v]
  (map (fn [row]
         (dot-product row v)) m))

(defn transpose [m]
  (reduce-n conj [] m))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map
      #(matrix-*-vector cols %)
      m)))

(deftest matrix-operations
  (testing "dot product"
    (is (= 3 (dot-product [1 3 -5] [4 -2 -1]))))
  (testing "matrix vector multiplication"
    (is (= [1 -3]
           (matrix-*-vector [[1 -1 2]
                             [0 -3 1]]
                            [2 1 0]))))
  (testing "matrix matrix multiplication"
    (is (= [[0 -10]
            [-3 -1]]
           (matrix-*-matrix [[0 4 -2]
                             [-4 -3 0]]
                            [[0 1]
                             [1 -1]
                             [2 3]]))))
  (testing "transpose matrix"
    (is (= [[1 3 5]
            [2 4 6]]
           (transpose [[1 2]
                       [3 4]
                       [5 6]])))))

(defn prime-sum-pairs [n]
  (->>
    (for [i (range 1 (inc n)) j (range 1 i)]
      [i j (+ i j)])
    (filter (fn [[_ _ s]] (prime? s)))))

(deftest prime-sum-pairs-test
  (is (= (list [2 1 3]
               [3 2 5]
               [4 1 5]
               [4 3 7]
               [5 2 7]
               [6 1 7]
               [6 5 11])
         (prime-sum-pairs 6))))
