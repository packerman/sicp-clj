(ns sicp-clj.data.symbolic.sets.ordered-list
  (:require [clojure.test :refer :all]))

(defn element-of-set? [e set]
  (if-let [[x & xs] (seq set)]
    (cond
      (= e x) true
      (< e x) false
      :else (recur e xs))
    false))

(defn intersection-set [set1 set2]
  (let [s (seq set1) t (seq set2)]
    (if (not (and s t))
      '()
      (let [[x & xs] s
            [y & ys] t]
        (cond
          (= x y) (conj (intersection-set xs ys) x)
          (< x y) (recur xs t)
          (< y x) (recur s ys))))))

(defn adjoin-set [e set]
  (if-let [[x & xs] (seq set)]
    (cond
      (= e x) set
      (< e x) (conj set e)
      (> e x) (conj (adjoin-set e xs) x))
    (list e)))

(defn union-set [set1 set2]
  (let [s (seq set1) t (seq set2)]
    (cond
      (not s) set2
      (not t) set1
      :else (let [[x & xs] set1
                  [y & ys] set2]
              (cond
                (= x y) (conj (union-set xs ys) x)
                (< x y) (conj (union-set xs set2) x)
                (> x y) (conj (union-set set1 ys) y))))))

(deftest set-operations
  (testing "element of set?"
    (is (not (element-of-set? 1 '())))
    (is (not (element-of-set? 2 '(3 4))))
    (is (not (element-of-set? 4 '(3 5))))
    (is (not (element-of-set? 6 '(3 5))))
    (is (element-of-set? 1 '(1)))
    (is (element-of-set? 2 '(2 3)))
    (is (element-of-set? 3 '(2 3 5)))
    (is (element-of-set? 5 '(2 3 5))))
  (testing "set intersection"
    (is (= '() (intersection-set '(2 4 6) '(1 3 5))))
    (is (= '() (intersection-set '(2 4 6) '(11 13 15))))
    (is (= '() (intersection-set '() '(2 3 4))))
    (is (= '() (intersection-set '(2 3 4) '())))
    (is (= '(2 3 5) (intersection-set '(2 3 5) '(2 3 5))))
    (is (= '(2 3) (intersection-set '(2 3 5) '(2 3 4))))
    (is (= '(3) (intersection-set '(1 3 5) '(2 3 4)))))
  (testing "adjoin set"
    (is (= '(5) (adjoin-set 5 '())))
    (is (= '(3 5) (adjoin-set 3 '(5))))
    (is (= '(3 5 7) (adjoin-set 7 '(3 5))))
    (is (= '(3 4 5 7) (adjoin-set 4 '(3 5 7))))
    (is (= '(2 3 4 5 7) (adjoin-set 2 '(3 4 5 7))))
    (is (= '(2 3 4 5 6 7) (adjoin-set 6 '(2 3 4 5 7))))
    (is (= '(2 3 4 5 6 7 8) (adjoin-set 8 '(2 3 4 5 6 7))))
    (is (= '(2 3 4 5 6 7 8) (adjoin-set 3 '(2 3 4 5 6 7 8))))
    (is (= '(2 3 4 5 6 7 8) (adjoin-set 4 '(2 3 4 5 6 7 8))))
    (is (= '(2 3 4 5 6 7 8) (adjoin-set 7 '(2 3 4 5 6 7 8)))))
  (testing "union set"
    (is (= '() (union-set '() '())))
    (is (= '(2 3) (union-set '(2 3) '())))
    (is (= '(2 3) (union-set '() '(2 3))))
    (is (= '(1 2 3 4 5 6) (union-set '(2 4 6) '(1 3 5))))
    (is (= '(1 2 3 4 5 6) (union-set '(1 2 3) '(4 5 6))))
    (is (= '(1 2 3) (union-set '(1 2 3) '(1 2 3))))
    (is (= '(1 2 3) (union-set '(2 3) '(1 2 3))))
    (is (= '(1 2 3) (union-set '(3) '(1 2 3))))))
