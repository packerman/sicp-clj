(ns sicp-clj.data.symbolic.sets.binary-tree
  (:require [clojure.test :refer :all]))

(defn make-tree [entry left right]
  (list entry left right))

(defn element-of-set? [x set]
  (if-let [[entry left right] set]
    (cond
      (= x entry) true
      (< x entry) (recur x left)
      (> x entry) (recur x right))
    false))

(defn adjoin-set [x set]
  (if-let [[entry left right] set]
    (cond
      (= x entry) set
      (< x entry) (make-tree
                    entry
                    (adjoin-set x left)
                    right)
      (> x entry) (make-tree
                    entry
                    left
                    (adjoin-set x right)))
    (make-tree x nil nil)))

(deftest set-operation
  (testing "element of set"
    (is (not (element-of-set? 4 nil)))
    (is (element-of-set? 4 '(4 nil nil)))
    (let [tree '(3 (1 nil nil)
                  (7 (5 nil nil)
                    (9 nil (11 nil nil))))]
      (doseq [e [1 3 5 7 9 11]]
        (is (element-of-set? e tree)))
      (doseq [e [0 2 4 6 8 10 12]]
        (is (not (element-of-set? e tree))))))
  (testing "adjoin set"
    (is (= '(3 nil nil)
           (adjoin-set 3 nil)))
    (is (= '(3 nil nil)
           (adjoin-set 3 '(3 nil nil))))
    (is (= '(3 (1 nil nil) nil)
           (adjoin-set 1 '(3 nil nil))))
    (is (= '(3 (1 nil nil) (7 nil nil))
           (adjoin-set 7 '(3 (1 nil nil) nil))))
    (is (= '(3 (1 nil nil) (7 (5 nil nil) nil))
           (adjoin-set 5 '(3 (1 nil nil) (7 nil nil)))))
    (is (= '(3 (1 nil nil) (7 (5 nil nil) (9 nil nil)))
           (adjoin-set 9 '(3 (1 nil nil) (7 (5 nil nil) nil)))))))

;TODO Exercise 2.63
;TODO Exercise 2.64
;TODO Exercise 2.65
