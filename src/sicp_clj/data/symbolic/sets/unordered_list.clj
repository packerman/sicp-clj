(ns sicp-clj.data.symbolic.sets.unordered-list
  (:require [clojure.test :refer :all]))

(defn element-of-set? [e set]
  (when-let [[x & xs] set]
    (if (= x e)
      true
      (recur e xs))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (conj set x)))

(defn list->set [list]
  (reduce
    #(adjoin-set %2 %1)
    '()
    list))

(defn equal-set? [s1 s2]
  (and (every? #(element-of-set? % s2) s1)
       (every? #(element-of-set? % s1) s2)))

(defn intersection-set [set1 set2]
  (if-let [[x & xs] set1]
    (cond
      (empty? set2) '()
      (element-of-set? x set2) (cons x
                                     (intersection-set xs set2))
      :else (recur xs set2))
    '()))

(deftest make-sets
  (is (equal-set? (list->set '(1 2 3)) (list->set '(2 3 1))))
  (is (not (equal-set? (list->set '(2 3)) (list->set '(2 1)))))
  (is (equal-set? (list->set '()) '()))
  (is (= 3 (count (list->set '(1 2 3 2 3 3)))))
  (is (equal-set? (list->set '(1 2 3)) (list->set '(1 2 3 2 3 3))))
  (is (not (equal-set? (list->set '(1 2 3)) (list->set '(1 2 3 2 3 3 4)))))
  (is (not (equal-set? (list->set '(1 2 3 4)) (list->set '(1 2 3 2 3 3))))))

(deftest set-operations
  (is (not (element-of-set? 5 '())))
  (is (element-of-set? 5 (adjoin-set 5 '())))
  (is (equal-set? (list->set '(5)) (adjoin-set 5 '())))
  (is (not (element-of-set? 5 (list->set '(2 3)))))
  (is (equal-set? (list->set '(2 3))
                  (intersection-set (list->set '(1 2 3))
                                    (list->set '(2 3 4)))))
  )
