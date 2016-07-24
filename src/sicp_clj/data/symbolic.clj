(ns sicp-clj.data.symbolic
  (:require [sicp-clj.core :refer :all]
            [clojure.test :refer :all]))

(defn memq [item s]
  (if-let [[x & xs] s]
    (if (= item x)
      s
      (recur item xs))
    false))

(deftest memq-test
  (is (false? (memq 'apple '(pear banana prue))))
  (is (= '(apple pear) (memq 'apple '(x (apple sauce) y apple pear)))))

(defn make-sum [a1 a2]
  (cond
    (= 0 a1) a2
    (= 0 a2) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond
    (or (= 0 m1) (= 0 m2)) 0
    (= 1 m1) m2
    (= 1 m2) m1
    (and (number? m1) (number? m2)) (* m1 m2)
    :else (list '* m1 m2)))

(defn make-exponentiation [b e]
  (cond
    (= 0 e) 1
    (= 1 e) b
    :else (list '** b e)))

(defn deriv [exp var]
  (cond
    (number? exp) 0
    (symbol? exp) (if (= exp var) 1 0)
    :else (let [[op arg1 arg2] exp]
            (condp = op
              '+ (make-sum (deriv arg1 var) (deriv arg2 var))
              '* (make-sum
                       (make-product arg1 (deriv arg2 var))
                       (make-product (deriv arg1 var) arg2))
              '** (make-product arg2
                                (make-product
                                  (make-exponentiation arg1 (- arg2 1))
                                  (deriv arg1 var)))
              (error "unknown expression type: " exp)))))

;TODO Exercise 2.57
;TODO Exercise 2.58
