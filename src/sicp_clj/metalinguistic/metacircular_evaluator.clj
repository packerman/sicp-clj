(ns sicp-clj.metalinguistic.metacircular-evaluator
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer :all]
            [sicp-clj.core :refer :all]))

(defn eval [exp env]
  (letfn [(self-evaluating? [exp]
            (or (number? exp)
                (string? exp)))
          (variable? [exp]
            (symbol? exp))
          (lookup-variable-value [exp env]
            (get env exp))
          (quoted? [exp]
            (and (list? exp) (= 'quote (first exp))))
          (text-of-quotation [[_ text]] text)]
    (cond
      (self-evaluating? exp) exp
      (variable? exp) (lookup-variable-value exp env)
      (quoted? exp) (text-of-quotation exp)
      :else (error "Unknown expression type: EVAL " exp))))

(deftest evaluator-test
  (testing "Self-evaluating expressions"
    (is (= 5 (eval '5 {})))
    (is (= 3 (eval 'x {'x 3})))
    (is (= 'x (eval ''x {})))))