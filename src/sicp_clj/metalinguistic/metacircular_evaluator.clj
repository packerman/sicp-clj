(ns sicp-clj.metalinguistic.metacircular-evaluator
  (:refer-clojure :exclude [eval true?])
  (:require [clojure.test :refer :all]
            [sicp-clj.core :refer :all]))

(defn exp-with-env [exp env]
  {:expression exp
   :environment env})

(defn eval [exp env]
  (letfn [(self-evaluating? [exp]
            (or (number? exp)
                (string? exp)
                (boolean? exp)))
          (variable? [exp]
            (symbol? exp))
          (lookup-variable-value [exp env]
            (get env exp))
          (tagged-list? [exp tag]
            (and (list? exp) (= tag (first exp))))
          (quoted? [exp]
            (tagged-list? exp 'quote))
          (text-of-quotation [[_ text]] text)
          (assignment? [exp]
            (tagged-list? exp 'set!))
          (eval-assignment [exp env]
            (let [[_ var new-val-expr] exp]
              (if (contains? env var)
                (let [{new-val :expression new-env :environment} (eval new-val-expr env)]
                  (exp-with-env nil (assoc new-env var new-val)))
                (error "Unbound variable: SET! " var))))
          (definition? [exp]
            (tagged-list? exp 'define))
          (eval-definition [exp env]
            (let [[_ var new-val-expr] exp
                  {new-val :expression new-env :environment} (eval new-val-expr env)]
              (exp-with-env nil (assoc new-env var new-val))))
          (if? [exp]
            (tagged-list? exp 'if))
          (boolean? [exp] (contains? #{'true 'false} exp))
          (true? [exp]
            (= 'true exp))
          (eval-if [exp env]
            (let [[_ condition if-branch else-branch] exp
                  {cond-val :expression new-env :environment} (eval condition env)]
              (if (true? cond-val)
                (eval if-branch new-env)
                (eval else-branch new-env))
              )
            )
          ]
    (cond
      (self-evaluating? exp) (exp-with-env exp env)
      (variable? exp) (exp-with-env (lookup-variable-value exp env) env)
      (quoted? exp) (exp-with-env (text-of-quotation exp) env)
      (assignment? exp) (eval-assignment exp env)
      (definition? exp) (eval-definition exp env)
      (if? exp) (eval-if exp env)
      :else (error "Unknown expression type: EVAL " exp))))

(deftest evaluator-test
  (testing "Self-evaluating expressions"
    (is (= (exp-with-env 5 {}) (eval '5 {})))
    (is (= (exp-with-env 3 {'x 3}) (eval 'x {'x 3}))))
  (testing "Quoting"
    (is (= (exp-with-env 'x {}) (eval ''x {}))))
  (testing "Assignment and definition"
    (is (= {'x 5} (:environment (eval '(set! x 5) {'x 3}))))
    (is (= {'x 5} (:environment (eval '(define x 5) {'x 3}))))
    (is (= {'x 5} (:environment (eval '(define x 5) {})))))
  (testing "If expression"
    (is (= 2 (:expression (eval '(if true 2 3) {}))))
    (is (= 3 (:expression (eval '(if false 2 3) {}))))))