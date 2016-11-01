(ns sicp-clj.metalinguistic.metacircular-evaluator
  (:refer-clojure :exclude [eval true?])
  (:require [clojure.test :refer :all]
            [sicp-clj.core :refer :all]))

(defrecord Evaluated [expression environment])

(defrecord Procedure [parameters body env])

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
                  (->Evaluated nil (assoc new-env var new-val)))
                (error "Unbound variable: SET! " var))))
          (definition? [exp]
            (tagged-list? exp 'define))
          (eval-definition [exp env]
            (let [[_ var new-val-expr] exp
                  {new-val :expression new-env :environment} (eval new-val-expr env)]
              (->Evaluated nil (assoc new-env var new-val))))
          (if? [exp]
            (tagged-list? exp 'if))
          (boolean? [exp] (contains? #{'true 'false} exp))
          (true? [exp]
            (not (false? exp)))
          (false? [exp]
            (or (= 'false exp) (= nil exp) (= '() exp)))
          (eval-if [exp env]
            (let [[_ condition if-branch else-branch] exp
                  {cond-val :expression new-env :environment} (eval condition env)]
              (if (true? cond-val)
                (eval if-branch new-env)
                (eval else-branch new-env))))
          (lambda? [exp]
            (tagged-list? exp 'lambda))
          (eval-lambda [exp env]
            (let [[_ parameters body] exp]
              (->Evaluated (->Procedure parameters body env)
                           env)))
          (begin? [exp]
            (tagged-list? exp 'begin))
          (eval-sequence [exps env]
            (if (not (next exps))
              (eval (first exps) env)
              (let [{new-env :environment} (eval (first exps) env)]
                (recur (rest exps)
                       new-env))))
          (sequence->exp [seq]
            (cond
              (empty? seq) seq
              (not (next seq)) (first seq)
              :else (cons 'begin seq)))
          (cond? [exp]
            (tagged-list? exp 'cond))
          (cond->if [exp]
            (expand-clauses (rest exp)))
          (expand-clauses [clauses]
            (letfn [(cond-test [clause]
                      (first clause))
                    (cond-actions [clause]
                      (rest clause))]
              (if-let [[first-clause & rest-clauses] (seq clauses)]
                (if (= 'else (cond-test first-clause))
                  (if (not rest-clauses)
                    (sequence->exp (cond-actions first-clause))
                    (error "ELSE clause isn't last: COND->IF" clauses))
                  (list 'if (cond-test first-clause)
                        (sequence->exp (cond-actions first-clause))
                        (expand-clauses rest-clauses)))
                'false)))
          ]
    (cond
      (self-evaluating? exp) (->Evaluated exp env)
      (variable? exp) (->Evaluated (lookup-variable-value exp env) env)
      (quoted? exp) (->Evaluated (text-of-quotation exp) env)
      (assignment? exp) (eval-assignment exp env)
      (definition? exp) (eval-definition exp env)
      (if? exp) (eval-if exp env)
      (lambda? exp) (eval-lambda exp env)
      (begin? exp) (eval-sequence (rest exp) env)
      (cond? exp) (eval (cond->if exp) env)
      :else (error "Unknown expression type: EVAL " exp))))

(defmacro expression-is [expected tested]
  `(is (= ~expected (:expression ~tested))))

(deftest evaluator-test
  (testing "Self-evaluating expressions"
    (is (= (->Evaluated 5 {}) (eval '5 {})))
    (is (= (->Evaluated 3 {'x 3}) (eval 'x {'x 3}))))
  (testing "Quoting"
    (is (= (->Evaluated 'x {}) (eval ''x {}))))
  (testing "Assignment and definition"
    (is (= {'x 5} (:environment (eval '(set! x 5) {'x 3}))))
    (is (= {'x 5} (:environment (eval '(define x 5) {'x 3}))))
    (is (= {'x 5} (:environment (eval '(define x 5) {})))))
  (testing "If expression and cond"
    (is (= 2 (:expression (eval '(if true 2 3) {}))))
    (is (= 3 (:expression (eval '(if false 2 3) {}))))
    (is (= (->Evaluated 3 '{x false y 3 z 4})
           (eval
             '(cond (x x)
                    (y y)
                    (z z))
             '{x false y 3 z 4})))
    (is (= (->Evaluated 2 '{x false y false z false})
           (eval
             '(cond (x x)
                    (y y)
                    (z z)
                    (else 2))
             '{x false y false z false}))))
  (testing "Lambda"
    (is (= (->Procedure '[x y]
                        '(if x x y)
                        '{x 3})
           (:expression
             (eval '(lambda [x y] (if x x y))
                   '{x 3})))))
  (testing "Sequence"
    (is (= (->Evaluated 5 '{x 5})
           (eval '(begin
                    (define x 3)
                    (set! x 5)
                    x)
                 {})))))