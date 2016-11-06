(ns sicp-clj.metalinguistic.metacircular-evaluator
  (:refer-clojure :exclude [eval true?] :rename {apply clojure-apply})
  (:require [clojure.test :refer :all]
            [sicp-clj.core :refer :all]))

(defrecord Environment [frame enclosing])

(def empty-environment (->Environment (atom {}) nil))

(defn make-env
  ([] (make-env {}))
  ([frame] (make-env frame nil))
  ([frame enclosing] (->Environment (atom frame) enclosing)))

(defn frame [env]
  @(:frame env))

(defn lookup-variable-value [var env]
  (cond
    (nil? env) (error "Unbound variable " var)
    (contains? @(:frame env) var) (get @(:frame env) var)
    :else (recur var (:enclosing env))))

(defn set-variable-value! [var val env]
  (cond
    (nil? env) (error "Unbound variable: SET! " var)
    (contains? @(:frame env) var) (do
                                    (swap! (:frame env) assoc var val)
                                    'ok)
    :else (recur var val (:enclosing env))))

(defn define-variable! [var val env]
  (swap! (:frame env) assoc var val)
  'ok)

(defn extend-environment [variables values base-env]
  (cond
    (= (count variables) (count values)) (make-env (zipmap variables values)
                                                   base-env)
    (< (count variables) (count values)) (error "Too many arguments supplied " variables " " values)
    :else (error "Too few arguments supplied " variables " " values)))

(defrecord Procedure [parameters body environment])

(defrecord Primitive [implementation])

(declare syntax-procedures syntax-procedure?)

(defn sequence->exp [seq]
  (cond
    (empty? seq) seq
    (not (next seq)) (first seq)
    :else (cons 'begin seq)))

(defn eval [exp env]
  (letfn [(self-evaluating? [exp]
            (or (number? exp)
                (string? exp)
                (boolean? exp)))
          (variable? [exp]
            (symbol? exp))
          (tagged-list? [exp tag]
            (and (seq? exp)
                 (= tag (first exp))))
          (quoted? [exp]
            (tagged-list? exp 'quote))
          (text-of-quotation [[_ text]] text)
          (assignment? [exp]
            (tagged-list? exp 'set!))
          (eval-assignment [exp env]
            (let [[_ var val] exp]
              (set-variable-value! var (eval val env) env)))
          (definition? [exp]
            (tagged-list? exp 'define))
          (eval-definition [exp env]
            (let [[_ var & body] exp]
              (if (symbol? var)
                (define-variable! var (eval (first body) env) env)
                (let [[var & parameters] var]
                  (define-variable! var (->Procedure parameters body env) env)))))
          (if? [exp]
            (tagged-list? exp 'if))
          (boolean? [exp] (contains? #{'true 'false} exp))
          (true? [exp]
            (not (false? exp)))
          (false? [exp]
            (or (= 'false exp) (= nil exp) (= '() exp)))
          (eval-if [exp env]
            (let [[_ condition if-branch else-branch] exp]
              (if (true? (eval condition env))
                (eval if-branch env)
                (eval else-branch env))))
          (lambda? [exp]
            (tagged-list? exp 'lambda))
          (eval-lambda [exp env]
            (let [[_ parameters & body] exp]
              (->Procedure parameters body env)))
          (begin? [exp]
            (tagged-list? exp 'begin))
          (eval-sequence [exps env]
            (if (not (next exps))
              (eval (first exps) env)
              (do
                (eval (first exps) env)
                (recur (rest exps)
                       env))))
          (application? [exp]
            (seq? exp))
          (apply [procedure arguments]
            (condp = (type procedure)
              Primitive (clojure-apply (:implementation procedure)
                                       arguments)
              Procedure (eval-sequence
                          (:body procedure)
                          (extend-environment
                            (:parameters procedure)
                            arguments
                            (:environment procedure)))
              (error "Unknown procedure type: " procedure)))
          (list-of-values [exps env]
            (when (seq exps)
              (cons (eval (first exps) env)
                    (list-of-values (next exps) env))))
          ]
    (cond
      (self-evaluating? exp) exp
      (variable? exp) (lookup-variable-value exp env)
      (quoted? exp) (text-of-quotation exp)
      (assignment? exp) (eval-assignment exp env)
      (definition? exp) (eval-definition exp env)
      (if? exp) (eval-if exp env)
      (lambda? exp) (eval-lambda exp env)
      (begin? exp) (eval-sequence (rest exp) env)
      (syntax-procedure? (first exp)) (eval ((syntax-procedures (first exp)) exp) env)
      (application? exp) (let [[operator & operands] exp]
                           (apply (eval operator env)
                                  (list-of-values operands env)))
      :else (error "Unknown expression type: EVAL " exp " " (type exp)))))

(def syntax-procedures {
                        'cond (fn [exp]
                                (letfn [(expand-clauses [clauses]
                                          (letfn [(cond-test [clause]
                                                    (first clause))
                                                  (cond-actions [clause]
                                                    (rest clause))]
                                            (if-let [[first-clause & rest-clauses] (seq clauses)]
                                              (cond
                                                (= 'else (cond-test first-clause)) (if (not rest-clauses)
                                                                                     (sequence->exp (cond-actions first-clause))
                                                                                     (error "ELSE clause isn't last: COND->IF" clauses))
                                                (= '=> (fnext first-clause)) (list 'if (cond-test first-clause)
                                                                                   (list (first (nnext first-clause)) (cond-test first-clause)) ;TODO - test evaluated two times!
                                                                                   (expand-clauses rest-clauses))
                                                :else (list 'if (cond-test first-clause)
                                                            (sequence->exp (cond-actions first-clause))
                                                            (expand-clauses rest-clauses))
                                                )
                                              'false)))]
                                  (expand-clauses (rest exp))))
                        'and  (fn [exp]
                                (letfn [(expand-and [exps]
                                          (if-let [[first-exp & rest-exps] (seq exps)]
                                            (if (not rest-exps)
                                              (list 'if first-exp
                                                    first-exp ;TODO - double evaluation
                                                    'false)
                                              (list 'if first-exp
                                                    (expand-and rest-exps)
                                                    'false))
                                            'true))]
                                  (expand-and (rest exp))))
                        'or   (fn [exp]
                                (letfn [(expand-or [exps]
                                          (if-let [[first-exp & rest-exps] (seq exps)]
                                            (list 'if first-exp
                                                  first-exp
                                                  (expand-or rest-exps))
                                            'false))]
                                  (expand-or (rest exp))))
                        'let  (fn [exp]
                                (if-not (symbol? (fnext exp))
                                  (let [[_ bindings & body] exp
                                        vars (map first bindings)
                                        exps (map second bindings)]
                                    (list*
                                      (list* 'lambda vars body)
                                      exps))
                                  (let [[_ var bindings & body] exp
                                        vars (map first bindings)
                                        params (map second bindings)]
                                    (list 'let '()
                                          (list* 'define (list* var vars) body)
                                          (list*
                                            var
                                            params)))))
                        'let* (fn [exp]
                                (letfn [(expand-lets [bindings body]
                                          (if-let [[first-binding & rest-bindings] (seq bindings)]
                                            (if (not rest-bindings)
                                              (list* 'let (list first-binding) body)
                                              (list 'let (list first-binding)
                                                    (expand-lets rest-bindings body)))
                                            (list* 'let '() body)))]
                                  (let [[_ bindings & body] exp]
                                    (expand-lets bindings body))))
                        })

(defn syntax-procedure? [proc]
  (contains? syntax-procedures proc))

(defn setup-environment []
  (let [initial-env (make-env {
                               'car (->Primitive first)
                               'cdr (->Primitive next)
                               'cons (->Primitive cons)
                               'null? (->Primitive nil?)
                               '+ (->Primitive +')
                               '- (->Primitive -')
                               '* (->Primitive *')
                               '= (->Primitive =)
                               })]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(def the-global-environment (setup-environment))

(def input-prompt  ";;; M-Eval input:")
(def output-prompt ";;; M-Eval value:")

(defn prompt-for-input [string]
  (newline) (newline)
  (print string) (newline)
  (flush))

(defn announce-output [string]
  (newline) (print string) (newline)
  (flush))

(defn user-print [object]
  (if (= Procedure (type object))
    (print (->Procedure (:parameters object)
                        (:body object)
                        '<procedure-env>))
    (print object)))

(defn driver-loop []
  (prompt-for-input input-prompt)
  (let [input (read)
        output (eval input
                     the-global-environment)]
    (announce-output output-prompt)
    (user-print output))
  (recur))

(deftest evaluator-test
  (testing "Self-evaluating expressions"
    (is (= 5 (eval '5 (make-env {}))))
    (is (= 3 (eval 'x (make-env {'x 3})))))
  (testing "Quoting"
    (is (= 'x (eval ''x (make-env {})))))
  (testing "Assignment and definition"
    (let [env (make-env {'x 3})]
      (eval '(set! x 5) env)
      (is (= {'x 5} (frame env))))
    (let [env (make-env '{x 3})]
      (eval '(define x 5) env)
      (is (= '{x 5} (frame env))))
    (let [env (make-env {})]
      (eval '(define x 5) env)
      (is (= '{x 5} (frame env))))
    (is (= 25 (eval '(begin
                       (define (square x) (* x x))
                       (square 5))
                    (make-env {
                               '* (->Primitive *')
                               })))))
  (testing "If expression and cond"
    (is (= 2 (eval '(if true 2 3) (make-env {}))))
    (is (= 3 (eval '(if false 2 3) (make-env {}))))
    (is (= 3 (eval
               '(cond (x x)
                      (y y)
                      (z z))
               (make-env '{x false y 3 z 4}))))
    (is (= 2 (eval
               '(cond (x x)
                      (y y)
                      (z z)
                      (else 2))
               (make-env '{x false y false z false}))))
    (is (= 3 (eval
               '(cond ((+ y 1) => (lambda (x) (+ x 1))))
               (make-env
                 {'y 1
                  '+ (->Primitive +)})))))
  (testing "Lambda"
    (is (= (->Procedure '[x y]
                        '((if x x y))
                        '{x 3})
           (eval '(lambda [x y] (if x x y))
                 '{x 3}))))
  (testing "Sequence"
    (is (= 5
           (eval '(begin
                    (define x 3)
                    (set! x 5)
                    x)
                 (make-env {})))))
  (testing "Application"
    (is (= 2
           (eval '((lambda (x y z) (if x y z)) true 2 3)
                 (make-env {}))))
    (is (= 3 (eval '((lambda (x y z) (if x y z)) false 2 3)
                   (make-env {})))))
  (testing "Primitive procedures"
    (is (= 5 (eval '(+ 2 3)
                   (make-env
                     {'+ (->Primitive +)})))))
  (testing "and, or"
    (is (= 'true (eval '(and) (make-env))))
    (is (= 3 (eval '(and 2 3) (make-env))))
    (is (= 'false (eval '(and 3 false) (make-env))))
    (is (= 'false (eval '(and false 3) (make-env))))
    (is (= 'false (eval '(or) (make-env))))
    (is (= 2 (eval '(or 2 3) (make-env))))
    (is (= 2 (eval '(or false 2 3) (make-env))))
    (is (= 'false (eval '(or false false false) (make-env)))))
  (testing "Let expressions"
    (is (= 5 (eval
               '(let ((x 2) (y 3))
                  (+ x y))
               (make-env
                 {'+ (->Primitive +)}))))
    (is (= 39 (eval
                '(let* ((x 3)
                         (y (+ x 2))
                         (z (+ x y 5)))
                   (* x z))
                (make-env
                  {'+ (->Primitive +)
                   '* (->Primitive *)}))))
    (is (= 55 (eval
                '(begin
                   (define (fib n)
                           (let fib-iter ((a 1) (b 0) (count n))
                                         (if (= count 0)
                                           b
                                           (fib-iter (+ a b)
                                                     a
                                                     (- count 1)))))
                   (fib 10))
                (make-env
                  {'+ (->Primitive +)
                   '- (->Primitive -)
                   '= (->Primitive =)})))))
  (testing "Example programs"
    (is (= '(a b c d e f)
           (eval '(begin
                    (define (append x y)
                            (if (null? x)
                              y
                              (cons (car x) (append (cdr x) y))))
                    (append '(a b c) '(d e f)))
                 (setup-environment))))
    (is (= '(1 4 9 16 25)
           (eval
             '(begin
                (define (map f list)
                        (if (null? list)
                          '()
                          (cons (f (car list)) (map f (cdr list)))))
                (define (square x) (* x x))
                (map square '(1 2 3 4 5)))
             (setup-environment))))))
