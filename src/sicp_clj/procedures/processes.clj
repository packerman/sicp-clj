(ns sicp-clj.procedures.processes
  (:require [sicp-clj.procedures.elements :refer [square cube abs]]))

(defn factorial-rec [n]
  (if (= n 1)
    1
    (*' n (factorial-rec (dec n)))))

(defn factorial [n]
  (letfn [(fact-iter [product counter]
            (if (> counter n)
              product
              (recur (*' counter product) (inc counter))))]
    (fact-iter 1 1)))

(defn ackermann [x y]
  (cond
    (zero? y) 0
    (zero? x) (* 2 y)
    (= y 1) 2
    :else (recur (dec x) (ackermann x (dec y)))))

(defn fib-rec [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib-rec (- n 1)) (fib-rec (- n 2)))))

(defn fib [n]
  (letfn [(fib-iter [a b count]
            (if (= count 0)
              b
              (recur (+' a b) a (dec count))))]
    (fib-iter 1 0 n)))

(defn count-change [amount]
  (letfn [(cc [amount kinds-of-coins]
            (cond
              (zero? amount) 1
              (or (neg? amount) (zero? kinds-of-coins)) 0
              :else (+ (cc amount (dec kinds-of-coins))
                       (cc (- amount (first-denomination kinds-of-coins))
                           kinds-of-coins))))
          (first-denomination [kinds-of-coins]
            (condp = kinds-of-coins
              1 1
              2 5
              3 10
              4 25
              5 50))]
    (cc amount 5)))

(defn f-rec [n]
  (if (< n 3)
    n
    (+' (f-rec (- n 1)) (*' 2 (f-rec (- n 2))) (*' 3 (f-rec (- n 3))))))

(defn f [n]
  (letfn [(f-iter [a b c i]
            (if (zero? i)
              c
              (recur (+' a (*' 2 b) (*' 3 c)) a b (dec i))))]
    (f-iter 2 1 0 n)))

(defn sine [angle]
  (let [p #(- (* 3 %) (* 4 (cube %)))
        double-pi (* 2 Math/PI)]
    (cond
      (>= (abs angle) double-pi) (sine (rem angle double-pi))
      (>= (abs angle) 0.1) (p (sine (/ angle 3.0)))
      :else angle)))

(defn fast-expt-rec [b n]
  (cond
    (zero? n) 1
    (even? n) (square (fast-expt-rec b (/ n 2)))
    :else (*' b (fast-expt-rec b (dec n)))))

(defn fast-expt [b n]
  (letfn [(fast-expt-iter [a b n]
            (cond
              (zero? n) a
              (even? n) (recur a (square b) (/ n 2))
              :else (recur (*' a b) b (dec n))))]
    (fast-expt-iter 1 b n)))

(defn fast-mult [a b]
  (letfn [(dbl [x] (*' 2 x))
          (hlv [x] (/ x 2))
          (fast-mult-iter [r x y]
            (cond
              (zero? x) r
              (even? x) (recur r (hlv x) (dbl y))
              :else (recur (+' r y) (dec x) y)))]
    (fast-mult-iter 0 a b)))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (rem a b))))

(defn divides? [a b]
  (zero? (rem b a)))

(defn prime? [n]
  (letfn [(next [k]
            (if (= k 2)
              3
              (+ k 2)))
          (find-divisor [k]
            (cond
              (> (square k) n) n
              (divides? k n) k
              :else (recur (next k))))]
    (and (>= n 2)
         (= n (find-divisor 2)))))

(defn expmod [base exp m]
  (letfn [(expmod-iter [a b n]
            (cond
              (zero? n) a
              (even? n) (recur a (rem (square b) m) (/ n 2))
              :else (recur (rem (*' a b) m) b (dec n))))]
    (expmod-iter 1 base exp)))

(defn fermat-test [n]
  (letfn [(try-it [a]
            (= (expmod a n n) a))]
    (try-it (inc (rand-int (dec n))))))

(defn fast-prime? [n times]
  (cond
    (zero? times) true
    (fermat-test n) (recur n (dec times))
    :else false))

;TODO Exercise 1.28 (Miller-Rabin test)
