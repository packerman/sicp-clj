(ns sicp-clj.procedures.processes
  (:require [sicp-clj.procedures.elements :refer [cube abs]]))

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
