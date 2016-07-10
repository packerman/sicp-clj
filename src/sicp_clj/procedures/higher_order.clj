(ns sicp-clj.procedures.higher-order
  (:require [sicp-clj.procedures.elements :refer [cube]]))

(defn sum [term a next b]
  (letfn [(sum-iter [s x]
            (if (> x b)
              s
              (recur (+ s (term x)) (next x))))]
    (sum-iter 0 a)))

(defn sum-cubes [a b]
  (sum cube a inc b))

(defn sum-integers [a b]
  (sum identity a inc b))

(defn pi-sum [a b]
  (letfn [(pi-term [x]
            (/ 1.0 (* x (+ x 2))))
          (pi-next [x]
            (+ x 4))]
    (sum pi-term a pi-next b)))

(defn integral [f a b dx]
  (letfn [(add-dx [x] (+ x dx))]
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx)))

;TODO Exercise 1.29
