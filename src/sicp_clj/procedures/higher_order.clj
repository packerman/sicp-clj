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

(defn simpson [f a b n]
  {:pre [(even? n) (pos? n) (<= a b)]}
  (let [h (/ (- b a) n)
        coeff (fn [k] (cond
                        (or (= k 0) (= k n)) 1
                        (even? k) 2
                        :else 4))]
    (* h (/ 3.0) (sum (fn [k] (* (coeff k)
                       (f (+ a (* h k)))))
            0 inc n))))
