(ns sicp-clj.procedures.elements)

(defn square [x] (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(defn f [a]
  (sum-of-squares (+ a 1) (* a 2)))

(defn abs [x]
  (cond
    (pos? x) x
    (zero? x) 0
    (neg? x) (- x)))

(defn abs2 [x]
  (cond (neg? x) (- x)
        :else x))

(defn abs3 [x]
  (if (neg? x) (- x) x))

(defn average [x y]
  (/ (+ x y) 2))

(defn sqrt [x]
  (letfn [(good-enough? [guess]
            (< (abs (- (square guess) x)) 0.001))
          (improve [guess]
            (average guess (/ x guess)))
          (sqrt-iter [guess]
            (if (good-enough? guess)
              guess
              (recur (improve guess))))]
    (sqrt-iter 1.0)))

(defn sqrt2 [x]
  (letfn [(good-enough? [guess]
            (< (abs (- guess (improve guess))) (* 0.00001 guess)))
          (improve [guess]
            (average guess (/ x guess)))
          (sqrt-iter [guess]
            (if (good-enough? guess)
              guess
              (recur (improve guess))))]
    (sqrt-iter 1.0)))

(defn sqrt-error [x sqrt-fn]
  (let [s (sqrt-fn x)]
    (abs (- (square s) x))))