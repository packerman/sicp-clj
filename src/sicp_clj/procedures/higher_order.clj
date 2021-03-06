(ns sicp-clj.procedures.higher-order
  (:require [sicp-clj.procedures.elements :refer [square cube average abs]]
            [sicp-clj.procedures.processes :refer [prime? gcd]]))

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

(defn product [term a next b]
  (letfn [(iter [s x]
            (if (> x b)
              s
              (recur (* s (term x)) (next x))))]
    (iter 1 a)))

(defn factorial [n]
  (product identity 1 inc n))

(defn accumulate [combiner null-value term a next b]
  (letfn [(iter [s x]
            (if (> x b)
              s
              (recur (combiner s (term x)) (next x))))]
    (iter null-value a)))

(defn filtered-accumulate [combiner null-value filter term a next b]
  (letfn [(iter [s x]
            (if (> x b)
              s
              (recur
                (if (filter x)
                  (combiner s (term x))
                  s)
                (next x))))]
    (iter null-value a)))

(defn sum-prime-squares [a b]
  (filtered-accumulate +'
                       0
                       prime?
                       square
                       a
                       inc
                       b))

(defn product-smaller-relatively-prime [n]
  (filtered-accumulate *'
                       1
                       #(= 1 (gcd % n))
                       identity
                       2
                       inc
                       (dec n)))

(defn f [g] (g 2))

(defn error [& args]
  (throw (RuntimeException. (apply str args))))

(defn half-interval-method [f a b]
  (letfn [(search [neg-point pos-point]
            (let [midpoint (average neg-point pos-point)]
              (if (close-enough? neg-point pos-point)
                midpoint
                (let [test-value (f midpoint)]
                  (cond
                    (pos? test-value) (recur neg-point midpoint)
                    (neg? test-value) (recur midpoint pos-point)
                    :else midpoint)))))
          (close-enough? [x y]
            (< (abs (- x y)) 0.001))]
    (let [a-value (f a)
          b-value (f b)]
      (cond
        (and (neg? a-value) (pos? b-value)) (search a b)
        (and (neg? b-value) (pos? a-value)) (search b a)
        :else (error "Values are not of opposite sign" a b)))))

(defn fixed-point [f first-guess]
  (let [tolerance 0.00001
        close-enough? (fn [v1 v2]
                        (< (abs (- v1 v2)) tolerance))
        next-try (fn [guess]
                   (let [next (f guess)]
                     (if (close-enough? guess next)
                       next
                       (recur next))))]
    (next-try first-guess)))

(defn fp-sqrt [x]
  (fixed-point
    (fn [y] (average y (/ x y)))
    1.0))

(defn golden-ratio []
  (fixed-point
    (fn [x] (+ 1 (/ 1 x)))
    1.0))

(defn log [x] (Math/log x))

(defn x-to-x-eq-1000 []
  (fixed-point
    (fn [x] (/ (log 1000) (log x)))
    2.0))

;TODO Exercise 1.36

(defn cont-frac [n d k]
  (letfn [(iter [a i]
            (if (zero? i)
              a
              (recur (/ (n i) (+ (d i) a)) (dec i))))]
    (iter 0 k)))

(defn e-approx [n]
  (+ 2
     (cont-frac
       (fn [i] 1.0)
       (fn [i]
         (cond
           (= i 1) 1.0
           (= (rem i 3) 2) (+ 2.0 (* 2 (quot i 3)))
           :else 1.0))
       n)))

;TODO Exercise 1.39

(defn average-damp [f]
  (fn [x]
    (average x (f x))))

(defn fp-sqrt2 [x]
  (fixed-point
    (average-damp
      (fn [y] (/ x y)))
    1.0))

(defn cube-root [x]
  (fixed-point
    (average-damp
      (fn [y]
        (/ x (square y))))
    1.0))

(defn deriv [g]
  (let [dx 0.00001]
    (fn [x]
      (/ (- (g (+ x dx)) (g x))
         dx))))

(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x)
            ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g)
               guess))

(defn newton-sqrt [x]
  (newtons-method
    (fn [y]
      (- (square y) x))
    1.0))

(defn fixed-point-of-transform [g transform guess]
  (fixed-point (transform g) guess))

(defn sqrt2 [x]
  (fixed-point-of-transform
    (fn [y] (/ x y))
    average-damp
    1.0))

(defn sqrt3 [x]
  (fixed-point-of-transform
    (fn [y] (- (square y) x))
    newton-transform
    1.0))

(defn cubic [a b c]
  (fn [x]
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(defn compose [f g]
  (fn [x]
    (f (g x))))

(defn repeated [f n]
  (letfn [(iter [k g]
            (if (zero? k)
              g
              (recur (dec k) (compose f g))))]
    (iter n identity)))

;TODO Exercise 1.44 (smooth)
;TODO Exercise 1.45 (n-th roots)
;TODO Exercise 1.46 (iterative improvement)
