(ns sicp-clj.data.complex-numbers.message-passing
  (:require [clojure.test :refer :all]))


(defprotocol Complex
  (real-part [z])
  (imag-part [z])
  (magnitude [z])
  (angle [z]))

(defrecord Rectangular [real imaginary]
  Complex
  (real-part [_] real)
  (imag-part [_] imaginary)
  (magnitude [_] (Math/hypot real imaginary))
  (angle [_] (Math/atan2 imaginary real)))

(defrecord Polar [magnitude angle]
  Complex
  (real-part [_] (* magnitude (Math/cos angle)))
  (imag-part [_] (* magnitude (Math/sin angle)))
  (magnitude [_] magnitude)
  (angle [_] angle))


(defn add-complex [z1 z2]
  (->Rectangular
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))))

(defn sub-complex [z1 z2]
  (->Rectangular
    (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2))))

(defn mul-complex [z1 z2]
  (->Polar
    (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))))

(defn div-complex [z1 z2]
  (->Polar
    (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))


(defn equal-complex? [tolerance z1 z2]
  (> tolerance (magnitude (sub-complex z1 z2))))


(deftest complex-operations
  (let [tolerance 1e-10
        i (->Rectangular 0 1)
        real (fn [x] (->Rectangular x 0))]
    (is (equal-complex? tolerance
                        (real -1) (mul-complex i i)))
    (is (equal-complex? tolerance
                        (->Rectangular 5 3) (add-complex (real 5) (mul-complex (real 3) i))))
    (is (equal-complex? tolerance
                        (->Rectangular 5 5)
                        (mul-complex (->Rectangular 2 1) (->Rectangular 3 1))))))
