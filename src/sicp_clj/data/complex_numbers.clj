(ns sicp-clj.data.complex-numbers
  (:require [clojure.test :refer :all]))


(defmulti real-part class)

(defmulti imag-part class)

(defmulti magnitude class)

(defmulti angle class)


(defrecord Rectangular [real imaginary])

(defmethod real-part Rectangular [{:keys [real]}] real)

(defmethod imag-part Rectangular [{:keys [imaginary]}] imaginary)

(defmethod magnitude Rectangular [{:keys [real imaginary]}] (Math/hypot real imaginary))

(defmethod angle Rectangular [{:keys [real imaginary]}] (Math/atan2 imaginary real))


(defrecord Polar [magnitude angle])

(defmethod real-part Polar [{:keys [magnitude angle]}] (* magnitude (Math/cos angle)))

(defmethod imag-part Polar [{:keys [magnitude angle]}] (* magnitude (Math/sin angle)))

(defmethod magnitude Polar [{:keys [magnitude]}] magnitude)

(defmethod angle Polar [{:keys [angle]}] angle)


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


(defn equal-complex? [z1 z2]
  (zero? (magnitude (sub-complex z1 z2))))

(defn almost-equal? [tolerance z1 z2]
  (> tolerance (magnitude (sub-complex z1 z2))))


(deftest complex-operations
  (let [tolerance 1e10
        i (->Rectangular 0 1)
        real (fn [x] (->Rectangular x 0))]
    (is (almost-equal? tolerance
                       (->Rectangular -1 0) (mul-complex i i)))
    (is (almost-equal? tolerance
                       (->Rectangular 5 3) (add-complex (real 5) (mul-complex (real 3) i))))
    (is (almost-equal? tolerance
                       (->Rectangular 5 5)
                       (mul-complex (->Rectangular 2 1) (->Rectangular 3 1))))))