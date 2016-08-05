(ns sicp-clj.data.complex-numbers.tagged
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


(defn nth-roots [z n]
  (let [r (magnitude z)
        a (angle z)
        r-nth-root (Math/pow r (/ 1.0 n))]
    (for [k (range n)]
      (->Polar r-nth-root
               (/ (+ a (* 2 k Math/PI)) n)))))

(defn nth-power
  ([z n]
    (nth-power z n (->Rectangular 1 0)))
  ([z n acc]
    (if (zero? n)
      acc
      (recur z (dec n) (mul-complex acc z)))))


(defmulti str-complex class)

(defmethod str-complex Rectangular [{:keys [real imaginary]}]
  (str "rect(" real ", " imaginary ")"))

(defmethod str-complex Polar [{:keys [magnitude angle]}]
  (str "polar(" magnitude ", " angle ")"))

(defmulti rectangular class)

(defmethod rectangular Rectangular [z] z)

(defmethod rectangular Polar [z]
  (->Rectangular (real-part z) (imag-part z)))


(defn equal-complex? [z1 z2]
  (zero? (magnitude (sub-complex z1 z2))))

(defn almost-equal? [tolerance z1 z2]
  (> tolerance (magnitude (sub-complex z1 z2))))


(deftest complex-operations
  (let [tolerance 1e10
        i (->Rectangular 0 1)
        real (fn [x] (->Rectangular x 0))]
    (is (almost-equal? tolerance
                       (real -1) (mul-complex i i)))
    (is (almost-equal? tolerance
                       (->Rectangular 5 3) (add-complex (real 5) (mul-complex (real 3) i))))
    (is (almost-equal? tolerance
                       (->Rectangular 5 5)
                       (mul-complex (->Rectangular 2 1) (->Rectangular 3 1))))))

;TODO Exercise 2.73
;TODO Exercise 2.74
