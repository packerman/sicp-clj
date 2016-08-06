(ns sicp-clj.data.generic.arithmetic
  (:require [clojure.test :refer :all]
            [sicp-clj.data.abstraction :refer [add-rat sub-rat mul-rat div-rat make-rat]]
            [sicp-clj.data.complex-numbers.message-passing :refer [add-complex sub-complex mul-complex div-complex
                                                                   ->Rectangular equal-complex?]])
  (:import [sicp_clj.data.complex_numbers.message_passing Complex]
           [clojure.lang BigInt]))

(defn- double-type-dispatch [a b]
  [(type a) (type b)])

(defmulti add double-type-dispatch)
(defmulti sub double-type-dispatch)
(defmulti mul double-type-dispatch)
(defmulti div double-type-dispatch)

(derive Long ::Integer)
(derive BigInt ::Integer)
(derive Double ::Float)
(derive ::Integer ::ClojureNumber)
(derive ::Float ::ClojureNumber)

(defmethod add [::ClojureNumber ::ClojureNumber] [a b]
  (+' a b))

(defmethod sub [::ClojureNumber ::ClojureNumber] [a b]
  (-' a b))

(defmethod mul [::ClojureNumber ::ClojureNumber] [a b]
  (*' a b))

(defmethod div [::ClojureNumber ::ClojureNumber] [a b]
  (/ a b))

(defn- add-type
      ([type isa]
        (when isa
          (derive type isa))
        (prefer-method add [::ClojureNumber ::ClojureNumber] [type type])
        (prefer-method sub [::ClojureNumber ::ClojureNumber] [type type])
        (prefer-method mul [::ClojureNumber ::ClojureNumber] [type type])
        (prefer-method div [::ClojureNumber ::ClojureNumber] [type type]))
      ([type] (add-type type nil)))


(alias 'rat 'sicp-clj.data.abstraction)

(derive ::Integer ::rat/Rational)

(defmethod add [::rat/Rational ::rat/Rational] [a b]
  (add-rat a b))

(defmethod sub [::rat/Rational ::rat/Rational] [a b]
  (sub-rat a b))

(defmethod mul [::rat/Rational ::rat/Rational] [a b]
  (mul-rat a b))

(defmethod div [::rat/Rational ::rat/Rational] [a b]
  (div-rat a b))

(add-type ::rat/Rational ::Float)

(derive Complex ::Complex)

(derive ::Float ::Complex)

(add-type ::Complex)

(defmethod add [::Complex ::Complex] [a b]
  (add-complex a b))

(defmethod sub [::Complex ::Complex] [a b]
  (sub-complex a b))

(defmethod mul [::Complex ::Complex] [a b]
  (mul-complex a b))

(defmethod div [::Complex ::Complex] [a b]
  (div-complex a b))


(deftest primitive-numbers
  (letfn [(float-equal? [delta a b]
            (> delta (Math/abs (- a b))))]
    (let [delta 1e10]
      (testing "clojure numbers"
        (is (= 20 (add 15 5)))
        (is (= 10 (sub 15 5)))
        (is (= 75 (mul 15 5)))
        (is (= 3 (div 15 5)))
        (is (= 5.6 (add 3.2 2.4)))
        (is (float-equal? delta 0.8 (sub 3.2 2.4)))
        (is (= 7.68 (mul 3.2 2.4)))
        (is (float-equal? delta 1.333 (div 3.2 2.4)))
        (is (= 7.5 (add 5 2.5)))
        (is (= 2.5 (sub 5 2.5)))
        (is (= 12.5 (mul 5 2.5)))
        (is (= 2.0 (div 5 2.5)))))))

(deftest rational-numbers
  (is (= (make-rat 17 30) (add (make-rat 3 10) (make-rat 4 15))))
  (is (= (make-rat 1 30) (sub (make-rat 3 10) (make-rat 4 15))))
  (is (= (make-rat 2 25) (mul (make-rat 3 10) (make-rat 4 15))))
  (is (= (make-rat 9 8) (div (make-rat 3 10) (make-rat 4 15)))))

(deftest complex-numbers
  (let [tolerance 1e-10]
    (is (equal-complex? tolerance (->Rectangular 3 -3)
                        (add (->Rectangular 2 3) (->Rectangular 1 -6))))
    (is (equal-complex? tolerance (->Rectangular 9 -1)
                        (sub (->Rectangular 5 -2) (->Rectangular -4 -1))))
    (is (equal-complex? tolerance (->Rectangular 10 5)
                        (mul (->Rectangular 2 -1) (->Rectangular 3 4))))
    (is (equal-complex? tolerance (->Rectangular 1.2 -0.6)
                        (div (->Rectangular 3 0) (->Rectangular 2 1))))))

(deftest combining-types
  (let [tolerance 1e-10]
    (is (equal-complex? tolerance (->Rectangular 5.4 2)
                        (add 2.4 (->Rectangular 3 2))))))

(deftest arithmetic
  (primitive-numbers)
  (rational-numbers)
  (complex-numbers)
  (combining-types))

(defn test-ns-hook []
  (arithmetic))

