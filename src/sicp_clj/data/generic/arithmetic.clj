(ns sicp-clj.data.generic.arithmetic
  (:require [clojure.test :refer :all]
            [sicp-clj.core :refer :all]
            [sicp-clj.data.abstraction :refer [add-rat sub-rat mul-rat div-rat make-rat rat->double]]
            [sicp-clj.data.complex-numbers.message-passing :refer [add-complex sub-complex mul-complex div-complex
                                                                   ->Rectangular equal-complex?]])
  (:import [sicp_clj.data.complex_numbers.message_passing Complex]
           [clojure.lang BigInt]))

(defn- double-type-dispatch [a b]
  [(type a) (type b)])

(defmulti add-2 double-type-dispatch)
(defmulti sub-2 double-type-dispatch)
(defmulti mul-2 double-type-dispatch)
(defmulti div-2 double-type-dispatch)
(defmulti raise type)
(defmulti generic-type type)

(defmacro install-type [type & {:keys [add sub mul div raise]}]
  {:pre [(and add sub mul div raise)]}
  `(do
     (defmethod add-2 [~type ~type] [a# b#]
       (~add a# b#))
     (defmethod sub-2 [~type ~type] [a# b#]
       (~sub a# b#))
     (defmethod mul-2 [~type ~type] [a# b#]
       (~mul a# b#))
     (defmethod div-2 [~type ~type] [a# b#]
       (~div a# b#))
     (defmethod raise ~type [a#]
       (~raise a#))
     (defmethod generic-type ~type [_#]
       ~type)))

(install-type ::Integer
              :add +' :sub -' :mul *' :div /
              :raise #(make-rat % 1))

(derive Long ::Integer)
(derive BigInt ::Integer)

(install-type ::Float
              :add +' :sub -' :mul *' :div /
              :raise #(->Rectangular % 0))

(derive Double ::Float)

(alias 'rat 'sicp-clj.data.abstraction)

(derive ::Integer ::rat/Rational)

(install-type ::rat/Rational
              :add add-rat :sub sub-rat :mul mul-rat :div div-rat
              :raise rat->double)

(derive ::rat/Rational ::Float)

(derive Complex ::Complex)

(derive ::Float ::Complex)

(install-type ::Complex
              :add add-complex :sub sub-complex :mul mul-complex :div div-complex
              :raise identity)

(defn make-generic [op on-empty]
  (fn gen
    ([] on-empty)
    ([a] a)
    ([a b]
     (let [ta (generic-type a)
           tb (generic-type b)]
       (cond
         (= ta tb) (op a b)
         (isa? ta tb) (recur (raise a) b)
         (isa? tb ta) (recur a (raise b))
         :else (error "Cannot op " a " and " b " with types " ta " and " tb))))
    ([a b & more] (reduce gen (gen a b) more))))

(def add (make-generic add-2 0))
(def sub (make-generic sub-2 0))
(def mul (make-generic mul-2 1))
(def div (make-generic div-2 1))

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
        (is (= 7.68 (mul-2 3.2 2.4)))
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
                        (add 2.4 (->Rectangular 3 2))))
    (is (equal-complex? tolerance (->Rectangular 6 2)
                        (add 2.4 (->Rectangular 3 2) (make-rat 3 5))))))

(deftest arithmetic
  (primitive-numbers)
  (rational-numbers)
  (complex-numbers)
  (combining-types))

(defn test-ns-hook []
  (arithmetic))

