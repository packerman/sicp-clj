(ns sicp-clj.data.generic.algebra
  (:require [clojure.test :refer :all]
            [sicp-clj.core :refer :all]
            [sicp-clj.data.generic.arithmetic :refer [install-type
                                                      add sub mul div =zero? neg]]))

(defn make-poly [variable terms]
  ^{:type ::Polynomial}
  {:variable variable
   :terms    terms})

(defn poly? [p]
  (= ::Polynomial (type p)))

(defn zero-poly? [{:keys [terms] :as p}]
  {:pre [(poly? p)]}
  (empty? terms))

(defn make-term [order coeff]
  [order coeff])

(declare add-terms
         adjoin-term)

(defn add-poly [{variable-1 :variable terms-1 :terms :as p1}
                {variable-2 :variable terms-2 :terms :as p2}]
  {:pre [(poly? p1) (poly? p2) (= variable-1 variable-2)]}
  (make-poly variable-1
             (add-terms terms-1 terms-2)))

(declare neg-terms)

(defn neg-poly [{:keys [variable terms] :as p}]
  {:pre [(poly? p)]}
  (make-poly variable (neg-terms terms)))

(declare sub-terms)

(defn sub-poly [{variable-1 :variable terms-1 :terms :as p1}
                {variable-2 :variable terms-2 :terms :as p2}]
  {:pre [(poly? p1) (poly? p2) (= variable-1 variable-2)]}
  (make-poly variable-1
             (sub-terms terms-1 terms-2)))

(declare mul-terms)

(defn mul-poly [{variable-1 :variable terms-1 :terms :as p1}
                {variable-2 :variable terms-2 :terms :as p2}]
  {:pre [(poly? p1) (poly? p2) (= variable-1 variable-2)]}
  (make-poly variable-1
             (mul-terms terms-1 terms-2)))

(declare div-terms)

(defn make-div-result [quot rem]
  {:quot quot
   :rem rem})

(defn div-poly [{variable-1 :variable terms-1 :terms :as p1}
                {variable-2 :variable terms-2 :terms :as p2}]
  {:pre [(poly? p1) (poly? p2) (= variable-1 variable-2)]}
  (let [{:keys [quot rem]} (div-terms terms-1 terms-2)]
    (make-div-result (make-poly variable-1 quot)
                     (make-poly variable-1 rem))))

(defn- add-terms [terms-1 terms-2]
  (let [[[order-1 coeff-1 :as t1] & rest-terms-1 :as seq-1] (seq terms-1)
        [[order-2 coeff-2 :as t2] & rest-terms-2 :as seq-2] (seq terms-2)]
    (cond
      (not seq-1) seq-2
      (not seq-2) seq-1
      (> order-1 order-2) (adjoin-term t1 (add-terms rest-terms-1 seq-2))
      (< order-1 order-2) (adjoin-term t2 (add-terms seq-1 rest-terms-2))
      :else (adjoin-term (make-term order-1 (add coeff-1 coeff-2))
                         (add-terms rest-terms-1 rest-terms-2)))))

(defn- neg-terms [terms]
  (for [[order coeff] terms] [order (neg coeff)]))

(defn- sub-terms [terms-1 terms-2]
  (add-terms terms-1 (neg-terms terms-2)))

(defn mull-term-by-all-terms [t terms]
  (letfn [(mul-2-terms [[order-1 coeff-1] [order-2 coeff-2]]
            (make-term (+ order-1 order-2)
                       (mul coeff-1 coeff-2)))]
    (map #(mul-2-terms t %) terms)))

(defn- mul-terms [terms-1 terms-2]
  (reduce add-terms
          nil
          (map #(mull-term-by-all-terms % terms-2) terms-1)))

(defn- div-terms [terms-1 terms-2]
  (if-let [[[order-1 coeff-1] & rest-terms-1] (seq terms-1)]
    (let [[[order-2 coeff-2] & rest-terms-2] terms-2]
      (if (> order-2 order-1)
        (make-div-result '() terms-1)
        (let [new-term (make-term (- order-1 order-2)
                                  (div coeff-1 coeff-2))
              rest-result (div-terms (sub-terms terms-1
                                               (mull-term-by-all-terms new-term terms-2))
                                     terms-2)]
          (make-div-result (adjoin-term new-term (:quot rest-result)) (:rem rest-result)))))
    (make-div-result '() '())))

(defn- adjoin-term [[_ coeff :as t] terms]
  (if (=zero? coeff)
    terms
    (cons t terms)))

(install-type ::Polynomial
              :add add-poly
              :sub sub-poly
              :mul mul-poly
              :div div-poly
              :zero zero-poly?
              :neg neg-poly
              :raise identity)

(deftest adding
  (is (= (make-poly 'x '([3 2] [2 3] [1 2] [0 -8]))
         (add (make-poly 'x '([3 1] [2 5] [1 -2]))
              (make-poly 'x '([3 1] [1 3] [0 -6]))
              (make-poly 'x '([2 -2] [1 1] [0 -2]))))))

(deftest negating
  (is (= (make-poly 'x '([3 -3] [2 8] [1 5] [0 -6])))
      (neg (make-poly 'x '([3 3] [2 -8] [[1 -5] [0 6]])))))

(deftest subtracting
  (is (= (make-poly 'x '([3 -2] [2 11] [1 10] [0 -10]))
         (sub (make-poly 'x '([3 1] [2 3] [1 5] [0 -4]))
              (make-poly 'x '([3 3] [2 -8] [1 -5] [0 6]))))))

(deftest multiplying
  (is (= (make-poly 'x '([6 2] [5 4] [4 1] [3 11] [2 2] [1 4] [0 4]))
         (mul (make-poly 'x '([3 1] [2 2] [0 4]))
              (make-poly 'x '([3 2] [1 1] [0 1]))))))

(deftest dividing
  (let [dividend (make-poly 'x '([5 1] [0 -1]))
        divisor (make-poly 'x '([2 1] [0 -1]))
        quotient (make-poly 'x '([3 1] [1 1]))
        remainder (make-poly 'x '([1 1] [0 -1]))]
    (let [{:keys [quot rem]} (div dividend divisor)]
      (is (= quotient quot))
      (is (= remainder rem))
      (is (= dividend (add (mul quot divisor) rem))))))

(deftest polynomials
  (adding)
  (negating)
  (subtracting)
  (multiplying)
  (dividing))

(defn test-ns-hook []
  (polynomials))
