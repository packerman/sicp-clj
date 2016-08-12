(ns sicp-clj.data.generic.algebra
  (:require [clojure.test :refer :all]
            [sicp-clj.core :refer :all]
            [sicp-clj.data.generic.arithmetic :refer [install-type
                                                      add sub mul div =zero?]]))

(defn make-poly [variable terms]
  ^{:type ::Polynomial}
  {:variable variable
   :terms terms})

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

(declare mul-terms)

(defn mul-poly [{variable-1 :variable terms-1 :terms :as p1}
                {variable-2 :variable terms-2 :terms :as p2}]
  {:pre [(poly? p1) (poly? p2) (= variable-1 variable-2)]}
  (make-poly variable-1
             (mul-terms terms-1 terms-2)))

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

(defn- mul-terms [terms-1 terms-2]
  (letfn [(mul-term-by-all-terms [[order-1 coeff-1 :as t] terms]
            (when-let [[[order-2 coeff-2 :as first-term] & rest-terms] (seq terms)]
              (adjoin-term (make-term (+ order-1 order-2)
                                      (mul coeff-1 coeff-2))
                           (mul-term-by-all-terms t rest-terms))))]
    (when-let [[t1 & rest-terms-1] (seq terms-1)]
      (add-terms (mul-term-by-all-terms t1 terms-2)
                 (mul-terms rest-terms-1 terms-2)))))

(defn- adjoin-term [[_ coeff :as t] terms]
  (if (=zero? coeff)
    terms
    (cons t terms)))

(install-type ::Polynomial
              :add add-poly
              :sub (fn [_ _])
              :mul mul-poly
              :div (fn [_ _])
              :zero zero-poly?
              :raise identity)

(deftest adding
  (is (= (make-poly 'x '([3 2] [2 3] [1 2] [0 -8]))
         (add (make-poly 'x '([3 1] [2 5] [1 -2]))
              (make-poly 'x '([3 1] [1 3] [0 -6]))
              (make-poly 'x '([2 -2] [1 1] [0 -2]))))))

(deftest multiplying
  (is (= (make-poly 'x '([6 2] [5 4] [4 1] [3 11] [2 2] [1 4] [0 4]))
         (mul (make-poly 'x '([3 1] [2 2] [0 4]))
              (make-poly 'x '([3 2] [1 1] [0 1]))))))

(deftest polynomials
  (adding)
  (multiplying))

(defn test-ns-hook []
  (polynomials))



