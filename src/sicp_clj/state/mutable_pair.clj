(ns sicp-clj.state.mutable-pair
  (:require [clojure.test :refer :all])
  (:import (state Cons)))

(defprotocol MutablePair
  (car [pair])
  (cdr [pair])
  (set-car! [pair newval])
  (set-cdr! [pair newval]))

(defn make-atom-pair [x y]
  (let [a (atom x)
        d (atom y)]
    (reify MutablePair
      (car [_] @a)
      (cdr [_] @d)
      (set-car! [_ newval] (reset! a newval))
      (set-cdr! [_ newval] (reset! d newval)))))

(defn make-ref-pair [x y]
  (let [a (ref x)
        d (ref y)]
    (reify MutablePair
      (car [_] @a)
      (cdr [_] @d)
      (set-car! [_ newval] (ref-set a newval))
      (set-cdr! [_ newval] (ref-set d newval)))))

(defn make-java-pair [x y]
  (let [c (Cons. x y)]
    (reify MutablePair
      (car [_] (.getCar c))
      (cdr [_] (.getCdr c))
      (set-car! [_ newval] (.setCar c newval))
      (set-cdr! [_ newval] (.setCdr c newval)))))

(deftest mutable-pair
  (letfn [(is-pair? [[x y] pair]
            (is (= x (car pair)))
            (is (= y (cdr pair))))
          (mutable-pair-test [pair]
            (set-car! pair 1)
            (set-cdr! pair 2)
            (is-pair? [1 2] pair)
            (set-cdr! pair 4)
            (is-pair? [1 4] pair)
            (set-car! pair 3)
            (is-pair? [3 4] pair))]
    (mutable-pair-test (make-atom-pair nil nil))
    (dosync
      (mutable-pair-test (make-ref-pair nil nil)))
    (mutable-pair-test (make-java-pair nil nil))))
