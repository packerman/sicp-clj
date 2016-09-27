(ns sicp-clj.core
  (:require [clojure.test :refer :all]))

(defn error [& args]
  (throw (RuntimeException. (apply str args))))

(defn square [x] (*' x x))

(defn cube [x] (*' x x x))

(defn divisible? [x y]
  (zero? (rem x y)))

(defn gcd [a b]
  (cond
    (neg? a) (recur (- a) b)
    (zero? b) a
    :else (recur b (rem a b))))

(defn positions [pred coll]
  (keep-indexed #(if (pred %2) %1) coll))

(deftest positions-test
  (is (= [1 3 4] (positions odd? [0 1 2 3 5]))))
