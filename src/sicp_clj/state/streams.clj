(ns sicp-clj.state.streams
  (:require [clojure.test :refer :all]))

(defn integers
  ([] (integers 1))
  ([n] (lazy-seq (cons n (integers (+ n 1))))))

(defn no-sevens []
  (filter
    #(not (zero? (rem % 7)))
    (integers)))

(deftest streams
  (testing "Infinite"
    (is (= [1 2 3 4 5] (take 5 (integers 1))))
    (is (= 117 (nth (no-sevens) 100)))))

