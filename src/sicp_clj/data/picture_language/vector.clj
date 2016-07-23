(ns sicp-clj.data.picture-language.vector
  (:require [clojure.test :refer :all]))

(defrecord Vector [x y])

(def zero-vector (->Vector 0 0))

(defn add-vector
  ([] zero-vector)
  ([v] v)
  ([{x1 :x y1 :y} {x2 :x y2 :y}] (->Vector (+ x1 x2) (+ y1 y2)))
  ([v1 v2 & vs] (reduce add-vector
                        (add-vector v1 v2)
                        vs)))

(defn sub-vector
  ([] zero-vector)
  ([{:keys [x y]}] (->Vector (- x) (- y)))
  ([{x1 :x y1 :y} {x2 :x y2 :y}] (->Vector (- x1 x2) (- y1 y2))))

(defn scale-vector [c {:keys [x y]}]
  (->Vector (* c x) (* c y)))

(deftest vector-operations-test
  (testing "adding vectors"
    (is (= (->Vector 0 0) (add-vector)))
    (is (= (->Vector 1 2) (add-vector (->Vector 1 2))))
    (is (= (->Vector 4 7) (add-vector (->Vector 1 2) (->Vector 3 5))))
    (is (= (->Vector 14 27) (add-vector (->Vector 1 2)
                                        (->Vector 3 5)
                                        (->Vector 10 20))))
    (is (= (->Vector 34.2 37.1) (add-vector (->Vector 20.2 10.1)
                                            (->Vector 1 2)
                                            (->Vector 3 5)
                                            (->Vector 10 20)))))
  (testing "scaling vectors"
    (is (= (->Vector 10 20) (scale-vector 10 (->Vector 1 2))))
    (is (= (->Vector 5.0 10.0) (scale-vector 0.5 (->Vector 10 20))))
    (is (= (->Vector 5 10) (scale-vector 1/2 (->Vector 10 20))))))
