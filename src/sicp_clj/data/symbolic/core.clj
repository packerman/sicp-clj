(ns sicp-clj.data.symbolic.core
  (:require [clojure.test :refer :all]))

(defn memq [item s]
  (if-let [[x & xs] s]
    (if (= item x)
      s
      (recur item xs))
    false))

(deftest memq-test
         (is (false? (memq 'apple '(pear banana prue))))
         (is (= '(apple pear) (memq 'apple '(x (apple sauce) y apple pear)))))
