(ns sicp-clj.data.abstraction
  (:require [sicp-clj.procedures.processes :refer [gcd]]))

(defn make-rat [n d]
  (if (neg? d)
    (recur (- n) (- d))
    (let [g (gcd n d)]
      {:numer (/ n g)
       :denom (/ d g)})))

(defn add-rat [{n1 :numer d1 :denom} {n2 :numer d2 :denom}]
  (make-rat (+ (* n1 d2) (* n2 d1))
            (* d1 d2)))

(defn sub-rat [{n1 :numer d1 :denom} {n2 :numer d2 :denom}]
  (make-rat (- (* n1 d2) (* n2 d1))
            (* d1 d2)))

(defn mul-rat [{n1 :numer d1 :denom} {n2 :numer d2 :denom}]
  (make-rat (* n1 n2)
            (* d1 d2)))

(defn div-rat [{n1 :numer d1 :denom} {n2 :numer d2 :denom}]
  (make-rat (* n1 d2)
            (* d1 n2)))

(defn equal-rat? [{n1 :numer d1 :denom} {n2 :numer d2 :denom}]
  (= (* n1 d2) (* n2 d1)))

(defn print-rat [{:keys [numer denom]}]
  (println (str numer "/" denom)))
