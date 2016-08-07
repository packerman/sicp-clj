(ns sicp-clj.data.abstraction
  (:refer-clojure :exclude [rational?])
  (:require [sicp-clj.procedures.processes :refer [gcd]]
            [sicp-clj.procedures.elements :refer [average abs]]))

(defn make-rat [n d]
  (if (neg? d)
    (recur (- n) (- d))
    (let [g (gcd n d)]
      ^{:type ::Rational}
      {:numer (/ n g)
       :denom (/ d g)})))

(defn rational? [r]
  (= ::Rational (type r)))

(defn add-rat [{n1 :numer d1 :denom :as r1} {n2 :numer d2 :denom :as r2}]
  {:pre [(rational? r1) (rational? r2)]}
  (make-rat (+ (* n1 d2) (* n2 d1))
            (* d1 d2)))

(defn sub-rat [{n1 :numer d1 :denom :as r1} {n2 :numer d2 :denom :as r2}]
  {:pre [(rational? r1) (rational? r2)]}
  (make-rat (- (* n1 d2) (* n2 d1))
            (* d1 d2)))

(defn mul-rat [{n1 :numer d1 :denom :as r1} {n2 :numer d2 :denom :as r2}]
  {:pre [(rational? r1) (rational? r2)]}
  (make-rat (* n1 n2)
            (* d1 d2)))

(defn div-rat [{n1 :numer d1 :denom :as r1} {n2 :numer d2 :denom :as r2}]
  {:pre [(rational? r1) (rational? r2)]}
  (make-rat (* n1 d2)
            (* d1 n2)))

(defn rat->double [{:keys [numer denom] :as rat}]
  {:pre [(rational? rat)]}
  (double (/ numer denom)))

(defn equal-rat? [{n1 :numer d1 :denom} {n2 :numer d2 :denom}]
  (= (* n1 d2) (* n2 d1)))

(defn print-rat [{:keys [numer denom]}]
  (println (str numer "/" denom)))

(defn make-segment [start-point end-point]
  {:start start-point :end end-point})

(defn make-point [x y]
  {:x x :y y})

(defn print-point [{:keys [x y]}]
  (println (str "(" x ", " y ")")))

(defn midpoint-segment [{:keys [start end]}]
  (let [{x1 :x y1 :y} start
        {x2 :x y2 :y} end]
    (make-point (average x1 x2)
                (average y1 y2))))

(defn make-rectangle [left-bottom-point right-top-point]
  {:left-bottom left-bottom-point
   :right-top   right-top-point})

(defn width-rectangle [{{left-bottom-x :x} :left-bottom {right-top-x :x} :right-top}]
  (abs (- right-top-x left-bottom-x)))

(defn height-rectangle [{{left-bottom-y :y} :left-bottom {right-top-y :y} :right-top}]
  (abs (- right-top-y left-bottom-y)))

(defn perimeter-rectagle [rectangle]
  (+ (* 2 (width-rectangle rectangle)) (* 2 (height-rectangle rectangle))))

(defn area-rectangle [rectangle]
  (* (width-rectangle rectangle) (height-rectangle rectangle)))

;TODO 2.1.4 Extended Exercise: Interval Arithmetic
