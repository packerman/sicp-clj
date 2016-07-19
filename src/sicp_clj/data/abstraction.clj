(ns sicp-clj.data.abstraction
  (:require [sicp-clj.procedures.processes :refer [gcd]]
            [sicp-clj.procedures.elements :refer [average abs]]))

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

(defn list-ref [items n]
  (let [[x & xs] items]
    (if (zero? n)
      x
      (recur xs (dec n)))))

(defn list-length [items]
  (if (empty? items)
    0
    (inc (list-length (next items)))))

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1)
          (append (next list1) list2))))

(defn my-last [lst]
  (when (seq lst)
    (let [[x & xs] lst]
      (if (empty? xs)
        x
        (recur xs)))))

(defn my-reverse [lst]
  (letfn [(iter [rev lst]
            (if-let [[x & xs] lst]
              (recur (cons x rev) xs)
              rev))]
    (iter '() lst)))

(defn cc [amount coins]
  (cond
    (zero? amount) 1
    (or (neg? amount) (empty? coins)) 0
    :else (+ (cc amount (rest coins))
             (cc (- amount (first coins)) coins))))

(defn same-parity [& lst]
  (when-let [[x & xs] lst]
    (cons x
          (filter #(even? (- % x)) xs))))

(defn count-leaves [x]
  (if (seq? x)
    (reduce + (map count-leaves x))
    1))

(defn deep-reverse [lst]
  (if (not (seq? lst))
    lst
    (reverse (map deep-reverse lst))))

(defn fringe [x]
  (if (not (seq? x))
    (list x)
    (mapcat fringe x)))

(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn total-weight [mobile]
  (letfn [(branch-weight [[length structure]]
            (if (number? structure)
              structure
              (mobile-weight structure)))
          (mobile-weight [[left right]]
            (+ (branch-weight left)
               (branch-weight right)))]
    (mobile-weight mobile)))

;TODO Exercise 2.29 3. (balanced)
