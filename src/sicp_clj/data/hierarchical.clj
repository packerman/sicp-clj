(ns sicp-clj.data.hierarchical
  (:require [clojure.test :refer :all]
            [sicp-clj.procedures.elements :refer [square]]))

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

;TODO Exercise 2.29 3. (balanced mobile)

(defn tree-map [f tree]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

(deftest tree-map-test
  (is (= (tree-map square
                   (list 1
                         (list 2 (list 3 4) 5)
                         (list 6 7)))
         '(1 (4 (9 16) 25) (36 49)))))

;TODO Exercise 2.32

