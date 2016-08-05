(ns sicp-clj.data.symbolic.huffman
  (:require [clojure.set :as sets]
            [clojure.test :refer :all]
            [sicp-clj.core :refer [error]]))

(defn leaf? [node] (:leaf node))

(defn symbols [node]
  (if (leaf? node)
    #{(:symbol node)}
    (:symbols node)))

(defn decode [bits tree]
  (letfn [(choose-branch [bit branch]
            (condp = bit
              0 (:left branch)
              1 (:right branch)
              (error "bad bit: " bit)))
          (decode-1 [{:keys [branch] :as state} bit]
            (let [next-branch (choose-branch bit branch)]
              (if (leaf? next-branch)
                (-> state
                    (update :decoded conj (:symbol next-branch))
                    (assoc :branch tree))
                (assoc state :branch next-branch))))]
    (:decoded
      (reduce
        decode-1
        {:decoded []
         :branch  tree}
        bits))))

(defn make-leaf [symbol weight]
  {:leaf   true
   :symbol symbol
   :weight weight})

(defn make-tree [left right]
  {:leaf    false
   :left    left
   :right   right
   :symbols (sets/union (symbols left)
                        (symbols right))
   :weight  (+ (:weight left)
               (:weight right))})

(defn adjoin-set [e set]
  (if-let [[x & xs] (seq set)]
    (if (< (:weight e) (:weight x))
      (cons e set)
      (cons x
            (adjoin-set e xs)))
    (list e)))

(defn make-leaf-set [pairs]
  (->> pairs
       (map
         (fn [[symbol weight]]
           (make-leaf symbol weight)))
       (reduce
         #(adjoin-set %2 %1)
         '())))

(defn encode [message tree]
  (letfn [(encode-symbol [symbol]
            (encode-1 symbol tree []))
          (encode-1 [symbol branch bits]
            (if (leaf? branch)
              bits
              (let [{:keys [left right]} branch]
                (condp contains? symbol
                  (symbols left) (recur symbol left (conj bits 0))
                  (symbols right) (recur symbol right (conj bits 1))
                  (error "unknown symbol: " symbol)))))]
    (mapcat
      encode-symbol
      message)))

(defn generate-huffman-tree [freqs]
  (letfn [(successive-merge [nodes]
            (if (= 1 (count nodes))
              (first nodes)
              (let [[node-1 node-2 & nodes-rest] nodes]
                (recur
                  (adjoin-set (make-tree node-1 node-2)
                              nodes-rest)))))]
    (successive-merge
      (make-leaf-set freqs))))

(deftest huffman
  (let [sample-tree (make-tree
                      (make-leaf 'A 4)
                      (make-tree
                        (make-leaf 'B 2)
                        (make-tree
                          (make-leaf 'C 1)
                          (make-leaf 'D 1))))
        test-bits [0 1 1 0 0 1 0 1 0 1 1 1 0]
        test-message '[A C A B B D A]]
    (testing "decode"
      (is (= test-message (decode test-bits sample-tree))))
    (testing "encode"
      (is (= test-bits (encode test-message sample-tree))))
    (testing "compression"
      (let [message (seq "BACADAEAFABBAAAGAH")
            tree (generate-huffman-tree
                   (frequencies message))
            bits (encode message tree)]
        (is (= 42 (count bits)))
        (is (= message (decode bits tree)))))))
