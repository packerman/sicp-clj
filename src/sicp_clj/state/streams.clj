(ns sicp-clj.state.streams
  (:require [clojure.test :refer :all]
            [sicp-clj.core :refer :all]))

(defn integers
  ([] (integers 1))
  ([n] (lazy-seq (cons n (integers (+ n 1))))))

(defn no-sevens []
  (filter
    #(not (divisible? % 7))
    (integers)))

(defn fibs
  ([] (fibs 0 1))
  ([a b] (lazy-seq (cons a (fibs b (+ a b))))))

(defn sieve [[x & xs]]
  (lazy-seq (cons x
                  (sieve (remove #(divisible? % x)
                                 xs)))))

(defn primes-sieve []
  (sieve (integers 2)))

(defn ones []
  (lazy-seq
    (cons 1 (ones))))

(defn add-seqs [s1 s2]
  (map + s1 s2))

(defn integers2 []
  (lazy-seq
    (cons 1
          (add-seqs (ones) (integers2)))))

(defn fibs2 []
  (lazy-cat [0 1]
            (add-seqs (next (fibs2)) (fibs2))))

(defn powers-of-2 []
  (lazy-seq
    (cons 1
          (map #(* % 2) (powers-of-2)))))

(defn primes []
  (letfn [(prime? [n]
            (->> (primes)
                 (take-while #(<= (square %) n))
                 (not-any? #(divisible? n %))))]
    (cons 2
          (filter prime? (integers 3)))))

(defn partial-sums
  ([s] (partial-sums s 0))
  ([[x & xs] part]
   (let [sum (+ x part)]
     (lazy-seq
       (cons sum
             (partial-sums xs sum))))))

(defn binary-merge [s1 s2]
  (lazy-seq
    (cond (empty? s1) s2
          (empty? s2) s1
          :else (let [f1 (first s1)
                      f2 (first s2)]
                  (cond
                    (< f1 f2) (cons f1
                                    (binary-merge (rest s1) s2))
                    (> f1 f2) (cons f2
                                    (binary-merge s1 (rest s2)))
                    :else (cons f1
                                (binary-merge (rest s1) (rest s2))))))))

(defn merge-seqs [seqs]
  (let [k (count seqs)]
    (condp = k
      0 nil
      1 (first seqs)
      2 (binary-merge (first seqs)
                      (second seqs))
      (let [[seqs-1 seqs-2] (split-at (quot k 2) seqs)]
        (binary-merge (merge-seqs seqs-1)
                      (merge-seqs seqs-2))))))

(defn scale-seq [k s]
  (map #(*' k %) s))

(defn hamming []
  (lazy-seq
    (cons 1
          (merge-seqs
            (list (scale-seq 2 (hamming))
                  (scale-seq 3 (hamming))
                  (scale-seq 5 (hamming)))))))

(deftest streams
  (testing "Infinite"
    (is (= [1 2 3 4 5] (take 5 (integers 1))))
    (is (= 117 (nth (no-sevens) 100)))
    (is (= [0 1 1 2 3 5 8 13 21 34] (take 10 (fibs))))
    (is (= 233 (nth (primes-sieve) 50)))
    (is (= [2 3 5 7 11 13 17 19 23 29] (take 10 (primes-sieve))))
    (is (= [1 1 1 1 1 1 1 1 1 1] (take 10 (ones))))
    (is (= [1 2 3 4 5 6 7 8 9 10] (take 10 (integers2))))
    (is (= [0 1 1 2 3 5 8 13 21 34] (take 10 (fibs2))))
    (is (= [1 2 4 8 16 32 64 128 256 512] (take 10 (powers-of-2))))
    (is (= [2 3 5 7 11 13 17 19 23 29] (take 10 (primes))))
    (is (= [1 3 6 10 15] (take 5 (partial-sums (integers)))))
    (is (= [1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54 60]
           (take 26 (hamming))))))
