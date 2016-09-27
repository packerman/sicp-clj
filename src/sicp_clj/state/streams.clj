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

(defn partial-sums [s]
  (lazy-seq
    (cons
      (first s)
      (add-seqs (partial-sums s)
                (rest s)))))

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

(defn scale-seq [k s]
  (map #(*' k %) s))

(defn hamming []
  (lazy-seq
    (cons 1
          (binary-merge (scale-seq 2 (hamming))
                        (binary-merge (scale-seq 3 (hamming))
                                      (scale-seq 5 (hamming)))))))

(defn expand [num den radix]
  (lazy-seq
    (cons (quot (*' num radix) den)
          (expand (rem (*' num radix) den)
                  den
                  radix))))

(defn integrate-series [series]
  (letfn [(integrate [s n]
            (lazy-seq
              (cons (* (/ n) (first s))
                    (integrate (rest s) (inc n)))))]
    (integrate series 1)))

(defn exp-series []
  (lazy-seq
    (cons 1
          (integrate-series (exp-series)))))

(defn negate-series [series]
  (map - series))

(declare sine-series)

(defn cosine-series []
  (cons 1
        (negate-series
          (integrate-series (sine-series)))))

(defn sine-series []
  (lazy-seq
    (cons 0
          (integrate-series (cosine-series)))))

#_(defn mul-series [s1 s2])
;TODO Implement mul-series, uncomment test for sin^2 x + cos^2 x = 1

(defn pairs [s t]
  (lazy-seq
    (cons
      (vector (first s) (first t))
      (interleave
        (map #(vector (first s) %) (rest t))
        (pairs (rest s) (rest t))))))

(defn triples
  ([[x & xs] [y & ys] [z & zs]]
   (lazy-seq
     (cons
       (vector x y z)
       (interleave
         (interleave
           (map #(apply vector x %) (pairs ys zs))
           (map #(vector x y %) zs))
         (triples xs ys zs)))))
  ([s] (triples s s s)))

(defn pythagorean-triples []
  (->> (triples (integers))
       (filter
         (fn [[a b c]]
           (= (+ (square a) (square b)) (square c))))))

(defn merge-weighted [s t weight]
  (lazy-seq
    (let [x (first s)
          y (first t)]
      (cond
        (< (weight x) (weight y)) (cons x
                                        (merge-weighted (rest s) t weight))
        (> (weight x) (weight y)) (cons y
                                        (merge-weighted s (rest t) weight))
        :else (cons x
                    (cons y
                          (merge-weighted (rest s) (rest t) weight)))))))

(defn weighted-pairs
  ([s t weight]
   (lazy-seq
     (cons
       (vector (first s) (first t))
       (merge-weighted
         (map #(vector (first s) %) (rest t))
         (weighted-pairs (rest s) (rest t) weight)
         weight))))
  ([s weight]
   (weighted-pairs s s weight)))

(defn weighted-triples
  ([[x & xs] [y & ys] [z & zs] weight]
   (lazy-seq
     (cons
       (vector x y z)
       (merge-weighted
         (merge-weighted
           (map #(apply vector x %)
                (weighted-pairs ys zs
                                #(weight (apply vector x %))))
           (map #(vector x y %) zs)
           weight)
         (weighted-triples xs ys zs weight)
         weight))))
  ([s weight] (weighted-triples s s s weight)))

(defn ramanujan-numbers []
  (let [sum-of-cubes (fn [[i j]]
                       (+ (cube i) (cube j)))]
    (->> (weighted-pairs (integers) sum-of-cubes)
         (partition 2 1)
         (filter (fn [[p1 p2]]
                   (= (sum-of-cubes p1) (sum-of-cubes p2))))
         (map (fn [[p _]] (sum-of-cubes p))))))

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
           (take 26 (hamming))))
    (is (= [1 4 2 8 5 7] (take 6 (expand 1 7 10))))
    (is (= [3 7 5] (take 3 (expand 3 8 10))))
    (is (= [1 1/2 1/3 1/4 1/5] (take 5 (integrate-series (ones)))))
    (is (= [1 1 1/2 1/6 1/24] (take 5 (exp-series))))
    (is (= [0 1 0 -1/6 0 1/120] (take 6 (sine-series))))
    (is (= [1 0 -1/2 0 1/24] (take 5 (cosine-series))))
    #_(is (= [1 0 0 0 0] (take 5 (add-seqs (mul-series (sine-series) (sine-series))
                                           (mul-series (cosine-series) (cosine-series))))))
    (is (= #{[1 1 1] [1 1 2] [1 2 2] [1 1 3] [1 2 3] [2 2 2] [1 1 4] [1 2 4] [2 2 3] [1 1 5]
             [1 3 3] [1 2 5] [2 3 3] [1 1 6] [2 2 4] [1 3 4] [1 2 6] [2 3 4] [1 1 7] [3 3 3]
             [1 3 5] [2 2 5] [1 4 4]}
           (set (take 23 (weighted-triples (integers) (fn [[x y z]] (+ x y z))))) ))
    (is (= #{[3 4 5] [6 8 10] [5 12 13] [9 12 15] [8 15 17]}
           (set (take 5 (filter (fn [[_ _ c]] (< c 20)) (pythagorean-triples))))))
    (is (= [1729 4104 13832 20683 32832 39312] (take 6 (ramanujan-numbers))))))
