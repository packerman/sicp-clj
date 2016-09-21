(ns sicp-clj.core)

(defn error [& args]
  (throw (RuntimeException. (apply str args))))

(defn square [x] (*' x x))

(defn divisible? [x y]
  (zero? (rem x y)))

