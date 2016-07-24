(ns sicp-clj.core)

(defn error [& args]
  (throw (RuntimeException. (apply str args))))
