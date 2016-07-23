(ns sicp-clj.data.picture-language.frame
  (:require [sicp-clj.data.picture-language.vector :refer :all])
  (:import [sicp_clj.data.picture_language.vector Vector]))

(defrecord Frame [^Vector origin ^Vector edge1 ^Vector edge2])

(defn frame-coord-map [^Frame {:keys [origin edge1 edge2]}]
  (fn [^Vector {:keys [x y]}]
    (add-vector origin
                (scale-vector x edge1)
                (scale-vector y edge2))))
