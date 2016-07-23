(ns sicp-clj.data.picture-language
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all])
  (:import [javax.swing JFrame]
           [javax.imageio ImageIO]
           [java.awt Graphics2D Component]
           [java.awt.geom AffineTransform]))

(defrecord Vector [x y])

(def zero-vector (->Vector 0 0))

(defn add-vector
  ([] zero-vector)
  ([v] v)
  ([{x1 :x y1 :y} {x2 :x y2 :y}] (->Vector (+ x1 x2) (+ y1 y2)))
  ([v1 v2 & vs] (reduce add-vector
                        (add-vector v1 v2)
                        vs)))

(defn sub-vector
  ([] zero-vector)
  ([{:keys [x y]}] (->Vector (- x) (- y)))
  ([{x1 :x y1 :y} {x2 :x y2 :y}] (->Vector (- x1 x2) (- y1 y2))))

(defn scale-vector [c {:keys [x y]}]
  (->Vector (* c x) (* c y)))

(deftest vector-operations-test
  (testing "adding vectors"
    (is (= (->Vector 0 0) (add-vector)))
    (is (= (->Vector 1 2) (add-vector (->Vector 1 2))))
    (is (= (->Vector 4 7) (add-vector (->Vector 1 2) (->Vector 3 5))))
    (is (= (->Vector 14 27) (add-vector (->Vector 1 2)
                                        (->Vector 3 5)
                                        (->Vector 10 20))))
    (is (= (->Vector 34.2 37.1) (add-vector (->Vector 20.2 10.1)
                                            (->Vector 1 2)
                                            (->Vector 3 5)
                                            (->Vector 10 20)))))
  (testing "scaling vectors"
    (is (= (->Vector 10 20) (scale-vector 10 (->Vector 1 2))))
    (is (= (->Vector 5.0 10.0) (scale-vector 0.5 (->Vector 10 20))))
    (is (= (->Vector 5 10) (scale-vector 1/2 (->Vector 10 20))))))

(defrecord Frame [^Vector origin ^Vector edge1 ^Vector edge2])

(defn frame-coord-map [^Frame {:keys [origin edge1 edge2]}]
  (fn [^Vector {:keys [x y]}]
    (add-vector origin
                (scale-vector x edge1)
                (scale-vector y edge2))))

(defn read-image [name]
  (ImageIO/read
    (io/resource name)))

(defn make-main-frame [width height]
  (->Frame zero-vector
           (->Vector width 0)
           (->Vector 0 height)))

(declare ^:dynamic *graphics2d*)

(defn image-painter [image]
  (let [div-x (dec (.getWidth image))
        div-y (dec (.getHeight image))]
    (fn [^Frame {{origin-x :x origin-y :y} :origin
                 {edge1-x :x edge1-y :y}   :edge1
                 {edge2-x :x edge2-y :y}   :edge2}]
      (let [transform (AffineTransform.
                        (double (/ edge1-x div-x)) (double (/ edge1-y div-y))
                        (double (/ edge2-x div-x)) (double (/ edge2-y div-y))
                        (double origin-x) (double origin-y))]
        (.drawImage *graphics2d* image transform nil)))))

(defn make-painting-component [painter frame]
  (proxy [Component] []
    (paint [graphics]
      (binding [*graphics2d* (cast Graphics2D graphics)]
        (painter frame)))))

(defn display
  ([painter] (display painter 800 600))
  ([painter width height]
   (doto (JFrame. "Picture Language")
     (.setSize width height)
     (.add (make-painting-component
             painter
             (make-main-frame width height)))
     (.setVisible true))))

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter (->Frame new-origin
                        (sub-vector (m corner1)
                                    new-origin)
                        (sub-vector (m corner2)
                                    new-origin))))))

(defn flip-vert [painter]
  (transform-painter
    painter
    (->Vector 0.0 1.0)
    (->Vector 1.0 1.0)
    (->Vector 0.0 0.0)))