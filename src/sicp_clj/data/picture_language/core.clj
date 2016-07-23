(ns sicp-clj.data.picture-language.core
  (:require [clojure.test :refer :all]
            [sicp-clj.data.picture-language.vector :refer :all]
            [sicp-clj.data.picture-language.frame :refer :all]
            [sicp-clj.data.picture-language.display :refer :all]))

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

(defn rotate90 [painter]
  (transform-painter
    painter
    (->Vector 1.0 0.0)
    (->Vector 1.0 1.0)
    (->Vector 0.0 0.0)))

(defn beside [painter1 painter2]
  (let [split-point (->Vector 0.5 0.0)
        paint-left (transform-painter
                     painter1
                     (->Vector 0.0 0.0)
                     split-point
                     (->Vector 0.0 1.0))
        paint-right (transform-painter
                      painter2
                      split-point
                      (->Vector 1.0 0.0)
                      (->Vector 0.5 1.0))]
    (fn [frame]
      (paint-left frame)
      (paint-right frame))))

(deftest image-language
  (let [painter (image-painter (read-image "images/lena.png"))]
    (display "image painter" painter)
    (display "flip vertical" (flip-vert painter))
    (display "rotate 90" (rotate90 painter))
    (display "beside" (beside painter painter))
    ))

