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

(defn flip-horiz [painter]
  (transform-painter
    painter
    (->Vector 1.0 0.0)
    (->Vector 0.0 0.0)
    (->Vector 1.0 1.0)))


(defn rotate90 [painter]
  (transform-painter
    painter
    (->Vector 1.0 0.0)
    (->Vector 1.0 1.0)
    (->Vector 0.0 0.0)))

(defn rotate180 [painter]
  (transform-painter
    painter
    (->Vector 1.0 1.0)
    (->Vector 0.0 1.0)
    (->Vector 1.0 0.0)))

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

(defn below [painter1 painter2]
  (let [split-point (->Vector 0.0 0.5)
        paint-down (transform-painter
                     painter1
                     (->Vector 0.0 0.0)
                     (->Vector 1.0 0.0)
                     split-point)
        paint-top (transform-painter
                    painter2
                    split-point
                    (->Vector 1.0 0.5)
                    (->Vector 0.0 1.0))]
    (fn [frame]
      (paint-down frame)
      (paint-top frame))))

(defn split [split1 split2]
  (letfn [(splitter [painter n]
            (if (zero? n)
              painter
              (let [smaller (splitter painter (dec n))]
                (split1 painter
                        (split2 smaller smaller)))))]
    splitter))

(def right-split (split beside below))
(def up-split (split below beside))

(defn corner-split [painter n]
  (if (zero? n)
    painter
    (let [up (up-split painter (dec n))
          right (right-split painter (dec n))]
      (let [top-left (beside up up)
            bottom-right (below right right)
            corner (corner-split painter (dec n))]
        (beside (below painter top-left)
                (below bottom-right corner))))))

(defn square-of-four [tl tr bl br]
  (fn [painter]
    (let [top (beside (tl painter)
                      (tr painter))
          bottom (beside (bl painter)
                         (br painter))]
      (below bottom top))))

(defn square-limit [painter n]
  (let [combine4 (square-of-four flip-horiz
                                 identity
                                 rotate180
                                 flip-vert)]
    (combine4 (corner-split painter n))))

(deftest image-language
  (let [painter (image-painter (read-image "images/lena.png"))]
    ;(display "image painter" painter)
    ;(display "flip vertical" (flip-vert painter))
    ;(display "rotate 90" (rotate90 painter))
    ;(display "beside" (beside painter painter))
    ;(display "below" (below painter painter))
    ;(display "flip horizontal" (flip-horiz painter))
    ;(display "right split" (right-split painter 4))
    ;(display "corner split" (corner-split painter 5))
    (display "square limit" (square-limit painter 2))))

