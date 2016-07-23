(ns sicp-clj.data.picture-language.display
  (:require [clojure.java.io :as io]
            [sicp-clj.data.picture-language.vector :refer :all]
            [sicp-clj.data.picture-language.frame :refer :all])
  (:import [javax.swing JFrame]
           [javax.imageio ImageIO]
           [java.awt Graphics2D Component]
           [java.awt.geom AffineTransform]
           [sicp_clj.data.picture_language.frame Frame]))

(defn read-image [name]
  (ImageIO/read
    (io/resource name)))

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

(defn display
  ([title painter] (display title painter 800 600))
  ([title painter width height]
   (letfn [(make-main-frame []
             (->Frame zero-vector
                      (->Vector width 0)
                      (->Vector 0 height)))
           (make-painting-component [frame]
             (proxy [Component] []
               (paint [graphics]
                 (binding [*graphics2d* (cast Graphics2D graphics)]
                   (painter frame)))))]
     (doto (JFrame. title)
       (.setSize width height)
       (.add (make-painting-component
               (make-main-frame)))
       (.setVisible true)))))
