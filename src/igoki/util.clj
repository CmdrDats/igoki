(ns igoki.util
  (:import (org.opencv.core Mat Size CvType Point)
           (java.awt.image BufferedImage DataBufferByte)
           (processing.core PImage PConstants)))

(defn mat-to-buffered-image [^Mat frame]
  (let [type (case (.channels frame)
               1 BufferedImage/TYPE_BYTE_GRAY
               3 BufferedImage/TYPE_3BYTE_BGR)
        image (BufferedImage. (.width frame) (.height frame) type)
        raster (.getRaster image)
        data-buffer ^DataBufferByte (.getDataBuffer raster)
        data (.getData data-buffer)]
    (.get frame 0 0 data)
    image))

(defn mat-to-buffered-image [^Mat frame]
  (let [type (case (.channels frame)
               1 BufferedImage/TYPE_BYTE_GRAY
               3 BufferedImage/TYPE_3BYTE_BGR)
        image (BufferedImage. (.width frame) (.height frame) type)
        raster (.getRaster image)
        data-buffer ^DataBufferByte (.getDataBuffer raster)
        data (.getData data-buffer)]
    (.get frame 0 0 data)
    image))

(defn bufimage-to-pimage [^BufferedImage bimg]
  (let [img (PImage. (.getWidth bimg) (.getHeight bimg) PConstants/ARGB)]
    (.getRGB bimg 0 0 (.-width img) (.-height img) (.-pixels img) 0 (.-width img))
    (.updatePixels img)
    img))

(defn mat-to-pimage [^Mat frame]
  (-> frame
      mat-to-buffered-image
      bufimage-to-pimage))

(defn line-length-squared [[[x1 y1] [x2 y2] :as line]]
  (+ (* (- y2 y1) (- y2 y1)) (* (- x2 x1) (- x2 x1))))

(defn line-length [[[x1 y1] [x2 y2] :as line]]
  (Math/sqrt (line-length-squared line)))

(defn line-to-point-dist [[[x1 y1] [x2 y2] :as line] [x0 y0]]
  (/ (Math/abs (+ (- (* x0 (- y2 y1)) (* y0 (- x2 x1)) (* y2 x1)) (* x2 y1)))
     (line-length line)))

(defn point-along-line [[[p1x p1y] [p2x p2y] :as line] percent]
  [(+ p1x (* (- p2x p1x) percent))
   (+ p1y (* (- p2y p1y) percent))])

(defn flipped-line [[p1 p2]]
  [p2 p1])


(defn matrix [w h & data]
  (doto (Mat. w h (CvType/CV_64F))
    (.put 0 0 (double-array data))))

(defn translate [m x y]
  (doto m
    (.put 0 2 (double-array [(+ x (first (.get m 0 2)))]))
    (.put 1 2 (double-array [(+ y (first (.get m 1 2)))]))))

(defn read-mat "Ai, really?" [{:keys [size type data]}]
  (doto (Mat. (Size. (first size) (second size)) type)
    (.put 0 0 (double-array data)))  )

(defn write-mat "Yes, really." [mat]
  {:type (.type mat)
   :size [(.width mat) (.height mat)]
   :data (read-string (.replace (.dump mat) ";\n " ","))})

(defn vec->mat
  [mat vec]
  (doto mat
    (.fromList (map (fn [[x y]] (Point. x y)) vec))))

(defmacro -->
  "Threads images through the forms. Passing images from call to call and relasing
   all but the last image."
  ([x] `(let [next# (Mat.)] (.copyTo ~x next#) next#))
  ([x form]
   `(let [img# ~x
          next# (Mat.)]
      (~(first form) img# next# ~@(next form))
      next#))
  ([x form & more]
   `(--> (--> ~x ~form) ~@more)))

(defn add-watch-path
  "Similar to add-watch, but takes a ks arg that narrow the scope down to watched changes on a
  single path of an atom. The contents of the atom needs to be traversable with get-in"
  [atom watch-key ks f]
  (add-watch
    atom watch-key
    (fn [_ _ old new]
      (let [o (get-in old ks)
            n (get-in new ks)]
        (if-not
          (= o n)
          (f watch-key atom o n))))))