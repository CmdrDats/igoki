(ns igoki.util
  (:require
    [clojure.java.io :as io])
  (:import
    (org.opencv.core Mat Size CvType Point MatOfPoint2f MatOfPoint)
    (java.awt.image BufferedImage DataBufferByte)
    (de.schlichtherle.truezip.file TFile TFileWriter TArchiveDetector)
    (java.io InputStream ByteArrayInputStream ByteArrayOutputStream)))

(defn star-points [size]
  (case size
    9 [[2 2] [4 4] [2 6] [6 2] [6 6]]
    13 [[3 3] [6 6] [3 9] [9 3] [9 9]]
    (for [x (range 3) y (range 3)]
      [(+ 3 (* x 6)) (+ 3 (* y 6))])))

(defn mat-to-buffered-image [^Mat frame ^BufferedImage bimg]
  (let [type (case (.channels frame)
               1 BufferedImage/TYPE_BYTE_GRAY
               3 BufferedImage/TYPE_3BYTE_BGR)
        image
        (if (and bimg (= type (.getType bimg))
              (= (.getWidth bimg) (.width frame)) (= (.getHeight bimg) (.height frame)))
          bimg
          (BufferedImage. (.width frame) (.height frame) type))
        raster (.getRaster image)
        data-buffer ^DataBufferByte (.getDataBuffer raster)
        data (.getData data-buffer)]
    (.get frame 0 0 data)
    image))

(defn mat-to-pimage [^Mat frame ^BufferedImage oldbuffer]
  (when (and (> (.rows frame) 0) (> (.cols frame) 0))
    {:bufimg (mat-to-buffered-image frame oldbuffer)}))

(defmacro with-release
  "A let block, calling .release on each provided binding at the end, in a finally block."
  [bindings & body]
  (let [release (map (fn [b] `(.release ~(first b))) (partition 2 bindings))]
    `(let ~bindings
       (try
         ~@body
         (finally
           ~@release)))))

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

(defn update-edges [points]
  (partition 2 (interleave points (take 4 (drop 1 (cycle points))))))

(defn update-closest-point [points p]
  (let [indexed-dist
        (->>
          points
          (map-indexed (fn [i g] [(line-length-squared [g p]) i]))
          sort)
        [_ i :as e] (first indexed-dist)]
    (assoc points i p)))

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

(defmulti mat->seq class)
(defmethod mat->seq :default [m]
  (throw (RuntimeException. (str "Unknown type for conversion to vec: " (class m)))))

(defmethod mat->seq MatOfPoint2f [m]
  (map (fn [i] [(.-x i) (.-y i)]) (seq (.toArray m))))

(defmethod mat->seq MatOfPoint [m]
  (map (fn [i] [(.-x i) (.-y i)]) (seq (.toArray m))))

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


(defn interleave-all "Greedy version of `interleave`, Ref. http://goo.gl/KvzqWb."
  ([] '())
  ([c1] (lazy-seq c1))
  ([c1 c2]
   (lazy-seq
     (let [s1 (seq c1) s2 (seq c2)]
       (cond
         (and s1 s2)
         (cons (first s1) (cons (first s2)
                                (interleave-all (rest s1) (rest s2))))
         s1 s1
         s2 s2))))
  ([c1 c2 & colls]
   (lazy-seq
     (let [ss (filter identity (map seq (conj colls c2 c1)))]
       (concat (map first ss)
               (apply interleave-all (map rest ss)))))))

(defn iupdate-in
  "update-in using loop/recur so that it doesn't blow the stack"
  [m ks f & args]
  (loop [oks ks
         a (get-in m ks)
         s (apply f a args)]
    (if oks
      (let [r (butlast oks)]
        (recur r (get-in m oks) (assoc (get-in m r) (last oks) s)))
      s)))

(defn std-dev
  "Find the standard deviation of a given number of samples (list of numbers)"
  [samples]
  (let [n (count samples)
        mean (if (zero? n) 0 (/ (reduce + samples) n))
        intermediate (map #(Math/pow (- %1 mean) 2) samples)]
    (if (zero? n)
      0 (Math/sqrt
          (/ (reduce + intermediate) n)))))


(defn zip-add-file [zipname destname ^InputStream input]
  (let [f (TFile. ^String zipname ^String destname (TArchiveDetector.".zip"))]
    (TFile/cp input f)))

(defn zip-add-file-string [zipname destname ^String input]
  (zip-add-file zipname destname (ByteArrayInputStream. (.getBytes input))))


(defn zip-read-file [zipname filename]
  (let [out (ByteArrayOutputStream.)]
    (.output (TFile. ^String zipname ^String filename) out)
    (.toByteArray out)))

(defn hamming-dist [s1 s2]
  (let [n (min (count s1) (count s2))
        dist* (fn [^long n ^long i ^long a]
                (if (zero? n)
                  a
                  (recur (dec n)
                         (inc i)
                         (if (not= (get s1 i) (get s2 i))
                           (inc a)
                           a))))]
    (dist* n 0 (- (max (count s1) (count s2)) n))))