(ns igoki.view
  (:require
    [igoki.ui :as ui]
    [igoki.util :as util]
    [clojure.java.io :as io])
  (:import
    (org.opencv.core MatOfPoint2f Mat Rect Size Core)
    (org.opencv.calib3d Calib3d)
    (org.opencv.imgproc Imgproc)
    (java.util UUID)
    (java.io File)
    (org.deeplearning4j.util ModelSerializer)
    (org.deeplearning4j.nn.multilayer MultiLayerNetwork)
    (org.nd4j.linalg.api.ndarray INDArray)
    (org.datavec.image.loader ImageLoader)
    (org.opencv.imgcodecs Imgcodecs)))

;; Moving average
;; 'dirty' flags on positions (set against previous x frames as reference)
;; Single-shot, multi stone .output call.
;;

;; Stone detection, neural network

(defn load-net [^File nm]
  (ModelSerializer/restoreMultiLayerNetwork nm))

(def net
  (let [tmp (File/createTempFile "igoki" "cnet")
        cnet (io/input-stream (io/resource "supersimple.cnet"))]
    (io/copy cnet tmp)
    (let [net (load-net tmp)]
      (.delete tmp)
      net)))

(def loader (ImageLoader. 10 10 3))
(def block-size 10)


(defn eval-net [flat px py]
  (let [smat (.submat flat (Rect. (- px (/ block-size 2)) (- py (/ block-size 2)) 10 10))
        img (util/mat-to-buffered-image smat nil)
        ^INDArray d (.asMatrix loader img)
        _ (.divi d 255.0)
        d (.reshape d (int-array [1 300]))
        ^INDArray o (.output ^MultiLayerNetwork net d)]
    (.release smat)
    (for [i (range 3)]
      (.getFloat o (int i)))))


(defn target-points [size]
  (let [extent (* block-size size)]
    [[block-size block-size] [extent block-size] [extent extent] [block-size extent]]))

(defn ref-size [size]
  (Size. (* block-size (inc size)) (* block-size (inc size))))

(defn sample-points [corners size]
  (let [[ctl ctr cbr cbl] corners
        divide (fn [[x1 y1] [x2 y2]]
                 (let [xf (/ (- x2 x1) (dec size))
                       yf (/ (- y2 y1) (dec size))]
                   (map (fn [s] [(+ x1 (* s xf)) (+ y1 (* s yf))]) (range size))))
        leftedge (divide ctl cbl)
        rightedge (divide ctr cbr)]
    (map
      (fn [left right] (divide left right))
      leftedge rightedge)))


(defn dump-points [ctx]
  (let [flat (-> ctx :camera :flattened)
        samplepoints (-> ctx :view :samplepoints)
        board (-> ctx :board)
        id (first (.split (.toString (UUID/randomUUID)) "[-]"))]
    (when flat
      (doseq [[py rows] (map-indexed vector samplepoints)]
        (doseq [[px [x y]] (map-indexed vector rows)]
          (let [r (Rect. (- x (/ block-size 2)) (- y (/ block-size 2)) block-size block-size)
                p (get-in board [py px])]
            (Imgcodecs/imwrite (str "samples/" (if p (name p) "e") "-" px "-" py "-" id ".png") (.submat ^Mat flat r)))
          ))
      samplepoints)))

;; Given circles and samplepoints, construct a possible board view.
(def reading (atom false))

(defn read-board [ctx]
  (if-not @reading
    (try
      (reset! reading true)
      (let [{{:keys [homography samplepoints]} :view
             {:keys [raw flattened flattened-pimage]} :camera
             {:keys [size]} :goban} ctx
            new-flat (or flattened (Mat.))]
        (if homography
          (do
            (Imgproc/warpPerspective raw new-flat homography (ref-size size))
            (let [brightness (/ (apply + (take 3 (seq (.val (Core/mean new-flat))))) 3.0)]
              (.convertTo new-flat new-flat -1 1 (- 140 brightness))
              (Core/normalize new-flat new-flat 0 255 Core/NORM_MINMAX))


            (let [board
                  (mapv
                    (fn [row]
                      (mapv
                        (fn [[px py]]
                          (let [[b e w] (eval-net new-flat px py)]
                            (cond
                              (> b 0.5) :b
                              (> w 0.7) :w)))
                        row))
                    samplepoints)]
              (->
                ctx
                (assoc-in [:camera :flattened] new-flat)
                (assoc-in [:camera :flattened-pimage]
                  (util/mat-to-pimage new-flat
                    (:bufimg flattened-pimage)))
                (assoc :board board))))
          ctx)
        )
      (finally
        (reset! reading false)))
    ctx))



(defn camera-updated [ctx]
  (swap! ctx read-board))


(defn gather-reference [context homography]
  (let [{{:keys [size]} :goban} context
        samplecorners (target-points size)
        samplepoints (sample-points samplecorners size)]
    {:homography homography
     :shift [0 0]
     :samplecorners samplecorners
     :samplepoints samplepoints}))

(defn update-reference [ctx]
  (let [{{:keys [homography]} :view :as context} @ctx]
    (when homography
      (swap! ctx assoc :view (gather-reference context homography)))))

(defn update-homography [ctx]
  (util/with-release
    [target (MatOfPoint2f.)
     origpoints (MatOfPoint2f.)]
    (let [{{:keys [points size]} :goban :as context} ctx
          target (util/vec->mat target (target-points size))
          origpoints (util/vec->mat origpoints points)
          homography
          (Calib3d/findHomography ^MatOfPoint2f origpoints ^MatOfPoint2f target
            Calib3d/FM_RANSAC 3.0)]
      (if homography
        (assoc-in ctx [:view :homography] homography)
        ctx))))

;; Super naive implementation - needs work.
(defn closest-samplepoint [samplepoints [x y :as p]]
  (first
    (reduce
      (fn [[w l :as winningpoint] s]
        (let [length (util/line-length-squared [p s])]
          (cond
            (or (nil? w) (< length l)) [s length]
            :else winningpoint)))
      nil
      (mapcat identity samplepoints))))

