(ns igoki.camera
  (:require
    [igoki.util :as util]
    [igoki.simulated :as sim]
    [clojure.java.io :as io])
  (:import
    (org.opencv.core MatOfPoint2f Mat Rect Size Core)
    (org.opencv.videoio Videoio VideoCapture)
    (org.opencv.imgcodecs Imgcodecs)
    (java.util LinkedList UUID)
    (java.io File)
    (org.opencv.imgproc Imgproc)
    (org.datavec.image.loader ImageLoader)
    (org.deeplearning4j.util ModelSerializer)
    (org.opencv.calib3d Calib3d)
    (org.nd4j.linalg.api.ndarray INDArray)
    (org.deeplearning4j.nn.multilayer MultiLayerNetwork)
    (java.awt.image BufferedImage)))

;; Step 1. Setup camera
(defn setup-camera [ctx camidx]
  (let [^VideoCapture video (VideoCapture. ^int camidx Videoio/CAP_ANY)]
    (swap! ctx update
      :camera assoc
      :video video
      :stopped false)
    video))

;; Step 2. Read camera

(defn camera-read [ctx video]
  (let [camera (:camera @ctx)]
    (cond
      (or (:stopped camera) (not video)) nil

      (not (.isOpened video))
      (println "Error: Camera not opened")

      :else
      (try
        (let [frame (or (:frame camera) (Mat.))]
          (when (.read video frame)

            (swap!
              ctx update :camera
              #(assoc %
                 :raw frame
                 ;; TODO: this chows memory - better to have a hook on update for each specific
                 ;; view - this will only be needed on the first screen.
                 :pimg (util/mat-to-pimage frame (get-in % [:pimg :bufimg]))))))
        (Thread/sleep (or (-> @ctx :camera :read-delay) 500))
        (catch Exception e
          (println "exception thrown")
          (.printStackTrace e))))))

;; Helpful for debugging
(defn read-single [ctx camidx]
  (let [video (VideoCapture. (int camidx) Videoio/CAP_ANY)
        frame (Mat.)]
    (Thread/sleep 500)
    (.read video frame)
    (swap!
      ctx
      update :camera
      #(assoc %
         :raw frame
         :pimg (util/mat-to-pimage frame (get-in % [:pimg :bufimg]))))
    (.release video)))

;; For simulation stepping (debugging)
(defn read-file [ctx fname]
  (let [frame (Imgcodecs/imread (str "resources/" fname))]
    (swap!
      ctx update :camera
      #(assoc %
         :raw frame
         :pimg (util/mat-to-pimage frame (get-in % [:pimg :bufimg]))))))


;; Step 3. Specify points & find homography
(def block-size 10)

(defn target-points [size]
  (let [extent (* block-size size)]
    [[block-size block-size] [extent block-size] [extent extent] [block-size extent]]))

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

;; Step 4. Flatten image
(defn sample-points [corners size]
  (let [[ctl ctr cbr cbl] corners
        divide
        (fn [[x1 y1] [x2 y2]]
          (let [xf (/ (- x2 x1) (dec size))
                yf (/ (- y2 y1) (dec size))]
            (map (fn [s] [(+ x1 (* s xf)) (+ y1 (* s yf))]) (range size))))
        leftedge (divide ctl cbl)
        rightedge (divide ctr cbr)]
    (map
      (fn [left right] (divide left right))
      leftedge rightedge)))



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

(defn reverse-transform [ctx]
  (cond
    (<  (count (-> @ctx :goban :edges)) 4)
    (swap! ctx
      (fn [c]
        (->
          c
          (update :view dissoc :homography)
          (assoc-in [:goban :lines] []))))

    :else
    (do
      (swap! ctx update-homography)
      (let [context @ctx
            homography (-> context :view :homography)
            size (-> context :goban :size)
            d (dec size)
            [topleft topright bottomright bottomleft] (target-points size)]

        (when homography
          (util/with-release
            [ref (MatOfPoint2f.)
             pts (MatOfPoint2f.)]
            (util/vec->mat
              pts
              (mapcat
                (fn [t]
                  [(util/point-along-line [topleft topright] (/ t (dec size)))
                   (util/point-along-line [bottomleft bottomright] (/ t (dec size)))
                   (util/point-along-line [topleft bottomleft] (/ t (dec size)))
                   (util/point-along-line [topright bottomright] (/ t (dec size)))])
                (range 0 size)))
            (Core/perspectiveTransform pts ref (.inv (-> @ctx :view :homography)))

            (swap! ctx assoc-in [:goban :lines] (partition 2 (util/mat->seq ref)))
            (update-reference ctx)))))))

;; Step 5. Read board state.
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

(defn ref-size [size]
  (Size. (* block-size (inc size)) (* block-size (inc size))))

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

(defn read-board [ctx]
  (let [{{:keys [homography samplepoints]} :view
         {:keys [raw flattened flattened-pimage]} :camera
         {:keys [size]} :goban} ctx
        new-flat (or flattened (Mat.))]

    (cond
      (not homography) ctx

      :else
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
            (assoc :board board)))))))

(defn read-stones [ctx]
  (let [{:keys [samplepoints homography]} (:view @ctx)]
    (when homography
      ;; TODO: this multiple swap thing, no good.
      (swap! ctx read-board)

      (util/with-release [src (MatOfPoint2f.) dst (MatOfPoint2f.)]
        (Core/perspectiveTransform
          (util/vec->mat src
            (apply concat samplepoints))
          dst
          (.inv homography))

        (swap! ctx assoc-in [:goban :camerapoints] (util/mat->seq dst))))))


;; Step 6. Repeat
(defn read-loop [ctx camidx]
  (when-not (-> @ctx :camera :stopped)
    (let [video (setup-camera ctx camidx)]
      (doto
        (Thread.
          ^Runnable
          #(when-not (-> @ctx :camera :stopped)
             (camera-read ctx video)
             (recur)))
        (.setDaemon true)
        (.start)))))

(defn stop-read-loop [ctx]
  (if-let [video ^VideoCapture (-> @ctx :camera :video)]
    (.release video))
  (swap! ctx update :camera assoc :stopped true :video nil))

(defn switch-read-loop [ctx camidx]
  (stop-read-loop ctx)
  (Thread/sleep (* 2 (or (-> @ctx :camera :read-delay) 1000)))
  (swap! ctx assoc-in [:camera :stopped] false)
  (read-loop ctx camidx))


(defn camera-updated [wk ctx old new]
  (try
    (read-stones ctx)
    (catch Exception e (.printStackTrace e))))

(defn update-corners [ctx points]
  (swap! ctx update :goban
    (fn [goban]
      (assoc
        goban
        :points points
        :edges (util/update-edges points))))
  (reverse-transform ctx))


(defn reset-board [ctx]
  (swap! ctx assoc :goban
    {:points []
     :size   19}))

(defn start-calibration [ctx]
  (when-not (:goban @ctx)
    (reset-board ctx))
  (util/add-watch-path ctx :goban-camera [:camera] #'camera-updated))

(defn stop-calibration [ctx]
  (remove-watch ctx :goban-camera))

(defn camera-image [ctx]
  (get-in @ctx [:camera :pimg :bufimg]))

(defn set-board-size [ctx size]
  (swap! ctx assoc-in [:goban :size] size)
  (reverse-transform ctx))

(defn cycle-size [ctx]
  (swap! ctx update-in [:goban :size]
    (fn [s]
      (case s 19 9 9 13 19)))
  (reverse-transform ctx))

(defn camera-size [ctx]
  (let [^BufferedImage c (camera-image ctx)]
    (cond
      (not c) nil
      :else [(.getWidth c) (.getHeight c)])))

(defn cycle-corners [ctx]
  (update-corners ctx (vec (take 4 (drop 1 (cycle (-> @ctx :goban :points)))))))

(defn select-camera [ctx camera-idx]
  (sim/stop)
  (stop-read-loop ctx)
  (update-corners ctx [])
  (case camera-idx
    -2 nil
    -1 (sim/start-simulation ctx)
    (switch-read-loop ctx camera-idx)))




;; Older testing code...

;; This was a good idea, but need to retrain the network to recognize these, so not
;; entirely sure it's worthwhile? Would have to measure.
(defn illuminate-correct [m]
  (util/with-release [lab-image (Mat.) equalized (Mat.)]
    (let [planes (LinkedList.)]
      (Imgproc/cvtColor m lab-image Imgproc/COLOR_BGR2Lab)
      (Core/split lab-image planes)
      (Imgproc/equalizeHist (first planes) equalized)
      (.copyTo equalized (first planes))
      (Core/merge planes lab-image)
      (Imgproc/cvtColor lab-image m Imgproc/COLOR_Lab2BGR)
      m)))


;; This is likely used to prep training data?
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

;; Again - older code, not sure what it was doing, think it was pre-neural net days
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

