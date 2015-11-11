(ns igoki.view
  (:require [igoki.ui :as ui]
            [igoki.util :as util]
            [quil.core :as q])
  (:import (org.opencv.core MatOfPoint2f Point Mat Rect Size Core Scalar CvType)
           (org.opencv.calib3d Calib3d)
           (org.opencv.imgproc Imgproc)
           (processing.core PImage)))

(def block-size 35.0)

(defn target-points [size]
  (let [extent (* block-size size)]
    [[block-size block-size] [extent block-size] [extent extent] [block-size extent]]))

(defn ref-size [size]
  (Size. (* block-size (inc size)) (* block-size (inc size))))

(defn mean-at [^Mat mat x y [sizex sizey]]
  (let [p (Point. (- x (/ sizex 2)) (- y (/ y sizey 2)))
        roi (Rect. p (Size. sizex sizey))
        m (Mat. mat roi)
        a (Core/mean m)]
    (seq (.-val a))))

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
      leftedge rightedge)
    )
  )


(defn read-board [ctx]
  (let [{{:keys [homography shift samplesize samplepoints]} :view {:keys [raw]} :camera {:keys [size]} :goban} @ctx
        [sx sy] shift
        [szx szy] samplesize
        flattened (Mat/zeros (ref-size size) CvType/CV_8UC3)]

    (Imgproc/warpPerspective raw flattened homography (ref-size size) )
    (Imgproc/cvtColor flattened flattened Imgproc/COLOR_BGR2HSV)

    (swap! ctx assoc-in [:view :flattened] flattened)
    #_(Core/absdiff ^Mat flattened ^Mat reference flattened)
    (->>
      (partition
        size
        (for [row samplepoints [px py] row]
          (let [[h s v :as d] (mean-at flattened px py samplesize)]
            (cond
              (< v 80) :b
              (and (> v 180) (< s 35)) :w)
            #_(if (> (apply max d) 50)
                (cond
                  (> w1 b) :w
                  :else :b)))))
      (map vec)
      vec)))



(defn camera-updated [wk ctx _ {:keys [raw]}]
  #_(println "Camera updated")
  (swap! ctx assoc :board (read-board ctx)))


(defn update-reference [ctx & [force]]
  (let [{{:keys [points size] :as goban} :goban
         {:keys [raw]} :camera
         {:keys [reference shift samplesize samplecorners]} :view} @ctx
        target (util/vec->mat (MatOfPoint2f.) (target-points size))
        origpoints (util/vec->mat (MatOfPoint2f.) points)
        homography (Calib3d/findHomography origpoints target Calib3d/FM_RANSAC 3)
        ref (Mat.)
        samplecorners (if force (target-points size) (or samplecorners (target-points size)))]
    (when homography
      (Imgproc/warpPerspective raw ref homography (ref-size size)))
    (swap! ctx assoc :view
           {:homography homography
            :shift      (if force [0 0] (or shift [0 0]))
            :samplesize (if force [14 14] (or samplesize [14 14]))
            :samplecorners samplecorners
            :samplepoints (sample-points samplecorners size)
            :reference  (if force ref (or reference ref))})))

(defmethod ui/construct :view [ctx]
  (util/add-watch-path ctx :view [:camera] camera-updated)
  (update-reference ctx))

(defmethod ui/destruct :view [ctx]
  (remove-watch ctx :view))

(defmethod ui/draw :view [ctx]
  (q/fill 128 64 78)
  (q/rect 0 0 (q/width) (q/height))

  (let [{{:keys [homography shift samplesize samplepoints flattened]} :view {:keys [raw]} :camera {:keys [size]} :goban
         board :board}  @ctx
        tx (+ (* size block-size) 40)]
    (cond
      (nil? homography)
      (ui/shadow-text "Could not locate board? <Backspace> to go back" 10 25)
      (nil? raw)
      (ui/shadow-text "No source image" 10 25)
      :else
      (do
        (let [[sx sy] shift
              [szx szy] samplesize
              drawn (if flattened (.clone ^Mat flattened))]
          #_(Imgproc/warpPerspective raw flattened homography (ref-size size))
          #_(Core/absdiff ^Mat flattened ^Mat reference flattened)

          (q/fill 255)
          (q/stroke 255)
          #_(q/rect tx 200 255 255)

          (q/fill 255 255 255 255)
          (q/stroke 255 0 0 128)
          (q/stroke-weight 10)
          (q/rect (+ tx 35) 200 255 255)

          (q/stroke-weight 1)
          (doseq [x (range 1 3)]
            (q/line (+ tx 35) (+ 200 (* x 85)) (+ tx 290) (+ 200 (* x 85))))

          (when drawn
            (doseq [x (range (- (q/mouse-x) 7) (+ (q/mouse-x) 7))
                    y (range (- (q/mouse-y) 7) (+ (q/mouse-y) 7))]
              (when-let [hsv (seq (.get flattened x y))]
                (let [[h s v] hsv]
                  (q/stroke 255 0 0 64)
                  (q/ellipse (+ tx s 35) (+ 200 v) 1 1)
                  (q/stroke 0 0 255 64)
                  (q/ellipse (+ tx h 35) (+ 200 v) 1 1)
                  (q/stroke 0 255 0 64)
                  (q/ellipse (+ tx h 35) (+ 200 s) 1 1)
                  )))

            (when
              (and (< 10 (q/mouse-x) (- (.cols flattened) 10))
                   (< 10 (q/mouse-y) (- (.rows flattened) 10)))
              (q/stroke-weight 5)
              (let [[h s v :as d] (mean-at flattened (q/mouse-x) (q/mouse-y) samplesize)]
                #_(println (seq (.-val a)))
                (ui/shadow-text (str "HSV Mean: " (vec (take 3 (map int d)))) (+ tx 35) 485)
                (q/stroke 255 0 0)
                (q/ellipse (+ tx s 35) (+ 200 v) 2 2)
                (q/stroke 0 0 255)
                (q/ellipse (+ tx h 35) (+ 200 v) 2 2)
                (q/stroke 0 255 0)
                (q/ellipse (+ tx h 35) (+ 200 s) 2 2)
                ))
            #_(Point. (q/mouse-x) (q/mouse-y))
            (q/stroke-weight 1)

            (q/stroke 0 255 0)
            (q/fill 0 0 0 0)

            (doseq [[y row] (map-indexed vector samplepoints)
                    [x [px py]] (map-indexed vector row)]
              (let [[w c] (if (#{[0 0] [0 (dec size)] [(dec size) 0] [(dec size) (dec size)]} [x y])
                            [2 (Scalar. 0 0 255)]
                            [1 (Scalar. 0 255 0)])
                    v (get-in board [y x])
                    p (Point. (- px (/ szx 2))
                              (- py (/ szy 2)))]
                (Core/rectangle drawn p (Point. (+ (.-x p) szx) (+ (.-y p) szy)) c w)
                (when-not (nil? v)
                  (Core/putText
                    drawn
                    (if (= v :w)
                      "W" #_(str (vec (map int (.-val a))))
                      "B" #_(str (vec (map int (.-val a)))))
                    (Point. (.-x p) (+ (.-y p) 14)) Core/FONT_HERSHEY_COMPLEX 0.5 (Scalar. 0 255 0) 1.5))))

            (q/image (util/mat-to-pimage drawn) 0 0))
          (q/stroke 0 255 0)
          (q/rect (- (q/mouse-x) (/ szx 2)) (- (q/mouse-y) (/ szy 2)) szx szy)
          (ui/shadow-text "Play around corners + center for perspective correction" tx 25)
          (ui/shadow-text "Use mouse on corners to shift point sampling" tx 50)
          (ui/shadow-text "<Backspace> Back to calibration" tx 100)
          (ui/shadow-text "<R> Reset reference" tx 125)
          (ui/shadow-text "<K> Kifu Recording" tx 150)
          )))))

(defmethod ui/mouse-dragged :view [ctx]
  (let [px (q/mouse-x)
        py (q/mouse-y)
        size (-> @ctx :goban :size)]
    (swap!
      ctx update-in [:view]
      (fn [view p]
        (let [corners (util/update-closest-point (:samplecorners view) p)
              points (sample-points corners size)]
          (assoc view :samplecorners corners :samplepoints points)))
      [px py])))

(defmethod ui/key-pressed :view [ctx]
  (case
    (q/key-code)
    8 (ui/transition ctx :goban)
    82 (update-reference ctx true)
    38 (swap! ctx update-in [:view :shift 1] dec)
    40 (swap! ctx update-in [:view :shift 1] inc)
    37 (swap! ctx update-in [:view :shift 0] dec)
    39 (swap! ctx update-in [:view :shift 0] inc)
    61 (swap! ctx update-in [:view :samplesize] (partial map inc))
    45 (swap! ctx update-in [:view :samplesize] (partial map dec))
    75 (ui/transition ctx :kifu)
    (println "Key code not handled: " (q/key-code))))