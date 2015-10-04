(ns igoki.view
  (:require [igoki.ui :as ui]
            [igoki.util :as util]
            [quil.core :as q])
  (:import (org.opencv.core MatOfPoint2f Point Mat Rect Size Core Scalar)
           (org.opencv.calib3d Calib3d)
           (org.opencv.imgproc Imgproc)))

;; TODO: Allow grid corners to be stretched with mouse instead of just shifted - better for perspective handling.

(def block-size 35.0)

(defn target-points [size]
  (let [extent (* block-size size)]
    [[block-size block-size] [extent block-size] [extent extent] [block-size extent]]))

(defn read-board [ctx]
  (let [{{:keys [homography shift reference]} :view {:keys [raw]} :camera {:keys [size]} :goban} @ctx
        [sx sy] shift
        flattened (Mat.)]

    (Imgproc/warpPerspective raw flattened homography (.size raw))
    (Core/absdiff ^Mat flattened ^Mat reference flattened)
    (partition
      size
      (for [y (range size) x (range size)]
        (let [p (Point. (+ (* block-size (inc x)) sx) (+ (* block-size (inc y)) sy))
              roi (Rect. p (Size. 14 14))
              m (Mat. flattened roi)
              a (Core/mean m)
              [w1 w2 b :as d] (seq (.-val a))]
          (if (> (apply max d) 50)
            (cond
              (> w1 b) :w
              :else :b)))))))



(defn camera-updated [wk ctx _ {:keys [raw]}]
  (println "Camera updated")
  (swap! ctx assoc :board (read-board ctx)))

(defn update-reference [ctx & [force]]
  (let [{{:keys [points size] :as goban} :goban
         {:keys [raw]} :camera
         {:keys [reference shift]} :view} @ctx
        target (util/vec->mat (MatOfPoint2f.) (target-points size))
        origpoints (util/vec->mat (MatOfPoint2f.) points)
        homography (Calib3d/findHomography origpoints target Calib3d/FM_RANSAC 3)
        ref (Mat.)]
    (when homography
      (Imgproc/warpPerspective raw ref homography (.size raw)))
    (swap! ctx assoc :view
           {:homography homography
            :shift      (if force [-7 -7] (or shift [-7 -7]))
            :reference  (if force ref (or reference ref))})))

(defmethod ui/construct :view [ctx]
  (util/add-watch-path ctx :view [:camera] camera-updated)
  (update-reference ctx))

(defmethod ui/destruct :view [ctx]
  (remove-watch ctx :view))

(defmethod ui/draw :view [ctx]
  (q/fill 128 64 78)
  (q/rect 0 0 (q/width) (q/height))

  (let [{{:keys [homography shift reference]} :view {:keys [raw]} :camera {:keys [size]} :goban
         board :board}  @ctx
        tx (+ (* size block-size) 20)
        ]
    (cond
      (nil? homography)
      (ui/shadow-text "Could not locate board? <Backspace> to go back" 10 25)
      (nil? raw)
      (ui/shadow-text "No source image" 10 25)
      :else
      (do
        (let [[sx sy] shift
              flattened (Mat.)]
          (Imgproc/warpPerspective raw flattened homography (.size raw))
          (Core/absdiff ^Mat flattened ^Mat reference flattened)

          (doseq [[y row] (map-indexed vector board)
                  [x v] (map-indexed vector row)]
            (let [p (Point. (+ (* block-size (inc x)) sx) (+ (* block-size (inc y)) sy))]
              (Core/rectangle flattened p (Point. (+ (.-x p) 14) (+ (.-y p) 14)) (Scalar. 0 255 0) 1)
              (when-not (nil? v)
                (Core/putText
                  flattened
                  (if (= v :w)
                    "W" #_(str (vec (map int (.-val a))))
                    "B" #_(str (vec (map int (.-val a)))))
                  (Point. (.-x p) (+ (.-y p) 14)) Core/FONT_HERSHEY_COMPLEX 0.5 (Scalar. 0 0 255) 1.5))))

          (q/image (util/mat-to-pimage flattened) 0 0)
          (ui/shadow-text "Play around corners + center for perspective correction" tx 25)
          (ui/shadow-text "Arrows to shift point sampling" tx 50)
          (ui/shadow-text "<Backspace> Back to calibration" tx 100)
          (ui/shadow-text "<R> Reset reference" tx 125)
          (ui/shadow-text "<K> Kifu Recording" tx 150)
          )))))

(defmethod ui/key-pressed :view [ctx]
  (case
    (q/key-code)
    8 (ui/transition ctx :goban)
    82 (update-reference ctx true)
    38 (swap! ctx update-in [:view :shift 1] dec)
    40 (swap! ctx update-in [:view :shift 1] inc)
    37 (swap! ctx update-in [:view :shift 0] dec)
    39 (swap! ctx update-in [:view :shift 0] inc)
    75 (ui/transition ctx :kifu)
    (println "Key code not handled: " (q/key-code))))