(ns igoki.view
  (:require [igoki.ui :as ui]
            [igoki.util :as util]
            [quil.core :as q])
  (:import (org.opencv.core MatOfPoint2f Point Mat Rect Size Core Scalar CvType MatOfDouble)
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
  (let [[x y] [(max 1 (min (- x (/ sizex 2)) (- (.rows mat) sizex 2)))
               (max 1 (min (- y (/ sizey 2)) (- (.rows mat) sizey 2)))]
        p (Point. x y)
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
      leftedge rightedge)))


(defn read-board [ctx]
  (let [{{:keys [homography shift samplesize samplepoints refmean refstddev reference]} :view
         {:keys [raw]} :camera
         {:keys [size]} :goban} @ctx
        [sx sy] shift
        [szx szy] samplesize
        flattened (Mat.)]

    (Imgproc/warpPerspective raw flattened homography (ref-size size) )
    (Imgproc/cvtColor flattened flattened Imgproc/COLOR_BGR2HSV)
    #_(println (util/write-mat imgmean) " ::: " (util/write-mat imgstddev))

    #_(Imgproc/equalizeHist flattened flattened)
    (swap! ctx assoc-in [:camera :flattened] flattened)
    #_(Core/absdiff ^Mat flattened ^Mat reference flattened)
    (let [[hm sm vm] refmean
          [hs ss vs] refstddev]
      (vec
        (map
          (comp
            vec
            (fn [row refrow]
              (map
                (fn [[px py] [rh rs rv]]
                  (let [[h s v] (mean-at flattened px py samplesize)]
                    (cond
                      (and
                        (not (< (- rh 20) h (+ rh 20)))
                        (< v (- rv vs))
                        (< s (+ rs ss))) :b

                      (and
                        (not (< (- rh 20) h (+ rh 30)))
                        (> v (+ rv vs))
                        (< s rs)) :w
                      )))
                row refrow)))
          samplepoints
          reference)))))



(defn camera-updated [wk ctx _ {:keys [raw]}]
  #_(println "Camera updated")
  (swap! ctx assoc :board (read-board ctx)))


(defn gather-reference [context homography]
  (util/with-release
    [imgmean (MatOfDouble.)
     imgstddev (MatOfDouble.)]
    (let [{{:keys [size]}             :goban
           {:keys [raw]}              :camera
           {:keys [shift samplesize]} :view} context
          samplecorners (target-points size)
          ref (Mat.)
          samplepoints (sample-points samplecorners size)]
      (Imgproc/warpPerspective raw ref homography (ref-size size))
      (Imgproc/cvtColor ref ref Imgproc/COLOR_BGR2HSV)
      (Core/meanStdDev ref imgmean imgstddev)
      {:homography    homography
       :shift         [0 0]
       :samplesize    [14 14]
       :samplecorners samplecorners
       :samplepoints  samplepoints
       :refmean       (seq (.toArray imgmean))
       :refstddev     (seq (.toArray imgstddev))
       :reference     (map (fn [row] (map (fn [[x y]] (mean-at ref x y samplesize)) row)) samplepoints)})))

(defn update-reference [ctx & [force]]
  (util/with-release
    [target (MatOfPoint2f.)
     origpoints (MatOfPoint2f.)]
    (let [{{:keys [points size]} :goban :as context} @ctx
          target (util/vec->mat target (target-points size))
          origpoints (util/vec->mat origpoints points)
          homography (Calib3d/findHomography origpoints target Calib3d/FM_RANSAC 3)]
      (when homography
        (if (or force (not (:view context)))
          (swap! ctx assoc :view (gather-reference context homography))
          (swap! ctx assoc-in [:view :homography] homography))))))

(defmethod ui/construct :view [ctx]
  (util/add-watch-path ctx :view [:camera :raw] camera-updated)
  (update-reference ctx))

(defmethod ui/destruct :view [ctx]
  (remove-watch ctx :view))

(defmethod ui/draw :view [ctx]
  (q/fill 128 64 78)
  (q/rect 0 0 (q/width) (q/height))

  (let [{{:keys [homography shift samplesize samplepoints refmean refstddev]} :view
         {:keys [raw flattened]} :camera
         {:keys [size]} :goban
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
            #_(doseq [x (range (- (q/mouse-x) 7) (+ (q/mouse-x) 7))
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

            (ui/shadow-text (str "Ref HSV Mean: " (vec (take 3 (map int refmean)))) (+ tx 35) 505)
            (ui/shadow-text (str "Ref HSV Stddev: " (vec (take 3 (map int refstddev)))) (+ tx 35) 530)
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