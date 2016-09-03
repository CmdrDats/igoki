(ns igoki.view
  (:require [igoki.ui :as ui]
            [igoki.util :as util]
            [quil.core :as q])
  (:import (org.opencv.core MatOfPoint2f Point Mat Rect Size Core Scalar CvType MatOfDouble TermCriteria MatOfInt MatOfFloat MatOfFloat6)
           (org.opencv.calib3d Calib3d)
           (org.opencv.imgproc Imgproc)
           (processing.core PImage)
           (java.util LinkedList UUID)
           (org.opencv.highgui Highgui)
           (java.io File)
           (org.deeplearning4j.util ModelSerializer)
           (org.deeplearning4j.eval Evaluation)
           (org.canova.image.loader ImageLoader)
           (org.deeplearning4j.nn.multilayer MultiLayerNetwork)
           (org.nd4j.linalg.api.ndarray INDArray)))

;; Moving average
;; 'dirty' flags on positions (set against previous x frames as reference)
;; Single-shot, multi stone .output call.
;;

;; Stone detection, neural network

(defn load-net [nm]
  (ModelSerializer/restoreMultiLayerNetwork (File. (str nm ".cnet"))))
(def net (load-net "resources/supersimple"))
(def loader (ImageLoader. 10 10 3))

(def block-size 10)


(defn eval-net [flat px py]
  (let [smat (.submat flat (Rect. (- px (/ block-size 2)) (- py (/ block-size 2)) 10 10))
        img (util/mat-to-buffered-image smat)
        ^INDArray d (.asRowVector loader img)
        _ (.divi d 255.0)
        ^INDArray o (.output ^MultiLayerNetwork net d)]
    (.release smat)
    (for [i (range 3)]
      (.getFloat o (int i)))))


(defn target-points [size]
  (let [extent (* block-size size)]
    [[block-size block-size] [extent block-size] [extent extent] [block-size extent]]))

(defn ref-size [size]
  (Size. (* block-size (inc size)) (* block-size (inc size))))

(defn sample-coords [x y [szx szy :as samplesize]]
  [(max 0 (- x szx)) (max 0 (- y szy)) (* 2 szx) (* 2 szy)])

(defn mat-rect [^Mat mat x y w h] (let [[sx sy sw sh] (sample-coords x y [w h])]
    (Mat. mat (Rect. (Point. sx sy) (Size. sw sh)))))

(defn read-clusters [labels centers]
  (.convertTo centers centers CvType/CV_8UC1 255.0)
  (.reshape centers 3)

  (util/with-release [labelint (MatOfInt. ^Mat labels)]
    (let [ls (.toArray labelint)
          counts (sort-by second (frequencies ls))
          dominant (ffirst counts)
          labelmap (into {} (map-indexed (fn [i [k _]] [k i]) counts))
          signature (vec (map labelmap ls))]
      {:signature signature
       :counts    counts
       :clusters  (vec (map (fn [[l _]]
                              (doall (map (fn [i] (int (first (.get centers l i)))) (range 3)))) counts))
       :hls       [(int (first (.get centers dominant 0)))
                   (int (first (.get centers dominant 1)))
                   (int (first (.get centers dominant 2)))]})))

(defn cluster-at [mat x y [sizex sizey]]
  (util/with-release
    [samples32f (Mat.)
     labels (Mat.)
     centers (Mat.)
     cutout (.clone (mat-rect mat
                              (max 0 (min x (- (.rows mat) sizex)))
                              (max 0 (min y (- (.rows mat) sizey)))
                              sizex sizey))]
    (let [k 2
          samples (.reshape cutout 1 (* (.cols cutout) (.rows cutout)))
          criteria (TermCriteria. TermCriteria/COUNT 1000 1)]
      (.convertTo samples samples32f CvType/CV_32F (/ 1.0 255.0))
      (Core/kmeans samples32f k labels criteria 10 Core/KMEANS_PP_CENTERS centers)
      (read-clusters labels centers))))


(defn mean-at [^Mat mat x y [sizex sizey]]
  (util/with-release
    [m (mat-rect mat x y sizex sizey)]
    (seq (.-val (Core/mean m)))))

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
            (Highgui/imwrite (str "samples/" (if p (name p) "e") "-" px "-" py "-" id ".png") (.submat ^Mat flat r)))
          ))
      samplepoints)))

;; Given circles and samplepoints, construct a possible board view.
(def reading (atom false))

(defn read-board [ctx]
  (if-not @reading
    (try
      (reset! reading true)
      (let [{{:keys [homography samplepoints flattened]} :view
             {:keys [raw flattened]} :camera
             {:keys [size]} :goban} ctx
            new-flat (or flattened (Mat.))]
        (when homography
          (Imgproc/warpPerspective raw new-flat homography (ref-size size))

          (let [board
                (vec
                  (map
                    (comp
                      vec
                      (fn [row]
                        (map
                          (fn [[px py]]
                            nil
                            (let [[b e w] (eval-net new-flat px py)]
                              (cond
                                (> b 0.5) :b
                                (> w 0.7) :w)))
                          row)))
                    samplepoints))]
            (->
              ctx
              (assoc-in [:camera :flattened] new-flat)
              (assoc-in [:camera :flattened-pimage] (util/mat-to-pimage new-flat))
              (assoc :board board))))
        )
      (finally
        (reset! reading false)))
    ctx))



(defn camera-updated [wk ctx _ {:keys [raw]}]
  #_(println "Camera updated")
  (swap! ctx read-board))


(defn gather-reference [context homography]
  (util/with-release
    [imgmean (MatOfDouble.)
     imgstddev (MatOfDouble.)]
    (let [{{:keys [size]}             :goban
           {:keys [raw]}              :camera
           {:keys [samplecorners shift samplesize reference]} :view} context
          samplesize (or samplesize [7 7])
          samplecorners (or samplecorners (target-points size))
          samplepoints (sample-points samplecorners size)
          ref (Mat.)]
      #_(Imgproc/cvtColor raw ref Imgproc/COLOR_BGR2HLS_FULL)
      (Imgproc/warpPerspective raw ref homography (ref-size size))

      (Core/meanStdDev ref imgmean imgstddev)
      {:homography    homography
       :shift         [0 0]
       :samplesize    samplesize
       :samplecorners samplecorners
       :samplepoints  samplepoints
       :refmean       (seq (.toArray imgmean))
       :refstddev     (seq (.toArray imgstddev))
       :reference
                      (vec (pmap (fn [row]
                                   (vec (map (fn [[x y]] (mean-at ref x y samplesize)) row))) samplepoints))})))

(defn update-reference [ctx & [force]]
  (let [{{:keys [homography]} :view :as context} @ctx]
    (when (and homography (or force (not (-> context :view :reference))))
      (swap! ctx assoc :view (gather-reference context homography)))))

(defn update-homography [ctx]
  (util/with-release
    [target (MatOfPoint2f.)
     origpoints (MatOfPoint2f.)]
    (let [{{:keys [points size]} :goban :as context} ctx
          target (util/vec->mat target (target-points size))
          origpoints (util/vec->mat origpoints points)
          homography (Calib3d/findHomography origpoints target Calib3d/FM_RANSAC 3)]
      (if homography
        (assoc-in ctx [:view :homography] homography)
        ctx))))

(defmethod ui/construct :view [ctx]
  (util/add-watch-path ctx :view [:camera :raw] camera-updated)
  (swap! ctx update-homography)
  (swap! ctx read-board)
  (update-reference ctx))

(defmethod ui/destruct :view [ctx]
  (remove-watch ctx :view))

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

(defmethod ui/draw :view [ctx]
  (q/fill 128 64 78)
  (q/rect 0 0 (q/width) (q/height))

  (let [{{:keys [homography samplesize samplepoints]} :view
         {:keys [raw flattened-pimage]} :camera
         {:keys [size]} :goban
         board :board}  @ctx
        local-block-size 35.0
        local-mult (/ local-block-size block-size)
        tx (+ (* size local-block-size) 40)]
    (cond
      (nil? homography)
      (ui/shadow-text "Could not locate board? <Backspace> to go back" 10 25)
      (nil? raw)
      (ui/shadow-text "No source image" 10 25)
      :else
      (do
        (q/fill 255)
        (q/stroke 255)

        (when-not flattened-pimage
          (ui/shadow-text "No flattened board image, check source?" 25 25))

        (when flattened-pimage
          (q/image-mode :corner)
          (q/image flattened-pimage 0 0 (* local-block-size (inc size)) (* local-block-size (inc size)))

          (q/stroke-weight 1)

          (q/stroke 0 255 0)
          (q/fill 0 0 0 0)


          ;; Draw overlay
          (doseq [[y row] (map-indexed vector samplepoints)
                  [x [px py]] (map-indexed vector row)]
            (let [v (get-in board [y x])
                  [sx sy sw sh] (sample-coords px py samplesize)]
              (q/fill 0 0)
              (q/stroke 0 0)
              (q/stroke-weight 1)

              (apply q/stroke [0 255 0])
              (q/stroke-weight 1)
              (q/fill 0 0)
              #_(q/rect sx sy sw sh)
              (q/rect
                (- (* local-mult px) (/ local-block-size 2)) (- (* local-mult py) (/ local-block-size 2))
                local-block-size local-block-size)

              (when-not (nil? v)
                (q/fill (case v :w 255 :b 0 :na (q/color 255 0 0)) 255)
                (q/stroke (if (= v :w) 0 255) 255)
                (q/stroke-weight 1)
                (q/ellipse (* local-mult (+ sx (/ sw 2))) (* local-mult (+ sy (/ sh 2))) 12 12)))))



        (q/fill 128)
        (ui/shadow-text "Play around corners + center" tx 25)
        (ui/shadow-text "to check sampling is correct" tx 50)
        (ui/shadow-text "<Backspace> Back to calibration" tx 100)
        (ui/shadow-text "<K> Kifu Recording" tx 150)
        ))))

(defmethod ui/mouse-dragged :view [ctx]
  #_(let [[szx szy] (-> @ctx :view :samplesize)
        px (- (q/mouse-x) (/ szx 2))
        py (- (q/mouse-y) (/ szy 2))
        size (-> @ctx :goban :size)]
    (swap!
      ctx update-in [:view]
      (fn [view p]
        (let [corners (util/update-closest-point (:samplecorners view) p)
              points (sample-points corners size)]
          (assoc view :samplecorners corners :samplepoints points)))
      [px py])))

(defmethod ui/mouse-pressed :view [ctx]

  (let [[szx szy] (-> @ctx :view :samplesize)
        local-block-size 35.0
        local-mult (/ local-block-size block-size)
        [px py] (map
                  (comp dec int #(/ % block-size))
                  (closest-samplepoint (-> @ctx :view :samplepoints) [(/ (q/mouse-x) local-mult) (/ (q/mouse-y) local-mult)]))]
    (cond
      (= (q/mouse-button) :left)
      (swap! ctx assoc-in [:board py px] :w)
      (= (q/mouse-button) :right)
      (swap! ctx assoc-in [:board py px] :b)
      :else
      (swap! ctx update-in [:board py px] #(if (nil? %) :na nil))
      )))

(defmethod ui/mouse-released :view [ctx]
  )

(defmethod ui/key-pressed :view [ctx]
  (case
    (q/key-code)
    8 (ui/transition ctx :goban)
    38 (swap! ctx update-in [:view :shift 1] dec)
    40 (swap! ctx update-in [:view :shift 1] inc)
    37 (swap! ctx update-in [:view :shift 0] dec)
    39 (swap! ctx update-in [:view :shift 0] inc)
    61 (swap! ctx update-in [:view :samplesize] (partial map inc))
    45 (swap! ctx update-in [:view :samplesize] (partial map dec))
    75 (ui/transition ctx :kifu)
    (println "Key code not handled: " (q/key-code))))



(defn cluster-demo [#_ctx]
  (let [img (Mat/zeros 200 200 CvType/CV_8UC3)]
    (Core/rectangle img (Point. 0 0) (Point. 200 200) (Scalar. 0 255 0) -1)
    (Core/rectangle img (Point. 100 0) (Point. 180 200) (Scalar. 0 0 255) -1)

    (let [clusters (cluster-at img 0 0 [200 200])
          _ (Core/rectangle img (Point. 99 0) (Point. 150 150) (Scalar. 0 0 255) -1)
          clusters2 (cluster-at img 0 0 [200 200])]
      #_(println clusters)
      #_(swap! ctx assoc-in [:view :sample-img] (util/mat-to-pimage img))
      #_(swap! ctx assoc-in [:view :cluster-img] (util/mat-to-pimage (second (first clusters))))
      #_(swap! ctx assoc-in [:view :cluster-img-2] (util/mat-to-pimage (second (second clusters))))
      #_(println clusters)
      (util/hamming-dist (:signature clusters) (:signature clusters2))
      )))

(comment
  [[nil :b nil nil nil nil nil :b nil nil nil nil nil nil :w :b :b nil nil]
   [:w :w :b :b nil nil :b nil :b :b :b nil nil :w :b nil :b nil nil]
   [nil :b :w :b nil :b nil :b :w :w nil :b :w nil :w :b nil nil nil]
   [nil :w :w :w :b :b :b :w :w nil nil :b :w :w :b :b :b :b nil]
   [nil nil nil nil :w :w :w :w nil :w :b nil :b :w nil :w :w :b nil]
   [nil nil :w :w nil :w nil :b nil :b nil :b :b :w nil nil nil :w nil]
   [nil :w :w :b :b :w :b nil :b nil :b nil :b :b :w :w :w nil nil]
   [nil :b :w :w :w :b :w :w :w :b :w :w :b :b :b :b :w :w nil]
   [nil nil :b nil :b :b :b :b :b :b :w :b :w :b :b :w :b :w nil]
   [nil :b :b nil :b nil nil :w :b :w :w nil :w :w :w :w :b :w nil]
   [:b :w :w :w nil nil :w nil :w :b :b nil :w nil :w :b :b :b nil]
   [nil nil nil nil nil :b :w :w :w :w :w :w nil :b :w :w :w :b nil]
   [nil nil :w :b nil nil nil nil nil nil nil nil :b nil :b :w :b :b nil]
   [nil nil :w :b nil nil :b nil nil nil nil :w nil :b :b :b nil nil nil]
   [nil :w :b nil :b nil nil :b :b nil :b nil :b :b :w :w :b :b nil]
   [nil :w :b :b nil nil nil :w :b nil :b :b :w :b nil :b :b :w nil]
   [nil :w :b nil nil :b nil :b :w :w :b :w :w :w :b :b :w :w nil]
   [nil :w :b :w :w nil nil :b :w :b :w :w nil nil :w :w nil nil nil]
   [nil nil :w :b :b :b nil :b :w nil nil nil nil nil nil nil nil nil nil]]
  )