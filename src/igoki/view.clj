(ns igoki.view
  (:require [igoki.ui :as ui]
            [igoki.util :as util]
            [quil.core :as q])
  (:import (org.opencv.core MatOfPoint2f Point Mat Rect Size Core Scalar CvType MatOfDouble TermCriteria MatOfInt MatOfFloat MatOfFloat6)
           (org.opencv.calib3d Calib3d)
           (org.opencv.imgproc Imgproc)
           (processing.core PImage)
           (java.util LinkedList)))

(def block-size 35.0)

(defn target-points [size]
  (let [extent (* block-size size)]
    [[block-size block-size] [extent block-size] [extent extent] [block-size extent]]))

(defn ref-size [size]
  (Size. (* block-size (inc size)) (* block-size (inc size))))

(defn sample-coords [x y [szx szy :as samplesize]]
  [(max 0 (- x szx)) (max 0 (- y szy)) (* 2 szx) (* 2 szy)])

(defn mat-rect [^Mat mat x y w h]
  (let [[sx sy sw sh] (sample-coords x y [w h])]
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




;; Given circles and samplepoints, construct a possible board view.


(defn read-board [ctx]
  (util/with-release
    [bilat (Mat.)]
    (let [{{:keys [homography shift samplesize samplepoints refmean refstddev reference]} :view
           {:keys [raw flattened]}                                                        :camera
           {:keys [size]}                                                                 :goban} ctx
          [sx sy] shift
          conv-flat (Mat.)
          new-flat (Mat.)]
      (when homography
        (Imgproc/cvtColor raw new-flat Imgproc/COLOR_BGR2HLS_FULL)
        (Imgproc/warpPerspective new-flat new-flat homography (ref-size size))
        #_(Imgproc/erode new-flat new-flat (Imgproc/getStructuringElement Imgproc/MORPH_RECT (Size. 5 5)))
        #_(Imgproc/bilateralFilter new-flat bilat 2 (double 10) (double 10))
        #_(Imgproc/cvtColor new-flat new-flat Imgproc/COLOR_BGR2HLS_FULL)
        #_(.convertTo new-flat new-flat CvType/CV_8UC3 1.0)

        #_(println "==================")
        (->
          ctx
          (assoc-in [:camera :flattened] new-flat)
          (assoc
            :board
            (vec
              (map
                (comp
                  vec
                  (fn [row refrow]
                    (map
                      (fn [[px py] reference-cluster]
                        nil
                        (let [[rh rl rs] reference-cluster
                              [h l s] (mean-at new-flat px py samplesize)]

                          (cond
                            (or
                              (< l (- rl (/ rl 3)))
                              #_(and (< l rl) (< s) (< s (/ rs 2)))) :b
                            (or
                              (> l (+ rl (/ (- 255 rl) 2)))
                              (and (> rl 180) (> s (+ rs 50)))) :w)))
                      row refrow)))
                samplepoints
                reference))))))))



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
      (Imgproc/cvtColor raw ref Imgproc/COLOR_BGR2HLS_FULL)
      (Imgproc/warpPerspective ref ref homography (ref-size size))

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

  (let [{{:keys [homography shift samplesize samplepoints refmean refstddev
                 sample-img cluster-img cluster-img-2 reference ]} :view
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
              [szx szy] samplesize]
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

          (when flattened
            #_(doseq [x (range (- (q/mouse-x) szx) (+ (q/mouse-x) szx))
                    y (range (- (q/mouse-y) szy) (+ (q/mouse-y) szy))]
              (when-let [hsv (seq (.get flattened x y))]
                (let [[h s v] hsv]
                  (q/stroke 255 0 0 64)
                  (q/ellipse (+ tx s 35) (+ 200 v) 1 1)
                  (q/stroke 0 0 255 64)
                  (q/ellipse (+ tx h 35) (+ 200 v) 1 1)
                  (q/stroke 0 255 0 64)
                  (q/ellipse (+ tx h 35) (+ 200 s) 1 1)
                  )))

            #_(ui/shadow-text (str "Ref HSV Mean: " (vec (map int refmean))) (+ tx 35) 525)
            #_(ui/shadow-text (str "Ref HSV Stddev: " (vec (map int refstddev))) (+ tx 35) 545)
            (q/image-mode :corner)
            (q/image (util/mat-to-pimage flattened) 0 0)
            (let [planes (LinkedList.)]
              (Core/split flattened planes)
              (q/image (util/mat-to-pimage (first planes)) 0 700 350 350)
              (q/image (util/mat-to-pimage (second planes)) 350 700 350 350)
              (q/image (util/mat-to-pimage (first (drop 2 planes))) 700 700 350 350))
            (when
              (and (< 10 (q/mouse-x) (- (.cols flattened) 10))
                   (< 10 (q/mouse-y) (- (.rows flattened) 10)))
              (q/stroke-weight 5)
              (let [[samplex sampley] (closest-samplepoint samplepoints [(q/mouse-x) (q/mouse-y)])
                    [px py sw sh] (sample-coords samplex sampley samplesize)
                    cluster (cluster-at flattened samplex sampley samplesize)
                    mean (mean-at flattened samplex sampley samplesize)
                    [c1 c2 c3] (:clusters cluster)]
                #_(println (seq (.-val a)))
                (ui/shadow-text (str "Cluster 1: " (vec c1)) (+ tx 35) 485)
                (ui/shadow-text (str "Cluster 2: " (vec c2)) (+ tx 35) 510)
                (ui/shadow-text (str "Mean: " (vec (map int mean))) (+ tx 35) 535)

                (ui/shadow-text (str "Variance: " (vec (map (fn [a b c] (+ (Math/abs (- a b)))) c1 c2 c3))) (+ tx 35) 560)
                #_(comment
                  ;; Show HSV mean sample
                  (q/stroke 255 0 0)
                  (q/ellipse (+ tx s 35) (+ 200 v) 2 2)
                  (q/stroke 0 0 255)
                  (q/ellipse (+ tx h 35) (+ 200 v) 2 2))
                #_(q/stroke 0 255 0)
                #_(q/ellipse (+ tx (int (* 200 (/ x (first samplesize)))) 35)
                           (+ 200 (int (* 200 (/ y (second samplesize))))) 2 2)

                (q/stroke-weight 1)
                (doseq [r (range (count (:signature cluster)))]
                  (let [l (get (:signature cluster) r)
                        [h s v :as c] (get (:clusters cluster) l)]
                    (q/stroke v s h)
                    (q/fill v s h)
                    (q/rect (+ 35 tx (* 4 (mod r sw))) (+ 200 (* 4 (int (/ r sw)))) 3 3)))


                (q/stroke-weight 1)
                (q/stroke 0 0 255 255)
                (q/fill 0 0 255 128)
                (q/rect px py sw sh)
                ))
            #_(Point. (q/mouse-x) (q/mouse-y))
            (q/stroke-weight 1)

            (q/stroke 0 255 0)
            (q/fill 0 0 0 0)


            ;; Draw overlay
            (doseq [[y row] (map-indexed vector samplepoints)
                    [x [px py]] (map-indexed vector row)]
              (let [ref (get-in reference [y x])
                    [w c] (if (#{[0 0] [0 (dec size)] [(dec size) 0] [(dec size) (dec size)]} [x y])
                            [3 [255 0 0]]
                            [1 [0 255 0]])
                    v (get-in board [y x])
                    [sx sy sw sh] (sample-coords px py samplesize)
                    ]
                (q/fill 0 0)
                (q/stroke 0 0)
                (q/stroke-weight 1)
                #_(doseq [r (range (count (:signature ref)))]
                  (let [l (get (:signature ref) r)]=
                    (q/fill (if (zero? l) 255 0) 255)
                    (q/rect (+ px (mod r szx)) (+ py (int (/ r szx)))
                            1 1)))

                #_(let [sref (cluster-at flattened px py samplesize)]
                  (doseq [r (range (count (:signature sref)))]
                    (let [l (get (:signature sref) r)]
                      (q/fill (if (zero? l) 255 0) 255)
                      (q/rect (+ px (mod r szx)) (+ szy (+ py (int (/ r szx))))
                              1 1))))

                (apply q/stroke c)
                (q/stroke-weight w)
                (q/fill 0 0)
                (q/rect sx sy sw sh)

                (when-not (nil? v)
                  (q/fill (if (= v :w) 255 0) 255)
                  (q/stroke (if (= v :w) 0 255) 255)
                  (q/stroke-weight 1)
                  (q/ellipse (+ sx (/ sw 2)) (+ sy (/ sh 2)) 12 12)))))



          (q/fill 128)
          (ui/shadow-text "Play around corners + center for perspective correction" tx 25)
          (ui/shadow-text "Use mouse on corners to shift point sampling" tx 50)
          (ui/shadow-text "<Backspace> Back to calibration" tx 100)
          (ui/shadow-text "<R> Reset reference" tx 125)
          (ui/shadow-text "<K> Kifu Recording" tx 150)
          )))))

(defmethod ui/mouse-dragged :view [ctx]
  (let [[szx szy] (-> @ctx :view :samplesize)
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