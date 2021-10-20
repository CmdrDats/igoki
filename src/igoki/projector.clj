(ns igoki.projector
  (:require
    [igoki.util :as util]
    [igoki.game :as game]
    [igoki.sgf :as sgf]
    [igoki.litequil :as lq]
    [igoki.camera :as camera])
  (:import
    (org.opencv.calib3d Calib3d)
    (org.opencv.imgproc Imgproc)
    (org.opencv.core Mat Size MatOfPoint2f TermCriteria Core Point Scalar CvType)))

(defonce proj-ctx
  (atom nil))

(defn update-projmat [ctx]
  (let [{:keys [homography board-homography sketch] :as projcontext} @proj-ctx
        {:keys [camera proj-img kifu board goban] :as context} @ctx
        {:keys [kifu-board]} kifu
        existing-corners (:corners projcontext)
        size (Size. 9 7)
        lastmove (game/find-last-move ctx)]
    (when board-homography
      (util/with-release
        [target (MatOfPoint2f.)
         projmat (Mat. (Size. (* (inc (:size goban)) camera/block-size)
                         (* (inc (:size goban)) camera/block-size)) CvType/CV_8UC3)
         newflat (Mat.)]
        (let [[[x1 y1] [x2 y2] [x3 y3] [x4 y4] :as corner-points]
              (camera/target-points (:size goban))

              sample-points (camera/sample-points corner-points (:size goban))
              target (util/vec->mat target (apply concat sample-points))]
          (Core/perspectiveTransform target target (.inv board-homography))
          (Imgproc/rectangle projmat
            (Point. 0 0)
            (Point. (+ x3 camera/block-size) (+ y3 camera/block-size))
            (Scalar. 0 0 0) -1)

          ;; Highlight differences between constructed and camera board (visual syncing)
          (when (and board kifu-board)
            (doseq [[x y o c]
                    (game/board-diff kifu-board board)]
              (let [[px py] (nth (nth sample-points y) x)]
                (Imgproc/circle projmat (Point. px py) 5 (Scalar. 0 0 255)
                  (if (= o :b) 1 -1)))))

          ;; Highlight last move
          (let [{:keys [black white]} lastmove
                m (or black white)]
            (doseq [coord m]
              (let [[x y] (sgf/convert-sgf-coord coord)
                    [px py] (nth (nth sample-points y) x)]
                (Imgproc/circle projmat (Point. px py) 5 (Scalar. 0 255 0)
                  (if black 1 -1)))))

          (when (:show-branches kifu)
            (doseq
              [[idx {:keys [black white]}] (map-indexed vector (:branches lastmove))
               m (or black white)]
              (let [[x y :as p] (sgf/convert-sgf-coord m)
                    [px py] (nth (nth sample-points y) x)]
                #_(Core/circle projmat (Point. px py) 5 (Scalar. 255 255 255) -1)
                (Imgproc/putText projmat (str (char (+ 65 idx))) (Point. (- px 5) (+ py 5))
                  Imgproc/FONT_HERSHEY_PLAIN 0.7 (Scalar. 255 255 255) 1.5))))

          ;; Draw entire current board state
          #_(doseq [[y row] (map-indexed vector (:board @ui/ctx))
                    [x cell] (map-indexed vector row)]
             (let [[x y] (nth (nth sample-points y) x)]
               (case cell
                 :b (Core/circle projmat (Point. x y) 5 (Scalar. 255 255 255) 1)
                 :w (Core/circle projmat (Point. x y) 5 (Scalar. 255 255 255) -1)
                 nil)))


          (Imgproc/warpPerspective projmat newflat (.inv board-homography) (Size. (.width sketch) (.height sketch)))
          (swap! proj-ctx assoc :proj-img (util/mat-to-pimage newflat (:bufimg proj-img)))

          #_(doseq [[x y] (util/mat->seq target)]
              (lq/color 0 0 255)
              (lq/stroke-weight 20)
              (lq/point x y)
              (lq/stroke-weight 0))))
      #_(Core/circle (:raw camera) (Point. 540 265) 20 (Scalar. 255 0 0)))))


(defn draw-checkerboard [{:keys [board]}]
  (doseq [[x y w h] board]
    (lq/rect x y w h)))

(defn checker [x y width height xblocks yblocks]
  (let [bw (/ width xblocks)
        bh (/ height yblocks)]
    {:setup {:x x :y y :width width :height height :xblocks xblocks :yblocks yblocks}
     :size (Size. (dec xblocks) (dec yblocks))
     :board
     (for [cellx (range xblocks) celly (range yblocks)
           :when (= (mod (+ cellx (* celly (inc xblocks))) 2) 1)]
       (if (= (mod celly 2) 0)
         [(+ x (* cellx bw)) (+ y (* celly bh)) (dec bw) (dec (+ bh (/ bh 10)))]
         [(+ x (* cellx bw)) (+ y (* celly bh) (/ bh 10)) (dec bw) (dec (- bh (/ bh 10)))]))
     :points
     (for [celly (range 1 yblocks) cellx (range 1 xblocks)]
       (if (= (mod celly 2) 0)
         [(+ x (* cellx bw)) (+ y (* celly bh))]
         [(+ x (* cellx bw)) (+ y (* celly bh) (/ bh 10))]))}))

(defn fix-checker-orientation [corners-mat]
  (let [corners (util/mat->seq corners-mat)
        p1 (nth corners 0)
        p2 (nth corners 9)
        p3 (nth corners 18)]
    (if (> (util/line-length [p1 p2]) (util/line-length [p2 p3]))
      (util/vec->mat corners-mat (reverse corners))
      corners-mat)))

(defn look-for-checkerboard [proj-ctx camera]
  (util/with-release [gray (Mat.)]
    (let [corners (MatOfPoint2f.)
          crit (TermCriteria. (bit-or TermCriteria/EPS TermCriteria/MAX_ITER) 30 0.1)]
      (Imgproc/cvtColor (:raw camera) gray Imgproc/COLOR_BGR2GRAY)
      (let [found
            (Calib3d/findChessboardCorners gray (:size checker) corners
              (+ Calib3d/CALIB_CB_ADAPTIVE_THRESH
                Calib3d/CALIB_CB_NORMALIZE_IMAGE
                Calib3d/CALIB_CB_FAST_CHECK))]
        (.println System/out (str "Checking: " found))
        (when found
          (println "Found corners.")
          (Imgproc/cornerSubPix gray corners (Size. 11 11) (Size. -1 -1) crit)
          (fix-checker-orientation corners)
          (swap! proj-ctx assoc :corners corners))))))

(defn update-homography [proj-ctx existing-corners]
  (util/with-release
    [target (MatOfPoint2f.)]
    (let [target (util/vec->mat target (:points checker))
          homography
          (Calib3d/findHomography ^MatOfPoint2f existing-corners ^MatOfPoint2f target
            Calib3d/FM_RANSAC 3.0)]
      (when homography
        (println "Homography updated")
        (swap! proj-ctx assoc :homography homography)))))

(defn update-board-homography [ctx proj-ctx homography]
  (util/with-release
    [projector-space (MatOfPoint2f.)
     board-space (MatOfPoint2f.)]
    (let [goban (:goban @ctx)
          projector-space (util/vec->mat projector-space (:points goban))
          _ (Core/perspectiveTransform projector-space projector-space homography)
          board-space (util/vec->mat board-space (camera/target-points (:size goban)))
          board-homography
          (Calib3d/findHomography ^MatOfPoint2f projector-space ^MatOfPoint2f board-space
            Calib3d/FM_RANSAC 3.0)]

      (doseq [[x y] (util/mat->seq projector-space)]
        (lq/color 255 0 0)
        (lq/ellipse x y 10 10))

      (when board-homography
        (println "Board Homography updated")
        (swap! proj-ctx assoc :board-homography board-homography)))))

(defn draw [ctx proj-ctx]
  (lq/frame-rate 10)
  (lq/background 0 0 0)
  (lq/rect 0 0 (lq/width) (lq/height))

  (let [[w h] [(lq/width) (lq/height)]
        [gw gh] [(/ w 2) (/ h 2)]
        checker (checker (- gw (/ gw 2)) (- gh (/ gh 2)) gw gh 10 8)

        {:keys [camera goban] :as context} @ctx
        {:keys [homography board-homography proj-img] :as projcontext} @proj-ctx
        existing-corners (:corners projcontext)
        img (:bufimg proj-img)]

    (lq/background 255 255 255)
    (lq/rect 0 0 (lq/width) (lq/height))
    (lq/background 0 0 0)
    (when-not homography
      (draw-checkerboard checker))

    ;; Draw screen intersection points
    #_(do
        (lq/color 255 0 0)
        (doseq [[x y] (:points checker)]
          (lq/ellipse x y 5 5)))

    #_(lq/image (-> @proj-ctx :pattern) (- (/ (q/width) 2) 180) (- (/ (q/height) 2) 200) 300 400)


    (when proj-img
      (lq/image img 0 0 (.getWidth img) (.getHeight img)))

    ;; There's no way this should be happening in the draw call.
    (cond
      (and (not existing-corners) (:raw camera))
      (look-for-checkerboard proj-ctx camera)

      (and existing-corners (not homography))
      (update-homography proj-ctx existing-corners)

      (and homography (= 4 (count (:points goban))) (not board-homography))
      (update-board-homography ctx proj-ctx homography))



    #_(when existing-corners
        (util/with-release [clone (.clone (:raw camera))]
          (Calib3d/drawChessboardCorners clone (:size checker) existing-corners true)
          (swap! ui/ctx update :camera
            assoc :pimg
            (util/mat-to-pimage clone
              (-> context :camera :pimg :bufimg)))))))

(defn reset-ctx []
  (reset! proj-ctx {:sketch (:sketch @proj-ctx)}))


(defn start-cframe [ctx]
  (reset! proj-ctx {})

  (let [sketch
        (lq/sketch
          {:title "Move on board in camera view (place paper on board for contrast)"
           :draw (partial #'draw proj-ctx ctx)
           :size (or (-> @proj-ctx :sketchconfig :size) [1280 720])})]
    (swap! proj-ctx assoc :sketch sketch))


  (doto
    (Thread.
      ^Runnable
      (fn []
        (while (not (:stopped (:sketch @proj-ctx)))
          (try
            (update-projmat ctx)
            (catch Exception e
              (.printStackTrace e)))
          (Thread/sleep 500))))
    (.setDaemon true)
    (.start))
  #_(when (:sketch @proj-ctx)
     (doto (:sketch @proj-ctx)
       #_(.setExtendedState JFrame/MAXIMIZED_BOTH)
       #_(.setUndecorated true))))
