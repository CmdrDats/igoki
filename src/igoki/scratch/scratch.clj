(ns igoki.scratch.scratch
  (:require
    [igoki.util :as util :refer [-->]])
  (:import
    (org.opencv.objdetect CascadeClassifier)
    (org.opencv.core MatOfRect Core Rect Point Scalar Mat Size MatOfPoint MatOfKeyPoint MatOfPoint2f Point3 TermCriteria MatOfPoint3 CvType MatOfPoint3f)
    (java.awt.image BufferedImage WritableRaster DataBufferByte)
    (java.awt Color Graphics KeyboardFocusManager KeyEventDispatcher Font RenderingHints)
    (java.io File)
    (javax.imageio ImageIO)
    (javax.swing JFrame JPanel)
    (org.opencv.imgproc Imgproc)
    (java.awt.event KeyEvent MouseListener MouseEvent)
    (org.opencv.imgcodecs Imgcodecs)
    (org.opencv.videoio VideoCapture Videoio)))

;; This namespace represents some of the early igoki work, mostly to detect the actual Go Board.
;; it has been deprecated in favour of simply doing manual calibration due to the complexity
;; of dealing with the fickleness of variances in Go boards, lighting conditions, camera quality,
;; etc.
;;
;; It would be neat to have automatic handling, but it's not the core objective of this project to
;; detect Go boards - instead, it's focussed on bridging the gap between the digital and physical
;; game.

;; Step 1 - Define Corners of board
;; Step 2 - Verify coordinates
;; Step 3 - Choose mode: Local Kifu, OGS Kifu

(nu.pattern.OpenCV/loadShared)

(defonce camera (atom nil))



(defonce appstate
         (atom
           {:images      [{} {} {} {} {}]
            :goban-corners [[670 145] [695 900] [1320 855] [1250 220]]
            :input :camera
            :selected    -1
            :frozen      false}))



(defn update-image-mat! [slot image title]
  (swap! appstate #(assoc-in % [:images slot] {:mat image :title title})))

(defn reset-to-index! []
  (swap! appstate assoc :selected -1))

(defn rotate-slot-left! []
  (swap! appstate (fn [i] (update i :selected #(mod (dec %) (count (:images i)))))))

(defn rotate-slot-right! []
  (swap! appstate (fn [i] (update i :selected #(mod (dec %) (count (:images i)))))))

(defn select-frame! [n]
  (swap! appstate (fn [i] (assoc i :selected (mod n (count (:images i)))))))

(defn toggle-freeze! []
  (println "Freeze toggle")
  (swap! appstate update-in [:frozen] not))

(defn save-image [^BufferedImage img]
  (ImageIO/write img "png" (File. "resources/new.png")))

(defn load-image [^String file]
  (ImageIO/read (File. file)))

(defn handle-keypress [^KeyEvent e]
  (println "Key pressed: " (.getKeyCode e) " - Shift: " (.isShiftDown e) )
  (when (= (.getID e) KeyEvent/KEY_PRESSED)
    (case (.getKeyCode e)
      67 (swap! appstate assoc :input :camera)
      82 (swap! appstate assoc :input (Imgcodecs/imread "resources/goboard.png"))
      32 (toggle-freeze!)
      27 (reset-to-index!)
      49 (select-frame! 1)
      50 (select-frame! 2)
      51 (select-frame! 3)
      52 (select-frame! 4)
      53 (select-frame! 5)
      54 (select-frame! 6)
      55 (select-frame! 7)
      56 (select-frame! 8)
      57 (select-frame! 9)
      48 (select-frame! 0)

      10 (swap! appstate assoc :accepted true)
      false)))

(defn draw-title [g title x y]
  (.setColor g (Color/BLACK))
  (.drawString g title (dec x) (dec y))
  (.setColor g (Color/WHITE))
  (.drawString g title x y))

(defn draw-index [^JFrame frame ^Graphics g {:keys [images]}]
  (let [gridsize (Math/ceil (Math/sqrt (count images)))
        gw (/ (.getWidth frame) gridsize)]
    (doseq [[c {:keys [mat title]}] (map-indexed vector images)]
      (if-let [image (if (pos? (.width mat)) (util/mat-to-buffered-image mat nil))]
        (let [ratio (if image (/ (.getHeight image) (.getWidth image)))
              x (* (mod c gridsize) gw)
              y (* (Math/floor (/ c gridsize)) ratio gw)]
          (.drawImage g image (int x) (int y) (int gw) (int (* ratio gw)) nil)
          (draw-title g (str title ", Slot: " c) (int (+ x 5)) (int (+ y 15)))
          )))))

(defn render [^JFrame frame ^Graphics g]
  (let [{:keys [images selected] :as state} @appstate
        {:keys [mat title] :as im} (get images selected)
        image (if (and im (pos? (.getWidth frame))) (util/mat-to-buffered-image mat nil))
        ratio (if image (/ (.getHeight image) (.getWidth image)))]
    (.setRenderingHints g (RenderingHints. RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BICUBIC))
    (if (or (= selected -1) (nil? image))
      (draw-index frame g state)
      (do
        (.drawImage g image 0 0 (.getWidth frame) (* ratio (.getWidth frame)) nil)
        (draw-title g (str title ", Slot: " selected) 5 15)))))

(defn click-mouse [^MouseEvent e]
  )

(defn window [text x y]
  (let [frame (JFrame.)]
    (.add (.getContentPane frame)
          (proxy [JPanel] []
            (paint [^Graphics g]
              (render frame g))
            ))

    (.addMouseListener
      frame
      (proxy [MouseListener] []
        (mousePressed [^MouseEvent e]
          (click-mouse e))))

    (.addKeyEventDispatcher (KeyboardFocusManager/getCurrentKeyboardFocusManager)
                            (proxy [KeyEventDispatcher] []
                              (dispatchKeyEvent [e]
                                (handle-keypress e)
                                false)))
    (doto frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setTitle text)
      (.setResizable true)
      (.setSize 800 600)
      (.setLocation x y)
      (.setVisible true))))

#_(defn highlight-faces [image]
  (let [face-detector (CascadeClassifier. (.getAbsolutePath (clojure.java.io/file "resources/lbpcascade_frontalface.xml")))
        face-detections (MatOfRect.)]
    (.detectMultiScale face-detector image face-detections)
    (doseq [^Rect r (seq (.toArray face-detections))]
      (Core/rectangle image (Point. (.-x r) (.-y r)) (Point. (+ (.-x r) (.-width r)) (+ (.-y r) (.-height r))) (Scalar. 0 255 0)))
    ))

(defn matofpoint-vec
  "Convert MatOfPoint to vector list [[x y] [x y] ...]"
  [mat]
  (for [p (.toList mat)]
    [(.-x p) (.-y p)]))


(def target-homography
  {:9x9
   (doto (MatOfPoint2f.)
     (.fromList (for [x (range 1 3) y (range 1 2)] (Point. (* 70.0 x) (* 70.0 y)))))
   :13x13
   (util/vec->mat (MatOfPoint2f.) [[70 70] [70 980] [980 980] [980 70]])
   :19x19
   (doto (MatOfPoint2f.)
     (.fromList (for [x (range 1 2) y (range 1 2)] (Point. (* 70.0 x) (* 70.0 y)))))})

(def current-transform (atom nil))

(defn count-perimeter [mat]
  (let [m (for [x (range (.rows mat))] (seq (.get mat x 0)))]
    (first
      (reduce
        (fn [[r [ax ay :as a]] [x y :as p]]
          (cond
            (nil? a) [r p]
            :else
            [(+ r (Math/sqrt (+ (* (- ax x) (- ax x)) (* (- ay y) (- ay y))))) p]))
        [0 nil] (concat [(last m)] m)))))

(defn find-goban [gray-img colour]
  #_(let [sorted (:goban-corners @appstate)
        s (map-indexed vector sorted)
        c (--> gray-img (Imgproc/Canny 100 50 3 false))]
    (update-image-mat! 5 c "Contours")
    (reduce
      (fn [[i [ax ay :as a]] [_ [x y] :as p]]
        (cond
          (nil? a) p
          :else
          (do
            (Core/line colour (Point. ax ay) (Point. x y) (Scalar. 0 0 (- 255 (* i 32))) 5)
            p)))
      nil (concat s [(first s)]))
    sorted
    )

  #_(let [contours (ArrayList.) hier (Mat.)
          c (--> gray-img (Imgproc/Canny 100 50 3 false))]
      ;; Find contours
      (Imgproc/findContours c
                            contours hier Imgproc/RETR_TREE Imgproc/CHAIN_APPROX_NONE (Point. 0 0))
      (update-image-mat! 5 c "Contours")

      ;; Find largest area 4-cornered polygon
      (let
        [[sq x _]
         (loop [[x & xs] (range 0 (.cols hier))
                [_ _ rarea :as result] nil]
           (if (nil? x)
             result
             (let [sq (MatOfPoint2f.)
                   c (nth contours x)
                   area (Imgproc/contourArea c)
                   perim (count-perimeter c)]
               (Imgproc/approxPolyDP (doto (MatOfPoint2f.) (.fromArray (.toArray c))) sq (* perim 0.02) true)
               (cond
                 (and (= 4 (.rows sq)) (> area (or rarea 0)))
                 (recur xs [sq x area])

                 :else (recur xs result)))))

         ;; Sort the corners
         sorted
         (if-not (nil? sq)
           (let [pointlist (matofpoint-vec sq)
                 closest-to-origin (first (sort-by #(apply + %) (matofpoint-vec sq)))
                 ;; Rotate corners until closest-to-origin is first
                 [p1 [_ pfy :as pf] pc [_ ply :as pl] :as rotated] (take 4 (drop-while #(not= closest-to-origin %) (concat pointlist pointlist)))]
             ;; Flip so that 'top right' point is next
             (if (< pfy ply) rotated [p1 pl pc pf])))]

        ;; Draw the actual matching contour
        (Imgproc/drawContours colour contours x
                              (Scalar. 0 0 255) 1 0 hier 1 (Point. 0 0))

        ;; Draw the "board" polygon.
        (let [s (map-indexed vector sorted)]
          (reduce
            (fn [[i [ax ay :as a]] [_ [x y] :as p]]
              (cond
                (nil? a) p
                :else
                (do
                  (Core/line colour (Point. ax ay) (Point. x y) (Scalar. 0 0 (- 255 (* i 32))) 5)
                  p)))
            nil (concat s [(first s)])))

        sorted)))


#_(defn old-process [w calibration frame]
  (do
    #_(highlight-faces frame)
    (let [fil (Mat.) m (Mat.) m2 (Mat.) edges (Mat.) hough (Mat.) hough-img (Mat.)
          corners (MatOfPoint.) corners2f (MatOfPoint2f.)
          timg (Mat.) detectimg (Mat.)

          dest
          (-->
            frame
            (Imgproc/cvtColor Imgproc/COLOR_BGR2GRAY)
            (Imgproc/bilateralFilter 5 (double 155) (double 105))
            )

          colour
          (-->
            dest
            (Imgproc/cvtColor Imgproc/COLOR_GRAY2BGR))
          [p1 pf _ pl :as goban-corners] (find-goban dest colour)
          goban-contour (util/vec->mat (MatOfPoint2f.) goban-corners)]
      (Imgproc/goodFeaturesToTrack dest corners 1000 0.03 15 (Mat.) 10 false 0.1)
      (.fromArray corners2f (.toArray corners))
      (Imgproc/cornerSubPix dest corners2f (Size. 11 11) (Size. -1 -1)
                            (TermCriteria. (bit-or TermCriteria/EPS TermCriteria/COUNT) 30 0.1))

      #_(println goban-corners)
      #_(println (filter
                   #(pos? (Imgproc/pointPolygonTest goban-corners % true))
                   (seq (.toArray corners2f))))


      (let
        [goban-points
         (->>
           (seq (.toArray corners2f))
           (filter
             #(> (Imgproc/pointPolygonTest goban-contour % true) -10))
           (map (fn [p] [(.-x p) (.-y p)])))
         _ (println (count goban-points))
         {:keys [size target]}
         (condp < (count goban-points)
           400 {:size 19 :target (:19x19 target-homography)}
           100 {:size 13 :target (:13x13 target-homography)}
           {:size 9 :target (:9x9 target-homography)}
           )
         sorted
         (sort-by
           (juxt
             (comp #(int (/ % 25)) (partial util/line-to-point-dist [p1 pf]))
             (comp #(int (/ % 25)) (partial util/line-to-point-dist [p1 pl])))
           (take (.rows target) goban-points))
         origpoints
         (doto (MatOfPoint2f.)
           (.fromList (map (fn [[x y]] (Point. x y)) goban-corners)))
         h (if (= (.rows target) (count goban-corners))
             (Calib3d/findHomography ^MatOfPoint2f origpoints ^MatOfPoint2f target Calib3d/FM_RANSAC 3.0))]


        (if h
          (reset! current-transform h))

        (when-let [h @current-transform]
          (let [transformed (Mat.) ih (Mat.) invert-transformed (Mat.)]
            (Core/perspectiveTransform corners2f transformed h)
            #_(Core/perspectiveTransform target invert-transformed ih)

            (Imgproc/warpPerspective frame timg h (.size frame))
            (Imgproc/warpPerspective frame detectimg h (.size frame))

            #_(Imgproc/erode detectimg detectimg (Imgproc/getStructuringElement Imgproc/MORPH_RECT (Size. 10 10)))
            (doseq [c (range 0 (.rows transformed))]
              (let [[x1 y1 :as p] (seq (.get transformed c 0))]
                #_(Core/putText hough-img (str c) (Point. x1 y1) Core/FONT_HERSHEY_COMPLEX 1 (Scalar. 0 255 0) 2)
                (Core/circle colour (Point. x1 y1) 2 (Scalar. 128 255 0) 2)
                ))
            (doseq [c (range 0 (.rows invert-transformed))]
              (let [[x1 y1 :as p] (seq (.get invert-transformed c 0))]
                #_(Core/putText hough-img (str c) (Point. x1 y1) Core/FONT_HERSHEY_COMPLEX 1 (Scalar. 0 255 0) 2)
                (Core/circle colour (Point. x1 y1) 2 (Scalar. 0 255 0 128) 10)
                ))
            )
          (Core/rectangle detectimg (Point. 70 70) (Point. 980 980) (Scalar. 0 255 0) 1)

          (doseq [x (range 1 (inc size))]
            (doseq [y (range 1 (inc size))]
              (let [p (Point. (- (* 75.0 x) 15) (- (* 75.0 y) 15))
                    roi (Rect. p (Size. 30 30))
                    m (Mat. detectimg roi)
                    a (Core/mean m)
                    c (int (first (seq (.-val a))))
                    text (cond (< c 30) "B" (> c 200) "W")]
                (when text
                  (Core/putText timg text p Core/FONT_HERSHEY_COMPLEX 1 (Scalar. 0 0 255) 1.5))
                (Core/rectangle detectimg p (Point. (+ (.-x p) 30) (+ (.-y p) 30)) (Scalar. 0 255 0) 1)
                (Core/circle timg (Point. (+ (.-x p) 15) (+ (.-y p) 15)) 5 a 5))))))

      #_(doseq [{[x y] :value :as p}
                (filter #(> (:strength (meta %)) 10) (kdtree-seq @stable-points))]
          (Core/circle colour (Point. x y)
                       (/ (Math/min (or (:strength (meta p)) 1) 100) 5) (Scalar. 255 25 25) 2))

      (doseq [p (seq (.toArray corners2f))]
        (Core/circle colour p 2 (Scalar. 255 0 255) 3))


      (update-image-mat! 0 frame "Source")
      (update-image-mat! 1 dest "Find points")
      (update-image-mat! 2 colour "Find points")
      (update-image-mat! 3 timg "Detected goban")
      (update-image-mat! 4 detectimg "Check for pieces")
      #_(when-not (.empty timg)
          (update-image-mat! 2 timg "Perspective Shifted")
          (update-image-mat! 3 detectimg "Detect Stones")
          )
      (.repaint w)
      true)))

(defn refresh-camera [w camera frame]
  (Thread/sleep 100)
  #_(let [{:keys [frozen calib-corners calibration input] :as state} @appstate
        read (if frozen true (if (= input :camera) (.read camera frame) false))
        frame (if (= input :camera) frame input)]
    (cond
      (not read) false
      :else
      (old-process w calibration frame)))
  (.repaint w))



(defn capture [camidx]
  (let [camera (VideoCapture. ^int camidx Videoio/CAP_ANY)
        frame (Mat.)]
    (.read camera frame)

    (cond
      (not (.isOpened camera)) (println "Error: camera not opened")
      :else
      (do
        (update-image-mat! 0 frame "Source")
        (let [w (window "Original" 0 0)]
          (swap! appstate assoc :diag-window w)
          (doto
            (Thread.
              #(loop []
                (try
                  (refresh-camera w camera frame)
                  (catch Exception e
                    (.printStackTrace e)
                    (Thread/sleep 5000)))
                (recur)))
            (.setDaemon true)
            (.start)))))
    #_(.release camera)))



;; Some work on hough circles, discarded because it gets very inaccurate on busy boards.
(comment
  (defn hough-circles [m]
    (util/with-release
      [mat (Mat.)
       bilat (Mat.)
       blurred (Mat.)
       white-mask (Mat.)
       black-mask (Mat.)
       laplacian (Mat.)
       masked (Mat.)
       circles (Mat.)
       canny (Mat.)]
      #_(doseq [r (range (count (:signature cluster)))]
          (let [[x y] [(mod r szx) (int (/ r szx))]]
            (.put ^Mat mat x y (double-array (repeat 3 (* 255.0 (double (get (:signature cluster) r))))))))

      (Imgproc/cvtColor m mat Imgproc/COLOR_HSV2BGR)
      (Imgproc/cvtColor mat mat Imgproc/COLOR_BGR2GRAY)
      (Imgproc/dilate mat blurred (Imgproc/getStructuringElement Imgproc/MORPH_ELLIPSE (Size. 7 7)))
      (Imgproc/erode blurred blurred (Imgproc/getStructuringElement Imgproc/MORPH_ELLIPSE (Size. 9 9)))
      (Imgproc/blur blurred bilat (Size. 5 5))
      #_(Imgproc/Laplacian bilat laplacian 0 1 0.8 0.1)
      #_(Core/addWeighted laplacian 10.0 bilat 0.8 10.0 bilat)

      (Core/compare bilat (Scalar. 200.0) white-mask Core/CMP_GT)
      (Imgproc/dilate white-mask white-mask (Imgproc/getStructuringElement Imgproc/MORPH_ELLIPSE (Size. 12 12)))
      (Core/compare bilat (Scalar. 80.0) black-mask Core/CMP_LT)
      #_(Imgproc/GaussianBlur blurred bilat (Size. 9 9) 20)
      #_(Imgproc/cvtColor (ui/illuminate-correct blurred) bilat Imgproc/COLOR_BGR2GRAY)
      #_(Imgproc/bilateralFilter blurred bilat 2 (double 10) (double 10))

      (Imgproc/Canny bilat canny 50 25)

      #_(q/image (util/mat-to-pimage bilat) 0 0)
      (let [min-radius 17
            max-radius 25]
        (Imgproc/HoughCircles bilat circles Imgproc/CV_HOUGH_GRADIENT 1 25 50 8 min-radius max-radius)

        (let [found (doall (map #(vec (.get circles 0 %)) (range (.cols circles))))]
          #_(println "---------------")
          {:bilat (util/mat-to-pimage bilat)
           :canny (util/mat-to-pimage canny)
           :white (doall (filter (fn [[x y]]
                                   #_(println (first (.get ^Mat white-mask y x)))
                                   (= 255 (int (first (.get ^Mat white-mask y x))))) found))
           :black (doall (filter (fn [[x y]]
                                   #_(println (first (.get ^Mat white-mask y x)))
                                   (= 255 (int (first (.get ^Mat black-mask y x))))) found))}
          #_{:white (filter (fn [[x y]] (= 1 (int (first (.get ^Mat white-mask y x))))) found)
             :black (filter (fn [[x y]] (= 1 (int (first (.get ^Mat black-mask y x))))) found)}))))
  (defn read-circle-board [samplepoints {:keys [black white] :as circles}]
    #_(for [[y row] (map-indexed vector samplepoints)
            [x [px py]] (map-indexed vector row)]
        )
    ))


;; Some prelim work on determining the corners of a board to ease or skip initial calibration.

(defn mat->lines [^Mat mat]
  (for [x (range (.cols mat))]
    (.get mat 0 x)))

(defn theta [[x1 y1 x2 y2]]
  (mod (Math/atan2 (- y1 y2) (- x1 x2)) Math/PI))

(defn avg-theta [vs]
  (/ (reduce #(+ %1 (nth %2 4)) 0 vs) (count vs)))

(defn group-lines [avg lines]
  (let [opp (mod (- avg (/ Math/PI 2)) Math/PI)
        [mn mx] (sort [(mod (- avg (/ Math/PI 2)) Math/PI) avg])]
    (group-by #(if (< mn (nth % 4) mx) avg opp) lines)))

(defn line-group [[cx cy] [x1 y1 x2 y2 t :as l]]
  (if (< (/ Math/PI 4) t (* 3 (/ Math/PI 4)))
    [(Math/round (* t 5)) (Math/round (- x2 (/ (- y2 cy) (Math/tan t)))) cy]
    [(Math/round (* t 5)) cx (Math/round (- y2 (* (- x2 cx) (Math/tan t))))]))


(defn remove-outliers [[k ls]]
  (let [avg (last (last (take (/ (count ls) 2) (sort-by #(nth % 4) ls))))]
    [avg (filter (fn [[_ _ _ _ t]] (< (Math/abs (double (- t avg))) (/ Math/PI 9))) ls)]))


(defn find-board [ctx]
  (let [{{:keys [homography shift reference]} :view
         {:keys [raw]} :camera
         {:keys [size]} :goban} @ctx

        cleaned (Mat.)
        bilat (Mat.)
        mask (Mat.)
        pts2f (MatOfPoint2f.)]
    (.copyTo raw cleaned)
    #_(Imgproc/filter2D cleaned cleaned 1  (Mat. [1 1 1 1 -8 1 1 1 1]))
    (Imgproc/cvtColor cleaned cleaned Imgproc/COLOR_BGR2GRAY)
    #_(Imgproc/equalizeHist cleaned cleaned)
    #_(Imgproc/bilateralFilter cleaned bilat 5 (double 15) (double 15))
    (Imgproc/GaussianBlur cleaned bilat (Size. 5 5) 2)
    (Imgproc/Laplacian bilat bilat -8 3 8 2)
    (Imgproc/cvtColor bilat bilat Imgproc/COLOR_GRAY2BGR)
    (Imgproc/cvtColor bilat bilat Imgproc/COLOR_BGR2HSV)
    (Core/inRange bilat (Scalar. 0 0 100) (Scalar. 180 255 255) mask)

    #_(Imgproc/Canny bilat bilat 200 50 3 true)
    (Imgproc/HoughLinesP mask pts2f 1 (/ Math/PI 360) 100 50 10)
    #_(println (util/write-mat pts2f))
    (Imgproc/cvtColor bilat bilat Imgproc/COLOR_HSV2BGR)
    #_(println (avg-theta (mat->lines pts2f)))
    #_(println (map theta (mat->lines pts2f)))
    #_(let [groups
            (->>
              (mat->lines pts2f)
              group-lines
              (map remove-outliers))])
    (let [lines (map (fn [[x1 y1 x2 y2 :as k]] [x1 y1 x2 y2 (theta k)]) (mat->lines pts2f))
          avg (avg-theta lines)]
      #_(println "=================================================")
      (swap! ctx update-in [:linedump] conj (map remove-outliers (group-lines avg lines)))
      #_(doseq [[k ls] (map remove-outliers (group-lines avg lines))
              [[_ gx gy] gls] (group-by (partial line-group [(/ (.cols bilat) 2) (/ (.rows bilat) 2)]) ls)]

        #_(println [x1 y1 x2 y2 t])
        #_(println g " -- " (count gls))
        (let [[x1 y1 x2 y2 t] (first gls)
              k (* k (/ 180 Math/PI))
              l (min (* (count gls) 30) 255)]
          (Core/line bilat (Point. x1 y1) (Point. x2 y2) (Scalar. 255 l 0) 5)
          (Core/line bilat (Point. gx gy ) (Point. x2 y2) (Scalar. 0 0 255) 2)
          #_(let [x (/ (/ (.rows bilat) 2) (Math/tan t))]
              #_(println x " = " y2 " / " (Math/tan t))
              (Core/line bilat (Point. (+ x (- x1 (/ y1 (Math/tan t)))) (/ (.rows bilat) 2)) (Point. x2 y2) (Scalar. 255 0 0) 13)
              #_(Core/line bilat (Point. xa 0) (Point. x1 y1) (Scalar. 255 0 0) 13)))))


    #_(doseq [[x1 y1 x2 y2] (partition 4 (:data (util/write-mat pts2f)))]
        (Core/line bilat (Point. x1 y1) (Point. x2 y2) (Scalar. 0 255 0) 1))
    #_(println (util/write-mat pts2f))
    #_(doseq [p (seq (.toArray pts2f))]
        (Core/line cropped p 2 (Scalar. 255 0 255) 3))
    (comment
      ;; Finding interesting points.. This doesn't lend itself too well :/
      (Imgproc/cvtColor cropped cleaned Imgproc/COLOR_BGR2GRAY)
      (Imgproc/bilateralFilter cleaned bilat 5 (double 15) (double 15))
      (Imgproc/goodFeaturesToTrack bilat pts 500 0.01 (- camera/block-size 5))
      (.fromArray pts2f (.toArray pts))
      (Imgproc/cornerSubPix bilat pts2f (Size. 11 11) (Size. -1 -1)
        (TermCriteria. (bit-or TermCriteria/EPS TermCriteria/COUNT) 30 0.1))
      (doseq [p (seq (.toArray pts2f))]
        (Core/circle cropped p 2 (Scalar. 255 0 255) 3)))
    (swap! ctx assoc-in [:goban :flat]
      (util/mat-to-pimage bilat nil))
    #_(util/write-mat pts)))