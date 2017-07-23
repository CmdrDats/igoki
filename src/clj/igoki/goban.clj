(ns igoki.goban
  (:require
    [igoki.util :as util]
    [igoki.ui :as ui]
    [igoki.view :as view]
    [quil.core :as q]
    [igoki.simulated :as sim])
  (:import (processing.core PImage)
           (javax.swing JFileChooser)
           (org.opencv.core Mat MatOfPoint2f Core Rect MatOfPoint Scalar TermCriteria Size Point MatOfKeyPoint MatOfDMatch MatOfByte CvType)
           (org.opencv.imgproc Imgproc)
           (org.opencv.features2d FeatureDetector DescriptorExtractor DescriptorMatcher DMatch Features2d KeyPoint)
           (java.util LinkedList)
           (org.opencv.calib3d Calib3d)
           (org.opencv.highgui Highgui)))



(defn start-simulation [ctx]
  (sim/stop)
  (ui/stop-read-loop ctx)
  (sim/start-simulation ctx))

(defn reverse-transform [ctx]
  (when (= 4 (count (-> @ctx :goban :edges)))
    (swap! ctx view/update-homography)
    (let [context @ctx
          tweaked-homography (-> context :goban :tweaked-homography)
          homography (-> context :view :homography)
          size (-> context :goban :size)
          d (dec size)
          [topleft topright bottomright bottomleft] (view/target-points size)]

      (when homography
        (util/with-release [ref (MatOfPoint2f.)
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
          (if tweaked-homography
            (Core/perspectiveTransform pts pts (.inv tweaked-homography)))
          (Core/perspectiveTransform pts ref (.inv (-> @ui/ctx :view :homography)))
          (swap! ctx assoc-in [:goban :lines] (partition 2 (util/mat->seq ref))))))))

(defn find-points-feature-detect [ctx]
  (let [{{:keys [homography shift reference]} :view
         {:keys [^Mat raw ^Mat prev]} :camera
         {:keys [size]} :goban} @ctx
        newprev (Mat.)
        flattened (Mat.)
        mx (* view/block-size (inc size))
        roi (Rect. 0 0 mx mx)]
    (when homography

      (.copyTo flattened newprev)
      #_(swap! ctx assoc-in [:camera :prev] newprev)

      (when prev
        ;; http://answers.opencv.org/question/10022/the-homography-tutorial-in-java/
        (let [cropped (Mat. flattened roi)
              detector (FeatureDetector/create 2)
              keypoints-object (MatOfKeyPoint.)
              keypoints-scene (MatOfKeyPoint.)

              extractor (DescriptorExtractor/create 2)
              desc-object (Mat.)
              desc-scene (Mat.)

              matcher (DescriptorMatcher/create 2)
              matches (MatOfDMatch.)
              new-flat (Mat.)]
          (.detect detector cropped keypoints-object)
          (.detect detector prev keypoints-scene)

          (.compute extractor cropped keypoints-object desc-object)
          (.compute extractor prev keypoints-scene desc-scene)

          (.match matcher desc-object desc-scene matches)

          (let [[min-dist max-dist]
                (loop [[^DMatch m & ms] (.toList matches)
                       min-dist 100
                       max-dist 0]
                  (if (nil? m)
                    [min-dist max-dist]
                    (recur ms (min min-dist (.distance m)) (max max-dist (.distance m)))))
                good-matches
                (filter (fn [^DMatch m] (< (.distance m) (* 2 min-dist))) (.toList matches))
                gm (MatOfDMatch.)
                img-matches (Mat.)

                keypoints-objlist (.toList keypoints-object)
                keypoints-scenelist (.toList keypoints-scene)

                obj
                (doto (MatOfPoint2f.)
                  (.fromList
                    (map
                      (fn [^DMatch m] (.pt ^KeyPoint (.get keypoints-objlist (.queryIdx m))))
                      good-matches)))

                scene
                (doto (MatOfPoint2f.)
                  (.fromList
                    (map
                      (fn [^DMatch m] (.pt ^KeyPoint (.get keypoints-scenelist (.trainIdx m))))
                      good-matches)))

                h (if (>= (count good-matches) 4) (Calib3d/findHomography obj scene))
                ]
            #_(println (count good-matches))
            (.fromList gm good-matches)
            (Features2d/drawMatches
              cropped keypoints-object
              prev keypoints-scene
              gm img-matches
              (Scalar. 255 255 0)
              (Scalar. 0 0 255)
              (MatOfByte.)
              2)

            ;; Display update
            (when h
              (Imgproc/warpPerspective flattened new-flat h (.size flattened)))

            (when h
              (swap! ctx assoc-in [:goban :tweaked-homography] h)


              #_(swap! ctx assoc-in [:goban :pimg]
                       (util/mat-to-pimage raw))
              (swap! ctx assoc-in [:goban :flat]
                     (util/mat-to-pimage img-matches nil nil))
              (reverse-transform ctx))))))))

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

  (let [r
        (if (< (/ Math/PI 4) t (* 3 (/ Math/PI 4)))
          [(Math/round (* t 5)) (Math/round (- x2 (/ (- y2 cy) (Math/tan t)))) cy]
          [(Math/round (* t 5)) cx (Math/round (- y2 (* (- x2 cx) (Math/tan t))))])]
    (comment
      (println l)
      (println [t r]))
    r))

(defn weld-lines [[mean lines]]
  [{}])


(defn remove-outliers [[k ls]]
  (let [avg (last (last (take (/ (count ls) 2) (sort-by #(nth % 4) ls))))]
    [avg (filter (fn [[_ _ _ _ t]] (< (Math/abs (double (- t avg))) (/ Math/PI 9))) ls)]))

(defn avg-together [[]])

(def lines (atom nil))

(defn find-minmax [[x1 y1 x2 y2 t1] [x3 y3 x4 y4 t2]]
  [(min x1 x2 x3 x4) (min y1 y2 y3 y4) (max x1 x2 x3 x4) (max y1 y2 y3 y4) t1])

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
      (doseq [[k ls] (map remove-outliers (group-lines avg lines))
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
      (Imgproc/goodFeaturesToTrack bilat pts 500 0.01 (- view/block-size 5))
      (.fromArray pts2f (.toArray pts))
      (Imgproc/cornerSubPix bilat pts2f (Size. 11 11) (Size. -1 -1)
                            (TermCriteria. (bit-or TermCriteria/EPS TermCriteria/COUNT) 30 0.1))
      (doseq [p (seq (.toArray pts2f))]
        (Core/circle cropped p 2 (Scalar. 255 0 255) 3)))
    (swap! ctx assoc-in [:goban :flat]
           (util/mat-to-pimage bilat nil nil))
    #_(util/write-mat pts)))


(defn camera-updated [wk ctx old new]
  (try
    (when (-> @ctx :goban :flat-view?)
      (find-board ctx))
    (catch Exception e (.printStackTrace e))))

(defmethod ui/construct :goban [ctx]
  (util/add-watch-path ctx :goban-camera [:camera] #'camera-updated)
  (when-not (:goban @ctx)
    (swap! ctx assoc :goban
           {:points []
            :size   19})))

(defmethod ui/destruct :goban [ctx]
  (remove-watch ctx :goban-camera))

(defn convert-point [pimg [px py]]
  [(/ (* px (q/width)) (.width pimg))
   (/ (* py (q/height)) (.height pimg))])

(def pn ["A19" "T19" "T1" "A1"])

(comment
  (when (-> @ui/ctx :goban :flat-view?)
    (find-points ui/ctx)))

(defmethod ui/draw :goban [ctx]
  (q/frame-rate 20)
  (q/fill 128 64 78)
  (q/rect 0 0 (q/width) (q/height))

  (let [c (-> @ctx :camera :pimg :pimg)]
    (cond
      (nil? c)
      (ui/shadow-text "Could not acquire image?" 10 25)
      :else
      (let [{{:keys [size edges points lines flat flat-view?]} :goban} @ctx
            points (map (partial convert-point c) points)
            edges (map #(map (partial convert-point c) %) edges)]
        (q/image c 0 0 (q/width) (q/height))
        (ui/shadow-text "Please select the corners of the board" 10 25)

        (ui/shadow-text "<Tab> Cycle 9x9, 13x13 and 19x19. " 10 50)
        (ui/shadow-text "<Enter> Confirm Calibration" 10 75)
        (ui/shadow-text "<Space> Toggle flat view" 10 100)
        (ui/shadow-text "<1..5> Camera Sources" 10 125)
        (ui/shadow-text "<S> Camera Sim" 10 150)

        (q/stroke 255 255 255 128)
        (q/stroke-weight 1)

        (when lines
          (doseq [[p1 p2] lines]
            (q/line (convert-point c p1) (convert-point c p2)))
          (ui/shadow-text
            (str size "x" size)
            (/ (reduce + (map first points)) 4)
            (/ (reduce + (map second points)) 4)
            :center :bottom))

        (q/stroke 78 64 255 128)
        (q/stroke-weight 2)
        (doseq [[p1 p2] edges]
          (q/line p1 p2))

        (q/text-align :center :bottom)
        (doseq [[p [x y]] (map-indexed vector points)]
          (q/text (get pn p) x (- y 5))
          (q/ellipse x y 2 2))
        (when (and flat flat-view?)
          (q/image (:pimg flat) 0 0 (q/width) (q/height)))))))

(defn update-corners [ctx points]
  (swap! ctx update :goban
    (fn [goban]
      (assoc
        goban
        :points points
        :edges (util/update-edges points))))
  (reverse-transform ctx))


(defmethod ui/mouse-dragged :goban [ctx]
  (when-let [c ^PImage (-> @ctx :camera :pimg :pimg)]
    (let [px (/ (* (q/mouse-x) (.width c)) (q/width))
          py (/ (* (- (q/mouse-y) 5) (.height c)) (q/height))
          p [px py]
          points (-> @ctx :goban :points)
          points
          (if (> (count points) 3)
            (util/update-closest-point points p)
            (vec (conj points p)))]
      (update-corners ctx points))))

(defmethod ui/mouse-pressed :goban [ctx]
  (ui/mouse-dragged ctx))

(defmethod ui/mouse-released :goban [ctx]
  (reverse-transform ctx))

(defn cycle-corners [ctx]

  (update-corners ctx (vec (take 4 (drop 1 (cycle (-> @ctx :goban :points)))))))

(defmethod ui/key-pressed :goban [ctx]
  (case
    (q/key-code)
    10
    (ui/transition ctx :view)
    9
    (do
      (swap!
        ctx update-in [:goban :size]
        (fn [s]
          (case s 19 9 9 13 19)))
      (reverse-transform ctx))
    49 (do (sim/stop) (ui/switch-read-loop ctx 0))
    50 (do (sim/stop) (ui/switch-read-loop ctx 1))
    51 (do (sim/stop) (ui/switch-read-loop ctx 2))
    52 (do (sim/stop) (ui/switch-read-loop ctx 3))
    53 (do (sim/stop) (ui/switch-read-loop ctx 4))
    83 (start-simulation ctx)
    32 (swap! ctx update-in [:goban :flat-view?] (fnil not false))
    67 (cycle-corners ctx)
    (println "Unhandled key-down: " (q/key-code))))

(comment
  (let [f
        (fn [[x1 y1 x2 y2]]
          (* (/ 180 Math/PI) (Math/atan2 (- x2 x1) (- y2 y1))))
        avg (/ (reduce + (map (comp #(mod % 180) f) lines2)) (count lines2))
        [minb maxb] (sort [(mod (- avg 45) 180) (mod (+ avg 45) 180)])
        grouped
        (group-by
          (comp
            (fn [a]
              (println minb " > " a " > " maxb)
              (< minb a maxb))
            f)
          lines2)]
    (println "avg: "avg)
    (map (comp #(/ (reduce + (map f %)) (count %)) second) grouped)

    #_(/ (reduce + (map f lines2)) (count lines2))))