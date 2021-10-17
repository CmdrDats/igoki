(ns igoki.calibration
  (:require
    [igoki.util :as util]
    [igoki.ui :as ui]
    [igoki.view :as view]
    [igoki.simulated :as sim]
    [igoki.litequil :as lq]
    [seesaw.core :as s])
  (:import
    (org.opencv.core MatOfPoint2f Core)
    (java.awt.image BufferedImage)))

(defn start-simulation [ctx]
  (sim/stop)
  (ui/stop-read-loop ctx)
  (sim/start-simulation ctx))

(defn read-stones [ctx]
  (when (-> @ctx :view :homography)
    (view/camera-updated ctx)
    (swap! ctx view/read-board)

    (util/with-release [src (MatOfPoint2f.) dst (MatOfPoint2f.)]
      (Core/perspectiveTransform (util/vec->mat src (apply concat (:samplepoints (:view @ui/ctx)))) dst (.inv (:homography (:view @ui/ctx))))
      (swap! ctx assoc-in [:goban :camerapoints] (util/mat->seq dst)))))

(defn reverse-transform [ctx]
  (when (= 4 (count (-> @ctx :goban :edges)))
    (swap! ctx view/update-homography)
    (let [context @ctx
          homography (-> context :view :homography)
          size (-> context :goban :size)
          d (dec size)
          [topleft topright bottomright bottomleft] (view/target-points size)]

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
          (Core/perspectiveTransform pts ref (.inv (-> @ui/ctx :view :homography)))

          (swap! ctx assoc-in [:goban :lines] (partition 2 (util/mat->seq ref)))
          (view/update-reference ctx))))))

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

(defn cycle-size [ctx]
  (swap! ctx update-in [:goban :size]
    (fn [s]
      (case s 19 9 9 13 19)))
  (reverse-transform ctx))

;; All UI code from here on out.

(defn construct [ctx]
  (ui/setup ctx)
  (lq/frame-rate 20)
  (start-calibration ctx))

(defn destruct [ctx]
  (stop-calibration ctx))

(defn convert-point [bufimg [px py]]
  [(/ (* px (lq/width)) (.getWidth bufimg))
   (/ (* py (lq/height)) (.getHeight bufimg))])

(def pn ["A19" "T19" "T1" "A1"])

(defn draw [ctx]
  (lq/background 128 64 78)
  (lq/rect 0 0 (lq/width) (lq/height))

  (let [c (camera-image ctx)]
    (cond
      (nil? c)
      (lq/shadow-text "Could not acquire image?" 10 25)

      :else
      (let [{{:keys [size edges points lines flat flat-view? camerapoints]} :goban
             board :board} @ctx

            points (map (partial convert-point c) points)
            edges (map #(map (partial convert-point c) %) edges)]
        (lq/image c 0 0 (lq/width) (lq/height))
        (lq/shadow-text "Please select the corners of the board" 10 25)

        (lq/shadow-text "<Tab> Cycle 9x9, 13x13 and 19x19. " 10 50)
        (lq/shadow-text "<Enter> Confirm Calibration" 10 75)
        (lq/shadow-text "<Space> Toggle flat view" 10 100)
        (lq/shadow-text "<1..5> Camera Sources" 10 125)
        (lq/shadow-text "<S> Camera Sim" 10 150)

        (lq/color 255 255 255 128)
        (lq/stroke-weight 0.5)
        (when (and camerapoints board size)
          (doseq [[idx p] (map-indexed vector camerapoints)
                  :let [[px py] (convert-point c p)
                        stone (get-in board [(int (/ idx size)) (mod idx size)])]
                  :when stone]
            (if (= stone :b)
              (do (lq/background 0 0 0) (lq/color 255 255 255))
              (do (lq/background 255 255 255) (lq/color 0 0 0)))
            (lq/ellipse px py 10 10)))



        (lq/color 255 255 255 96)
        (lq/stroke-weight 1)
        (when lines
          (doseq [[p1 p2] lines]
            (lq/line (convert-point c p1) (convert-point c p2)))
          (lq/shadow-text
            (str size "x" size)
            (/ (reduce + (map first points)) 4)
            (/ (reduce + (map second points)) 4)
            :center :bottom))

        (lq/color 78 64 255 128)
        (lq/stroke-weight 2)
        (doseq [[p1 p2] edges]
          (lq/line p1 p2))


        (doseq [[p [x y]] (map-indexed vector points)]
          (lq/text (get pn p) x (- y 5)
            {:align [:center :bottom]})
          (lq/ellipse x y 2 2))
        (when (and flat flat-view?)
          (lq/image (:bufimg flat) 0 0 (lq/width) (lq/height)))))))

(defn mouse-dragged [ctx e]
  (when-let [c ^BufferedImage (camera-image ctx)]
    (let [px (/ (* (lq/mouse-x) (.getWidth c)) (lq/width))
          py (/ (* (- (lq/mouse-y) 5) (.getHeight c)) (lq/height))
          p [px py]
          points (-> @ctx :goban :points)
          points (util/update-closest-point points p)]
      (update-corners ctx points))))

(defn mouse-pressed [ctx e]
  (when-let [c ^BufferedImage (camera-image ctx)]
    (let [px (/ (* (lq/mouse-x) (.getWidth c)) (lq/width))
          py (/ (* (- (lq/mouse-y) 5) (.getHeight c)) (lq/height))
          p [px py]
          points (-> @ctx :goban :points)
          points
          (if (> (count points) 3)
            (util/update-closest-point points p)
            (vec (conj points p)))]
      (update-corners ctx points))))

(defn mouse-released [ctx e]
  (reverse-transform ctx))

(defn cycle-corners [ctx]
  (update-corners ctx (vec (take 4 (drop 1 (cycle (-> @ctx :goban :points)))))))



(defn key-typed [ctx e]
  (case (lq/key-code e)
    10
    (ui/transition ctx :kifu)
    9 (cycle-size ctx)
    49 (do (sim/stop) (ui/switch-read-loop ctx 0))
    50 (do (sim/stop) (ui/switch-read-loop ctx 1))
    51 (do (sim/stop) (ui/switch-read-loop ctx 2))
    52 (do (sim/stop) (ui/switch-read-loop ctx 3))
    53 (do (sim/stop) (ui/switch-read-loop ctx 4))
    83 (start-simulation ctx)
    32 (swap! ctx update-in [:goban :flat-view?] (fnil not false))
    67 (cycle-corners ctx)
    (println "Unhandled key-down: " (lq/key-code e))))

(defn calibration-options [ctx]
  (s/flow-panel
    :items
    ["Size: "
     (s/combobox
       :model ["9x9" "13x13" "19x19"])
     [20 :by 10]
     "Camera: "
     (s/combobox
       :model
       (concat
         ["Simulated"]
         (for [x (range 5)]
           (str "Camera " x))))]))

(defn calibration-panel [ctx]
  (s/border-panel
    :south
    (calibration-options ctx)
    :center
    (:panel
      (lq/sketch-panel
        {:setup (partial #'construct ctx)
         :close (partial #'destruct ctx)
         :draw (partial #'draw ctx)
         :mouse-dragged (partial #'mouse-dragged ctx)
         :mouse-pressed (partial #'mouse-pressed ctx)
         :mouse-released (partial #'mouse-released ctx)
         :key-typed (partial #'key-typed ctx)}))))


(comment
  ;; What was this again? maybe delete.
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
