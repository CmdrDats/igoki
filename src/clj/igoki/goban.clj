(ns igoki.goban
  (:require
    [igoki.util :as util]
    [igoki.ui :as ui]
    [igoki.view :as view]
    [quil.core :as q]
    [igoki.simulated :as sim])
  (:import (processing.core PImage)
           (javax.swing JFileChooser)
           (org.opencv.core Mat MatOfPoint2f Core)))

(defn start-simulation [ctx]
  (sim/stop)
  (ui/stop-read-loop ctx)
  (sim/start-simulation ctx))

(defn reverse-transform [ctx]
  (when (= 4 (count (-> @ctx :goban :edges)))
    (swap! ctx view/update-homography)
    (let [context @ctx
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
          (Core/perspectiveTransform pts ref (.inv (-> @ui/ctx :view :homography)))
          (swap! ctx assoc-in [:goban :lines] (partition 2 (util/mat->seq ref))))))))


(defmethod ui/construct :goban [ctx]
  (when-not (:goban @ctx)
    (swap! ctx assoc :goban
           {:points []
            :size   19})))

(defn convert-point [pimg [px py]]
  [(/ (* px (q/width)) (.width pimg))
   (/ (* py (q/height)) (.height pimg))])

(def pn ["A19" "T19" "T1" "A1"])

(defmethod ui/draw :goban [ctx]
  (q/fill 128 64 78)
  (q/rect 0 0 (q/width) (q/height))

  (let [c (-> @ctx :camera :pimg)]
    (cond
      (nil? c)
      (ui/shadow-text "Could not acquire image?" 10 25)
      :else
      (let [{{:keys [size edges points lines]} :goban} @ctx
            points (map (partial convert-point c) points)
            edges (map #(map (partial convert-point c) %) edges)]
        (q/image c 0 0 (q/width) (q/height))
        (ui/shadow-text "Please select the corners of the board" 10 25)

        (ui/shadow-text "<Tab> Cycle 9x9, 13x13 and 19x19. " 10 50)
        (ui/shadow-text "<Enter> Confirm Calibration" 10 75)
        (ui/shadow-text "<1..5> Camera Sources" 10 100)
        (ui/shadow-text "<S> Camera Sim" 10 125)

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
          (q/ellipse x y 2 2))))))


(defmethod ui/mouse-dragged :goban [ctx]
  (when-let [c ^PImage (-> @ctx :camera :pimg)]
    (let [px (/ (* (q/mouse-x) (.width c)) (q/width))
          py (/ (* (- (q/mouse-y) 5) (.height c)) (q/height))]
      (swap!
        ctx update-in [:goban]
        (fn [{:keys [points] :as goban} p]
          (let [points (if (> (count points) 3)
                         (util/update-closest-point points p)
                         (vec (conj points p)))
                edges (util/update-edges points)]
            (assoc goban :points points :edges edges))
          )
        [px py])
      (reverse-transform ctx))))

(defmethod ui/mouse-pressed :goban [ctx]
  (ui/mouse-dragged ctx))

(defmethod ui/mouse-released :goban [ctx]
  (reverse-transform ctx))

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
    (println "Unhandled key-down: " (q/key-code))))

