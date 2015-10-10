(ns igoki.goban
  (:require
    [igoki.util :as util]
    [igoki.ui :as ui]
    [quil.core :as q]
    [igoki.simulated :as sim])
  (:import (processing.core PImage)
           (javax.swing JFileChooser)))

;; TODO: Implement simulation mode (separate window for 'camera' input), to dev without a camera setup.
;; TODO: Replace grid with simply the size text in the middle, since the grid is misleading.

(defn start-simulation [ctx]
  (sim/stop)
  (ui/stop-read-loop ctx)
  (sim/start-simulation ctx))



(defmethod ui/construct :goban [ctx]
  (when-not (:goban @ctx)
    (swap! ctx assoc :goban
           {:points []
            :size   19})))

(defn convert-point [pimg [px py]]
  [(/ (* px (q/width)) (.width pimg))
   (/ (* py (q/height)) (.height pimg))])

(defmethod ui/draw :goban [ctx]
  (q/fill 128 64 78)
  (q/rect 0 0 (q/width) (q/height))

  (let [c (-> @ctx :camera :pimg)]
    (cond
      (nil? c)
      (ui/shadow-text "Could not acquire image?" 10 25)
      :else
      (let [{{:keys [size edges points]} :goban} @ctx
            points (map (partial convert-point c) points)
            edges (map #(map (partial convert-point c) %) edges)
            pn ["A19" "T19" "T1" "A1"]
            d (dec size)
            [e1 e2 e3 e4] edges]
        (q/image c 0 0 (q/width) (q/height))

        (ui/shadow-text "Please select the corners of the board" 10 25)

        (ui/shadow-text "<Tab> Cycle 9x9, 13x13 and 19x19. " 10 50)
        (ui/shadow-text "<Enter> Confirm Calibration" 10 75)
        (ui/shadow-text "<1..5> Camera Sources" 10 100)
        (ui/shadow-text "<S> Camera Sim" 10 125)

        (q/stroke 78 64 255 128)
        (q/stroke-weight 1)
        (when e4
          (doseq [t (range 1 d)]
            (q/line
              (util/point-along-line e1 (/ t d))
              (util/point-along-line (util/flipped-line e3) (/ t d)))
            (q/line
              (util/point-along-line e2 (/ t d))
              (util/point-along-line (util/flipped-line e4) (/ t d)))))

        (q/stroke-weight 2)
        (doseq [[p1 p2] edges]
          (q/line p1 p2))

        (q/text-align :center :bottom)
        (doseq [[p [x y]] (map-indexed vector points)]
          (q/text (get pn p) x (- y 5))
          (q/ellipse x y 2 2))))))

(defn update-edges [{:keys [points] :as goban}]
  (assoc goban
    :edges (partition 2 (interleave points (take 4 (drop 1 (cycle points)))))))

(defn update-closest-point [{:keys [points] :as goban} p]
  (let [indexed-dist
        (->>
          points
          (map-indexed (fn [i g] [(util/line-length-squared [g p]) i]))
          sort)
        [_ i :as e] (first indexed-dist)]
    (assoc-in goban [:points i] p)))

(defmethod ui/mouse-dragged :goban [ctx]
  (when-let [c ^PImage (-> @ctx :camera :pimg)]
    (let [px (/ (* (q/mouse-x) (.width c)) (q/width))
          py (/ (* (- (q/mouse-y) 5) (.height c)) (q/height))]
      (swap!
        ctx update-in [:goban]
        (fn [goban p]
          (update-edges
            (if (> (count (:points goban)) 3)
              (update-closest-point goban p)
              (update goban :points (comp vec conj) p))))
        [px py]))))

(defmethod ui/mouse-pressed :goban [ctx]
  (ui/mouse-dragged ctx))

(defmethod ui/key-pressed :goban [ctx]
  (case
    (q/key-code)
    10
    (ui/transition ctx :view)
    9
    (swap!
      ctx update-in [:goban :size]
      (fn [s]
        (case s 19 9 9 13 19)))
    49 (do (sim/stop) (ui/switch-read-loop ctx 0))
    50 (do (sim/stop) (ui/switch-read-loop ctx 1))
    51 (do (sim/stop) (ui/switch-read-loop ctx 2))
    52 (do (sim/stop) (ui/switch-read-loop ctx 3))
    53 (do (sim/stop) (ui/switch-read-loop ctx 4))
    83 (start-simulation ctx)
    (println "Unhandled key-down: " (q/key-code))))

