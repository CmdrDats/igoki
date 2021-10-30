(ns igoki.ui.calibration
  (:require
    [seesaw.core :as s]
    [igoki.camera :as camera]
    [igoki.litequil :as lq]
    [igoki.util :as util]
    [igoki.projector :as projector]
    [clojure.java.io :as io]
    [igoki.ui.util :as ui.util])
  (:import
    (javax.swing JComboBox BorderFactory)))

(defn calibration-options [ctx]
  (s/flow-panel
    :items
    ["Size: "
     (s/combobox
       :listen
       [:action
        (fn [e]
          (let [sel (.getSelectedIndex ^JComboBox (.getSource e))]
            (camera/set-board-size ctx (nth [9 13 19] sel))))]
       :model ["9x9" "13x13" "19x19"]
       :selected-index 2)
     [20 :by 10]
     "Camera: "
     (s/combobox
       :listen
       [:action
        (fn [e]
          (if (.getParent (.getSource e))
            (.grabFocus (.getParent (.getSource e))))
          (doto
            (Thread.
              #(camera/select-camera ctx (- (.getSelectedIndex ^JComboBox (.getSource e)) 2)))
            (.setDaemon true)
            (.start)))]
       :model
       (concat
         ["Off"
          "Simulated"]
         (for [x (range 5)]
           (str "Camera " (inc x))))
       :selected-index 0)
     [20 :by 10]
     (s/button :text "Projector Window"
       :listen
       [:action
        (fn [e]
          (projector/start-cframe ctx))])
     [20 :by 10]
     (s/button
       :id :kofi-button
       :icon (io/resource "kofi.png")
       :border (BorderFactory/createEmptyBorder)
       :focusable? false
       :listen
       [:action
        (fn [e]
          (ui.util/open "https://ko-fi.com/cmdrdats"))])]))

(defn construct [ctx]
  (lq/smooth)
  (lq/frame-rate 5)
  (lq/background 200))

(defn convert-point [bufimg [px py]]
  [(/ (* px (lq/width)) (.getWidth bufimg))
   (/ (* py (lq/height)) (.getHeight bufimg))])

(def pn ["A19" "T19" "T1" "A1"])

(defn draw [ctx]
  (lq/background 128 64 78)
  (lq/rect 0 0 (lq/width) (lq/height))

  (let [c (camera/camera-image ctx)]
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
  (when-let [size (camera/camera-size ctx)]
    (let [[cx cy] size
          p
          [(/ (* (lq/mouse-x) cx) (lq/width))
           (/ (* (- (lq/mouse-y) 5) cy) (lq/height))]

          points (get-in @ctx [:goban :points])
          points (util/update-closest-point points p)]
      (camera/update-corners ctx points))))

(defn mouse-pressed [ctx e]
  (when-let [size (camera/camera-size ctx)]
    (let [[cx cy] size
          p
          [(/ (* (lq/mouse-x) cx) (lq/width))
           (/ (* (- (lq/mouse-y) 5) cy) (lq/height))]

          points (get-in @ctx [:goban :points])
          points
          (if (> (count points) 3)
            (util/update-closest-point points p)
            (vec (conj points p)))]
      (camera/update-corners ctx points))))


;; TODO: This isn't even working - this needs to all become UI elements or just straight
;; out dropped.
(defn key-typed [ctx e]
  (case (lq/key-code e)
    32 (swap! ctx update-in [:goban :flat-view?] (fnil not false))
    67 (camera/cycle-corners ctx)
    (println "Unhandled key-down: " (lq/key-code e))))

(defn calibration-panel [ctx]
  (s/border-panel
    :id :calibration-panel
    :south
    (calibration-options ctx)
    :center
    (:panel
      (lq/sketch-panel
        {:setup (partial #'construct ctx)
         :draw (partial #'draw ctx)
         :mouse-dragged (partial #'mouse-dragged ctx)
         :mouse-pressed (partial #'mouse-pressed ctx)
         :key-typed (partial #'key-typed ctx)}))))