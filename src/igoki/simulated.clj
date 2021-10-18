(ns igoki.simulated
  (:require
    [igoki.ui :as ui]
    [igoki.litequil :as lq]
    [igoki.util :as util])
  (:import
    (org.opencv.core Mat Point Scalar MatOfPoint MatOfByte)
    (de.schlichtherle.truezip.fs FsEntryNotFoundException)
    (javax.swing JFrame)
    (java.awt.event MouseEvent)
    (org.opencv.imgproc Imgproc)
    (org.opencv.imgcodecs Imgcodecs)))

;; This view simulates a camera for testing igoki's behaviour without having a board and camera handy
(defonce simctx (atom {:sketchconfig {:framerate 5 :size [640 480]}}))

(defn blank-board [size]
  (vec
    (for [y (range size)]
      (vec (for [x (range size)] nil)))))

(defn stone-point [[mx my] grid-start cell-size]
  [(int (/ (+ (- mx grid-start) (/ cell-size 2)) cell-size))
   (int (/ (+ (- my grid-start) (/ cell-size 2)) cell-size))])

(defn grid-spec [m]
  (let [size (-> @simctx :sim :size)
        cellsize (/ (.rows m) (+ size 2))
        grid-start (+ cellsize (/ cellsize 2))]
    [cellsize grid-start]))

(defn reset-board [ctx size]
  (swap! ctx update :sim assoc :size size :board (blank-board size) :next :b :mode :alt))

(defn stone-colors [c]
  (if (= c :w)
    [(Scalar. 255 255 255) (Scalar. 28 28 28)]
    [(Scalar. 28 28 28) (Scalar. 0 0 0)]))

(defn draw-stone [m x y c cellsize]
  (when c
    (let [[incolor bcolor] (stone-colors c)]
      (Imgproc/circle m (Point. x y) (/ cellsize 4) incolor (/ cellsize 2))
      (Imgproc/circle m (Point. x y) (/ cellsize 2) bcolor 2))))

(defn draw-board [^Mat m]
 (try
    (when (and m (.rows m))
      #_(.setTo m (Scalar. 92 179 220))
      (let [{{:keys [size board next]} :sim} @simctx
            [cellsize grid-start] (grid-spec m)
            mpos (lq/mouse-position)
            [mx my]
            (if mpos
              [(* (/ (float (.getX mpos)) (lq/width)) (.width m))
               (* (/ (float (.getY mpos)) (lq/height)) (.height m))]
              [2000 2000])]

        (doseq [x (range size)]
          (let [coord (+ grid-start (* x cellsize))
                extent (+ grid-start (* cellsize (dec size)))]

            (Imgproc/line m (Point. coord grid-start) (Point. coord extent) (Scalar. 0 0 0))
            (Imgproc/line m (Point. grid-start coord) (Point. extent coord) (Scalar. 0 0 0))))


        (doseq [[x y] (util/star-points size)]
          (Imgproc/circle m (Point. (+ grid-start (* x cellsize))
                                 (+ grid-start (* y cellsize))) 2 (Scalar. 0 0 0) 2))

        (doseq [[y rows] (map-indexed vector board)
                [x v] (map-indexed vector rows)]
          (when v
            (draw-stone m (+ grid-start (* cellsize x)) (+ grid-start (* cellsize y)) v cellsize)))

        (let [[x y] (stone-point [mx my] grid-start cellsize)]
          (Imgproc/circle m (Point. (+ grid-start (* x cellsize))
                                 (+ grid-start (* y cellsize))) (/ cellsize 2) (Scalar. 0 0 255) 1))

        (draw-stone m mx my (-> @simctx :sim :next) cellsize)

        (util/with-release [pts (MatOfPoint.)]
          (util/vec->mat pts (map (fn [[x y]] [(+ (or mx 100) x) (+ (or my 100) y)]) [[0 0] [120 0] [120 55] [200 200] [55 120] [0 120] [0 0]]))
          (Imgproc/fillPoly m [pts] (Scalar. 96 90 29)))))

    (catch Exception e
      (.printStackTrace e)))
  m)

(defn simulate []
  (let [context @simctx]

    (if (= (:mode context) :replay)
      (when (-> context :camera :raw)
        (swap! simctx
          update :camera assoc
          :raw (-> context :camera :raw)
          :pimg (util/mat-to-pimage (-> context :camera :raw)
                  (-> context :camera :pimg :bufimg))))
      (when (-> context :sim :background)
        (let [m (.clone (-> context :sim :background))]
          (draw-board m)
          (swap! simctx
            update :camera assoc
            :raw m
            :pimg
            (util/mat-to-pimage m
              (-> context :camera :pimg :bufimg))))))))


(defn next-stone [{:keys [next mode]}]
  (case mode
    :black :b
    :white :w
    :erase nil
    (if (= next :w) :b :w)))

(defn mouse-pressed [ctx ^MouseEvent e]
  (swap! ctx
    (fn [{{:keys [raw]} :camera :keys [sim] :as c}]
      (let [[cs gs] (grid-spec raw)
            mpos (lq/mouse-position)
            [px py]
            (stone-point
              [(* (/ (float (.getX mpos)) (lq/width)) (.width raw))
               (* (/ (float (.getY mpos)) (lq/height)) (.height raw))] gs cs)
            current (get-in sim [:board py px] :outside)]
        (println "[cs gs]" [cs gs])
        (println "[px py]" [px py])
        (println "current" current)
        (cond
          (= current :outside) c
          :else
          (-> c
              (assoc-in [:sim :board py px] (:next sim))
              (assoc-in [:sim :next] (next-stone sim))))))))

(defn alternate-mode [{:keys [next] :as sim}]
  (assoc sim :mode :alt :next (if (= next :w) :b :w)))

(defn set-mode [sim c]
  (assoc sim :mode c :next (case c :white :w :black :b nil)))

(defn step-file-index [ctx nextfn]
  (try
    (let [{{:keys [file index]} :replay
           {:keys [pimg]} :camera} @ctx
          nextindex (nextfn index)
          image (util/zip-read-file file (str nextindex ".jpg"))
          raw (Imgcodecs/imdecode (MatOfByte. image) Imgcodecs/IMREAD_UNCHANGED)
          pimg (util/mat-to-pimage raw (:bufimg pimg))]
      (swap!
        ctx
        (fn [c]
          (-> c
              (update :camera assoc :raw raw :pimg pimg)
              (update :replay assoc :index nextindex)))))
    (catch FsEntryNotFoundException e (.printStackTrace e))))

(defn load-zip [ctx file]
  (println "Loading:" file)
  (swap! ctx assoc :mode :replay :replay {:file file :index 0})
  (step-file-index ctx identity))

(defn load-img [ctx file]
  (swap! ctx assoc :mode :replay :replay {:file file :index 0})
  (step-file-index ctx identity))

(defn key-pressed [ctx e]
  (case (lq/key-code e)
    9
    (swap!
      ctx update-in [:sim :size]
      (fn [s]
        (case s 19 9 9 13 19)))
    ;; A
    65 (swap! ctx update :sim alternate-mode)
    ;; C - clear mode
    67 (swap! ctx update :sim set-mode :erase)
    ;; W
    87 (swap! ctx update :sim set-mode :white)
    ;; B
    66 (swap! ctx update :sim set-mode :black)
    ;; R
    82 (reset-board ctx (-> @ctx :sim :size))
    ;; Left
    37 (step-file-index simctx dec)
    ;; Right
    39 (step-file-index simctx inc)
    ;; L
    76 (ui/load-dialog #(load-zip simctx (.getAbsolutePath %)) (str (System/getProperty "user.dir") "/capture"))
    (println "Unhandled key-down: " (lq/key-code e))))



(defn setup [ctx]
  (lq/smooth)
  (lq/frame-rate (or (-> @ctx :sketchconfig :framerate) 5))
  (lq/background 200))

(defn paint [ctx]
  (simulate)
  (let [{{:keys [^Mat raw pimg]} :camera
         {:keys [frame index]} :replay
         :keys [stopped mode]} @ctx
        [cellsize grid-start] (if raw (grid-spec raw) [])
        tx (- (lq/width) 180)]
    (lq/background 128 64 78)
    (lq/rect 0 0 (lq/width) (lq/height))
    (cond
      stopped
      (lq/shadow-text "Select 'simulation' camera..." 20 35)

      (nil? pimg)
      (lq/shadow-text "Image not built yet, please wait..." 10 25)
      (= mode :replay)
      (do
        (lq/image (:bufimg pimg) 0 0 (lq/width) (lq/height))
        (lq/shadow-text "Esc: Back to simulation" tx 50)
        (lq/shadow-text (str "Arrows: Forward/back (" index ")") tx 75)
        (lq/shadow-text "L: Load captured zip" tx 100))

      :else
      (do
        (lq/image (:bufimg pimg) 0 0 (lq/width) (lq/height))
        (lq/shadow-text "Tab: Cycle Size" tx 50)
        (lq/shadow-text "B: Black " tx 75)
        (lq/shadow-text "W: White" tx 100)
        (lq/shadow-text "A: Alternating" tx 125)
        (lq/shadow-text "C: Clear" tx 150)
        (lq/shadow-text "R: Reset board. " tx 175)
        (lq/shadow-text "L: Load zip. " tx 200)))))

(defn start-simulation [ctx]
  (swap! simctx
    (fn [s]
      (->
        s
        (assoc :stopped false)
        (update :sim assoc :background (Imgcodecs/imread "./resources/wooden-background.jpg")))))
  (reset-board simctx 19)

  (doto
    (Thread.
      #(when-not
        (-> @simctx :stopped)
        (let [{{:keys [raw pimg]} :camera} @simctx]
          ;; TODO: this needs to write to a different 'camera' view so it doesn't clash with camera input.
          (swap! ctx update :camera assoc :raw raw :pimg pimg)
          (Thread/sleep (or (-> @ctx :camera :read-delay) 500))
          (recur))))
    (.setDaemon true)
    (.start)))

(defn simulation-panel [ctx]
  (let [sketch
        (lq/sketch-panel
          {:draw (partial #'paint simctx)
           :setup (partial #'setup simctx)
           :mouse-pressed (partial #'mouse-pressed simctx)
           :key-pressed (partial #'key-pressed simctx)})]
    (swap! simctx assoc :sketch sketch)
    (:panel sketch)))

(defn stop []
  (swap! simctx assoc :stopped true)
  (when-let [frame (get-in @simctx [:sketch :frame])]
    (.dispose ^JFrame frame)))
