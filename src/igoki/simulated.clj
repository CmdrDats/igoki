(ns igoki.simulated
  (:require [igoki.ui :as ui]
            [quil.core :as q]
            [igoki.util :as util]
            [igoki.game :as game])
  (:import (org.opencv.core Mat Size Core CvType Point Scalar MatOfPoint MatOfPoint2f MatOfByte)
           (org.opencv.imgproc Imgproc)
           (org.opencv.highgui Highgui)
           (de.schlichtherle.truezip.fs FsEntryNotFoundException)))

;; This view simulates a camera for testing igoki's behaviour without having a board and camera handy
(defonce simctx (atom {:sketchconfig {:framerate 20 :size [640 480]}}))

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

(defmethod ui/construct :simulation [ctx]
  (reset-board ctx 19))





(defn stone-colors [c]
  (if (= c :w)
    [(Scalar. 255 255 255) (Scalar. 28 28 28)]
    [(Scalar. 28 28 28) (Scalar. 0 0 0)]))

(defn draw-stone [m x y c cellsize]
  (when c
    (let [[incolor bcolor] (stone-colors c)]
      (Core/circle m (Point. x y) (/ cellsize 4) incolor (/ cellsize 2))
      (Core/circle m (Point. x y) (/ cellsize 2) bcolor 2))))

(defn draw-board [^Mat m]
  (try
    (when (and m (.rows m))
      #_(.setTo m (Scalar. 92 179 220))
      (let [{{:keys [size board next]} :sim} @simctx
            [cellsize grid-start] (grid-spec m)
            [mx my] (if (q/focused)
                      [(* (/ (.rows m) (q/height)) (q/mouse-x))
                       (* (/ (.cols m) (q/width)) (q/mouse-y))] [2000 2000])]
        (doseq [x (range size)]
          (let [coord (+ grid-start (* x cellsize))
                extent (+ grid-start (* cellsize (dec size)))]

            (Core/line m (Point. coord grid-start) (Point. coord extent) (Scalar. 0 0 0))
            (Core/line m (Point. grid-start coord) (Point. extent coord) (Scalar. 0 0 0))
            ))

        (doseq [[x y] (game/star-points size)]
          (Core/circle m (Point. (+ grid-start (* x cellsize))
                                 (+ grid-start (* y cellsize))) 2 (Scalar. 0 0 0) 2))

        (doseq [[y rows] (map-indexed vector board)
                [x v] (map-indexed vector rows)]
          (when v
            (draw-stone m (+ grid-start (* cellsize x)) (+ grid-start (* cellsize y)) v cellsize)))

        (let [[x y] (stone-point [mx my] grid-start cellsize)]
          (Core/circle m (Point. (+ grid-start (* x cellsize))
                                 (+ grid-start (* y cellsize))) (/ cellsize 2) (Scalar. 0 0 255) 1))

        (draw-stone m mx my (-> @simctx :sim :next) cellsize)

        (util/with-release [pts (MatOfPoint.)]
          (util/vec->mat pts (map (fn [[x y]] [(+ (or mx 100) x) (+ (or my 100) y)]) [[0 0] [120 0] [120 55] [200 200] [55 120] [0 120] [0 0]]))
          (Core/fillPoly m [pts] (Scalar. 96 90 29)))))

    (catch Exception e
      (.printStackTrace e)))
  m)

(defn simulate []
  (let [context @simctx]
    (if (= (:mode context) :replay)
      (swap! simctx
             update :camera assoc
             :raw (-> context :camera :raw)
             :pimg (util/mat-to-pimage (-> context :camera :raw)))
      (util/with-release [m (.clone (-> context :sim :background))]
        (draw-board m)
        (swap! simctx
               update :camera assoc
               :raw m
               :pimg (util/mat-to-pimage m))))))

(defmethod ui/draw :simulation [ctx]

  (simulate)

  (let [{{:keys [^Mat raw pimg]} :camera
         {:keys [frame]} :replay
         mode :mode} @ctx
        [cellsize grid-start] (if raw (grid-spec raw) [])
        tx (q/height)]
    (q/fill 128 64 78)
    (q/rect 0 0 (q/width) (q/height))
    (cond
      (nil? pimg)
      (ui/shadow-text "Image not built yet, please wait..." 10 25)
        (= mode :replay)
        (do
          (q/image pimg 0 0 (q/width) (q/height))
          (ui/shadow-text "Esc: Back to simulation" tx 50)
          (ui/shadow-text "Arrows: Forward/back " tx 75)
          (ui/shadow-text "L: Load captured zip" tx 100)
          )
        :else
        (do
          (q/image pimg 0 0 (q/width) (q/height))
          (ui/shadow-text "Tab: Cycle Size" tx 50)
          (ui/shadow-text "B: Black " tx 75)
          (ui/shadow-text "W: White" tx 100)
          (ui/shadow-text "A: Alternating" tx 125)
          (ui/shadow-text "C: Clear" tx 150)
          (ui/shadow-text "R: Reset board. " tx 175)
          (ui/shadow-text "L: Load zip. " tx 200)
          ))))

(defn next-stone [{:keys [next mode]}]
  (case mode
    :black :b
    :white :w
    :erase nil
    (if (= next :w) :b :w)))

(defmethod ui/mouse-pressed :simulation [ctx]
  (swap!
    ctx
    (fn [{{:keys [raw]} :camera :keys [sim] :as c}]
      (let [[cs gs] (grid-spec raw)
            [px py] (stone-point [(* (/ (.rows raw) (q/height)) (q/mouse-x))
                                  (* (/ (.cols raw) (q/width)) (q/mouse-y))] gs cs)
            current (get-in sim [:board py px] :outside)]
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
    (let [{{:keys [file index]} :replay} @ctx
          nextindex (nextfn index)
          image (util/zip-read-file file (str nextindex ".jpg"))
          raw (Highgui/imdecode (MatOfByte. image) Highgui/IMREAD_UNCHANGED)
          pimg (util/mat-to-pimage raw)]
      (swap!
        ctx
        (fn [c]
          (-> c
              (update :camera assoc :raw raw :pimg pimg)
              (update :replay assoc :index nextindex)))))
    (catch FsEntryNotFoundException e)))

(defn load-zip [ctx file]
  (println "Loading:" file)
  (swap! ctx assoc :mode :replay :replay {:file file :index 0})
  (step-file-index ctx identity))


(defmethod ui/key-pressed :simulation [ctx]
  (case
    (q/key-code)
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
    (println "Unhandled key-down: " (q/key-code))
    ))





(defn start-simulation [ctx]
  (swap! simctx assoc
         :stopped false
         :sim {:background (Highgui/imread "./resources/wooden-background.jpg")})
  (doto
    (Thread.
      #(when-not
        (-> @simctx :stopped)
        (let [{{:keys [raw pimg]} :camera} @simctx]
          (swap! ctx update :camera assoc :raw raw :pimg pimg)
          (Thread/sleep (or (-> @ctx :camera :read-delay) 500))
          (recur))))
    (.start))
  (ui/start (ui/transition simctx :simulation)))

(defn stop []
  (swap! simctx assoc :stopped true)
  (q/with-sketch (-> @simctx :sketch) (q/exit)))