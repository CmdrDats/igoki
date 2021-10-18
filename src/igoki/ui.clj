(ns igoki.ui
  (:require
    [igoki.util :as util]
    [igoki.litequil :as lq]
    [clojure.java.io :as io])

  (:import
    (org.opencv.core Mat Core)
    (javax.swing SwingUtilities JFrame JFileChooser)
    (org.opencv.imgproc Imgproc)
    (java.util LinkedList)
    (javax.swing.filechooser FileNameExtensionFilter)
    (org.opencv.imgcodecs Imgcodecs)
    (org.opencv.videoio VideoCapture Videoio)))

(defn setup [ctx]
  (lq/smooth)
  (lq/frame-rate (or (-> @ctx :sketchconfig :framerate) 5))
  (lq/background 200))

(defn state [ctx & _] (:state @ctx))

(defmulti construct state)
(defmethod construct :default [ctx])

(defmulti destruct state)
(defmethod destruct :default [ctx])

(defmulti draw state)
(defmethod draw :default [ctx]
  (lq/color 255 64 78)
  (lq/rect 0 0 (lq/width) (lq/height))
  (lq/shadow-text (str "State not implemented: " (:state @ctx)) 10 25))

(defmulti mouse-dragged state)
(defmethod mouse-dragged :default [ctx e])

(defmulti mouse-pressed state)
(defmethod mouse-pressed :default [ctx e])

(defmulti mouse-released state)
(defmethod mouse-released :default [ctx e])

(defmulti mouse-moved state)
(defmethod mouse-moved :default [ctx e])

(defmulti key-pressed state)
(defmethod key-pressed :default [ctx e])

(defn transition [ctx new-state]
  (destruct ctx)
  (swap! ctx assoc :state new-state)
  (construct ctx)
  ctx)

(defn start [ctx exit-fn]
  (let [sketch
        (lq/sketch
          {:title "igoki"
           :setup (partial setup ctx)
           :close exit-fn
           :draw (partial #'draw ctx)
           :size (or (-> @ctx :sketchconfig :size) [1280 720])
           :mouse-dragged (partial #'mouse-dragged ctx)
           :mouse-pressed (partial #'mouse-pressed ctx)
           :mouse-released (partial #'mouse-released ctx)
           :mouse-moved (partial #'mouse-moved ctx)
           :key-pressed (partial #'key-pressed ctx)})]

    (swap! ctx assoc :sketch sketch)))

;; Following code doesn't belong in here, but can move it out in due time.

(defonce ctx (atom {}))
(defn read-single [ctx camidx]
  (let [video (VideoCapture. (int camidx) Videoio/CAP_DSHOW)
        frame (Mat.)]
    (Thread/sleep 500)
    (.read video frame)
    (swap!
      ctx
      update :camera
      #(assoc %
        :raw frame
        :pimg (util/mat-to-pimage frame (get-in % [:pimg :bufimg]))))
    (.release video)))

(defn read-file [ctx fname]
  (let [frame (Imgcodecs/imread (str "resources/" fname))]
    (swap!
      ctx update :camera
      #(assoc %
        :raw frame
        :pimg (util/mat-to-pimage frame (get-in % [:pimg :bufimg]))))))

(defn stop-read-loop [ctx]
  (if-let [video ^VideoCapture (-> @ctx :camera :video)]
    (.release video))
  (swap! ctx update :camera assoc :stopped true :video nil))

(defn illuminate-correct [m]
  (util/with-release [lab-image (Mat.) equalized (Mat.)]
    (let [planes (LinkedList.)]
      (Imgproc/cvtColor m lab-image Imgproc/COLOR_BGR2Lab)
      (Core/split lab-image planes)
      (Imgproc/equalizeHist (first planes) equalized)
      (.copyTo equalized (first planes))
      (Core/merge planes lab-image)
      (Imgproc/cvtColor lab-image m Imgproc/COLOR_Lab2BGR)
      m)))

(defn camera-read [ctx video]
  (let [camera (:camera @ctx)]
    (when-not (.isOpened video)
      (println "Error: Camera not opened"))
    (when-not (:stopped camera)
      (try
        (let [frame (or (:frame camera) (Mat.))]
          (doseq [l (:pre-read-listeners camera)]
            (l ctx))

          (.read video frame)

          (doseq [l (:post-read-listeners camera)]
            (l ctx))

          (swap!
            ctx update :camera
            #(assoc %
               :raw frame
               ;; TODO: this chows memory - better to have a hook on update for each specific
               ;; view - this will only be needed on the first screen.
               :pimg (util/mat-to-pimage frame (get-in % [:pimg :bufimg])))))
        (Thread/sleep (or (-> @ctx :camera :read-delay) 1000))
        (catch Exception e
          (println "exception thrown")
          (.printStackTrace e)
          #_(stop-read-loop ctx)
          #_(throw e))))))

(defn add-camera-listeners [ctx pre-fn post-fn]
  (swap! ctx update :camera
    (fn [cam]
      (-> cam
          (update :pre-read-listeners conj pre-fn)
          (update :post-read-listeners conj post-fn)))))

(defn read-loop [ctx camidx]
  (when-not (-> @ctx :camera :stopped)
    (let [^VideoCapture video (VideoCapture. ^int camidx Videoio/CAP_DSHOW)]
      (swap! ctx update
        :camera assoc
        :video video
        :stopped false)
      (doto
        (Thread.
          ^Runnable
          #(when-not (-> @ctx :camera :stopped)
             (camera-read ctx video)
             (recur)))
        (.setDaemon true)
        (.start)))))

(defn switch-read-loop [ctx camidx]
  (stop-read-loop ctx)
  (Thread/sleep (* 2 (or (-> @ctx :camera :read-delay) 1000)))
  (swap! ctx assoc-in [:camera :stopped] false)
  (read-loop ctx camidx))

(defn save-dialog [current-file success-fn]
  (SwingUtilities/invokeLater
    #(let [frame (JFrame. "Save")
           chooser (JFileChooser.)]
      (try
        (.setAlwaysOnTop frame true)

        (doto chooser
          (.setSelectedFile (or current-file (io/file "game.sgf")))
          (.setFileFilter (FileNameExtensionFilter. "SGF Files" (into-array ["sgf"]))))

        (when
          (= JFileChooser/APPROVE_OPTION (.showSaveDialog chooser frame))
          (success-fn (.getSelectedFile chooser)))
        (finally (.dispose frame))))))

(defn load-dialog [success-fn & [start-dir]]
  (SwingUtilities/invokeLater
    #(let [frame (JFrame. "Load")
           chooser (if start-dir (JFileChooser. ^String start-dir) (JFileChooser.))]
      (try
        (.setAlwaysOnTop frame true)
        (doto chooser
          (.setFileFilter (FileNameExtensionFilter. "SGF Files" (into-array ["sgf"]))))

        (when
          (= JFileChooser/APPROVE_OPTION (.showOpenDialog chooser frame))
          (success-fn (.getSelectedFile chooser)))
        (finally (.dispose frame))))))


#_(start (transition ctx :goban))
