(ns igoki.integration.robot
  (:require [seesaw.core :as s]
            [igoki.camera :as camera])
  (:import (java.awt Robot Rectangle)
           (java.awt.image BufferedImage AffineTransformOp)
           (java.awt.geom AffineTransform)
           (org.nd4j.linalg.exception ND4JIllegalStateException)))


; BufferedImage before = getBufferedImage(encoded);
; int w = before.getWidth();
; int h = before.getHeight();
; BufferedImage after = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
; AffineTransform at = new AffineTransform();
; at.scale(2.0, 2.0);
; AffineTransformOp scaleOp =
;    new AffineTransformOp(at, AffineTransformOp.TYPE_BILINEAR);
; after = scaleOp.filter(before, after);

(defn rescale-image [bufimg [width height]]
  (let [result (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        g2d (.getGraphics result)]

    (.drawImage g2d bufimg 0 0 width height nil)
    (.dispose g2d)
    result))

(defn read-frame [ctx]
  (let [{:keys [goban] :as c} @ctx
        {:keys [frame bounds ^Robot robot]} (:robot c)
        _ (println "bounds:" bounds)
        _ (println "robot:" robot)
        size (or (:size goban) 19)
        [x y w h] bounds
        _ (.setVisible frame false)
        _ (Thread/sleep 50)
        bufimg (.createScreenCapture robot (Rectangle. x y w h))
        scaled (rescale-image bufimg (camera/ref-size-vec (dec size)))
        ]

    (swap! ctx update :robot assoc
      :scaled scaled
      :board
      (doall
        (for [y (range size)]
          (doall
            (for [x (range size)]
              (try
                (let [pt
                      (.getSubimage scaled (* x camera/block-size) (* y camera/block-size)
                        camera/block-size camera/block-size)
                      [b e w]
                      (try
                        (camera/eval-spot pt)
                        (catch ND4JIllegalStateException e
                          (.printStackTrace e)))]
                  (cond
                    (> b 0.5) :black
                    (> w 0.7) :white))
                (catch Exception e)))))))
    (.setVisible frame true)
    (.repaint frame)))

(defn read-robot-loop [ctx]
  (let [{:keys [goban robot]} @ctx]
    (read-frame ctx)
    (Thread/sleep 1000)

    (when (:started robot)
      (recur ctx))))

(defn start-capture [ctx bounds]
  (try
    (swap! ctx update :robot assoc :started true :robot (Robot.) :bounds bounds)

    (read-frame ctx)
    (doto
      (Thread. (partial #'read-robot-loop ctx))
      (.setDaemon true)
      (.start))
    (catch Exception e
      (s/alert (str "Could not start capturing: " (.getName (.getClass e)) " - " (.getMessage e)) :type :error)
      (.printStackTrace e))))

(defn stop-capture [ctx]
  (when (-> @ctx :robot :started)
    (swap! ctx update :robot assoc :started false)))