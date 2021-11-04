(ns igoki.core
  (:require
    [igoki.ui.main :as ui.main]
    [igoki.camera :as camera]
    [igoki.game :as game]
    [clojure.java.io :as io]
    [seesaw.core :as s])
  (:gen-class)
  (:import
    (org.slf4j.bridge SLF4JBridgeHandler)
    (nu.pattern OpenCV)
    (java.util.logging LogManager Level)
    (javax.imageio ImageIO)
    (java.awt Toolkit)))



(defn screen-size []
  (let [ss (.getScreenSize (Toolkit/getDefaultToolkit))]
    [(.width ss) (.height ss)]))


(defn image-size
  "
  See
  http://stackoverflow.com/questions/4217896/how-can-i-determine-the\\
  -dimensions-of-an-image-file-of-arbitrary-format-jpeg-p
  "
  [image-stream]
  (let [image (ImageIO/read image-stream)]
    [(.getWidth image) (.getHeight image)]))

(defn splash!
  "
  Use Seesaw to display an undecorated window based on graphic in
  resources directory (splash screen).
  "
  [resource-file & {:keys [duration filename height width]
                    :or {duration 2500}}]
  (let [[scrw scrh] (screen-size)
        img_bg (s/icon resource-file)
        [imgw imgh] (image-size resource-file)
        xpos (/ (- scrw imgw) 2)
        ypos (/ (- scrh imgh) 2)
        fr (s/frame :size [imgw :by imgh]
             :icon "igoki48.png"
             :title "igoki"
             :undecorated? true
             :content (s/label :icon img_bg))]

    (doto
      (Thread.
        (fn []
          (s/move! fr :to [xpos ypos])
          (s/show! fr)
          (try
            (Thread/sleep duration)
            (finally
              (s/hide! fr)))))
      (.setDaemon true)
      (.start))
    #(when (s/visible? fr) (s/hide! fr))))


(OpenCV/loadShared)
(SLF4JBridgeHandler/install)
(.setLevel (.getLogger (LogManager/getLogManager) "") Level/INFO)



(defonce ctx (atom {}))
(defn start []
  ;; TODO: these are gross. refactor out these init steps.
  ;; The spice should just flow.
  (camera/start-calibration ctx)
  (game/init ctx)

  (ui.main/main-frame ctx))

(defn -main [& args]
  (let [hide-splash (splash! (io/resource "splash.png") :duration 30000)]
    (start)
    (hide-splash)))
