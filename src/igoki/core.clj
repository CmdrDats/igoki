(ns igoki.core
  (:require
    [igoki.ui.main :as ui.main]
    [igoki.camera :as camera]
    [igoki.game :as game])
  (:gen-class)
  (:import
    (org.slf4j.bridge SLF4JBridgeHandler)
    (nu.pattern OpenCV)
    (java.util.logging LogManager Level)))

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
  (start))
