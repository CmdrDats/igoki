(ns igoki.core
  (:require
    [igoki.ui.main :as ui.main]
    [igoki.camera :as camera]
    [igoki.view]
    [igoki.game :as game])
  (:gen-class))

(nu.pattern.OpenCV/loadShared)

(defonce ctx (atom {}))
(defn start []
  ;; TODO: these are gross. refactor out these init steps.
  ;; The spice should just flow.
  (camera/start-calibration ctx)
  (game/init ctx)

  (ui.main/main-frame ctx))

(defn -main [& args]
  (start))
