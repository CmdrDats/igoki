(ns igoki.core
  (:require
    [igoki.ui]
    [igoki.goban]
    [igoki.view]
    [igoki.game]
    [igoki.ui :as ui])
  (:gen-class))

(nu.pattern.OpenCV/loadShared)

(defn start []
  (ui/read-loop ui/ctx 0)
  (ui/start (ui/transition ui/ctx :goban)))

(defn -main [& args]
  (start))