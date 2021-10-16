(ns igoki.core
  (:require
    [igoki.ui]
    [igoki.ui.main :as ui.main]
    [igoki.goban]
    [igoki.view]
    [igoki.game]
    [igoki.ui :as ui]
    [clojure.tools.logging :as log])
  (:gen-class))

(nu.pattern.OpenCV/loadShared)

(defn start []
  (ui/read-loop ui/ctx 0)
  (ui.main/main-frame)
  #_(ui/start (ui/transition ui/ctx :goban)
    #(ui/stop-read-loop ui/ctx)))

(defn -main [& args]
  (start))
