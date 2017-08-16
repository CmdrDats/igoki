(ns igoki.core
  (:require
    [igoki.ui]
    [igoki.goban]
    [igoki.view]
    [igoki.game]
    [igoki.ui :as ui]
    [igoki.web.server :as server]
    [clojure.tools.logging :as log]
    [igoki.web.handler :as handler])
  (:gen-class)
  (:import (java.awt Desktop)
           (java.net URI)))

(nu.pattern.OpenCV/loadShared)

(defn start []
  (ui/read-loop ui/ctx 0)
  (ui/start (ui/transition ui/ctx :goban)))

(defn -main [& args]
  (server/start ui/ctx)

  (when (Desktop/isDesktopSupported)
    (.browse (Desktop/getDesktop) (URI. "http://localhost:3000"))))
