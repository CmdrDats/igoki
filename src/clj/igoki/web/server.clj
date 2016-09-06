(ns igoki.web.server
  (:require [igoki.web.handler :as handler]
            [environ.core :refer [env]]
            [org.httpkit.server :as server])
  (:gen-class))

;; Battle plan:

;; DONE: Go board component
;; Game view (turn indicator + captured pieces view)
;; Game setup popup
;; DONE: Camera input tab
;; Camera raw view
;; Camera raw: board corner setup
;; Camera flattened view
;; Camera flattened tuning

;;

 (defn start []
   (let [port (Integer/parseInt (or (env :port) "3000"))]
     (server/run-server handler/app {:port port :join? false})
     (handler/start-router!)))
