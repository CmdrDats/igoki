(ns igoki.web.server
  (:require [igoki.web.handler :refer [app]]
            [environ.core :refer [env]]
            [ring.adapter.jetty :refer [run-jetty]])
  (:gen-class))

;; Battle plan:

;; Go board component
;; Game view (turn indicator + captured pieces view)
;; Game setup popup
;; Camera input tab
;; Camera raw view
;; Camera raw: board corner setup
;; Camera flattened view
;; Camera flattened tuning

;;

 (defn start []
   (let [port (Integer/parseInt (or (env :port) "3000"))]
     (run-jetty app {:port port :join? false})))
