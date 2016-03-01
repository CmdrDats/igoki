(ns igoki.web.handler
  (:require [compojure.core :refer [GET defroutes]]
            [compojure.route :refer [not-found resources]]
            [hiccup.page :refer [include-js include-css html5]]
            [igoki.web.middleware :refer [wrap-middleware]]
            [environ.core :refer [env]]
            [igoki.ui :as ui]
            [igoki.util :as util])
  (:import (javax.imageio ImageIO)
           (com.sun.xml.internal.messaging.saaj.util ByteOutputStream)
           (java.io ByteArrayInputStream)))

(def mount-target
  [:div#app
   [:h3 "Loading..."]
   [:p "If this message persists, please run "
    [:b "lein figwheel"]
    " in order to start the compiler"]])

(def loading-page
  (html5
    [:head
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport"
             :content "width=device-width, initial-scale=1"}]
     (include-css (if (env :dev) "css/site.css" "css/site.min.css"))
     (include-css (if (env :dev) "semantic/semantic.css" "semantic/semantic.min.css"))]
    [:body
     mount-target
     (include-js "js/app.js")]))

(defn pic [req]
  (if-let [raw (-> @ui/ctx :camera :raw)]
    (let [bufimage (util/mat-to-buffered-image raw)
          out (ByteOutputStream.)]
      (ImageIO/write bufimage "jpg" out)
      {:body         (ByteArrayInputStream. (.toByteArray out))
       :content-type "image/jpeg"
       :headers      {"Cache-Control" "no-cache, no-store, must-revalidate"
                      "Pragma"        "no-cache"
                      "Expires"       "0"}})))

(defroutes routes
  (GET "/" [] loading-page)
  (GET "/about" [] loading-page)
  (GET "/cap.png" [] pic)
  
  (resources "/")
  (not-found "Not Found"))

(def app (wrap-middleware #'routes))
