(ns igoki.web.handler
  (:require [compojure.core :refer [GET POST defroutes]]
            [compojure.route :refer [not-found resources]]
            [hiccup.page :refer [include-js include-css html5]]
            [igoki.web.middleware :refer [wrap-middleware]]
            [environ.core :refer [env]]
            [igoki.ui :as ui]
            [igoki.util :as util]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit      :refer (sente-web-server-adapter)])
  (:import (javax.imageio ImageIO)
           (com.sun.xml.internal.messaging.saaj.util ByteOutputStream)
           (java.io ByteArrayInputStream)
           (java.util UUID)))

(let [{:keys [ch-recv send-fn ajax-post-fn ajax-get-or-ws-handshake-fn
              connected-uids]}
      (sente/make-channel-socket! sente-web-server-adapter {})]
  (def ring-ajax-post                ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk                       ch-recv) ; ChannelSocket's receive channel
  (def chsk-send!                    send-fn) ; ChannelSocket's send API fn
  (def connected-uids                connected-uids) ; Watchable, read-only atom
  )

(defmulti -event-msg-handler
          "Multimethod to handle Sente `event-msg`s"
          :id ; Dispatch on event-id
          )

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [{:as ev-msg :keys [id ?data event]}]
  (-event-msg-handler ev-msg))

(defmethod -event-msg-handler
  :default ; Default/fallback case (no other matching handler)
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (let [session (:session ring-req)
        uid     (:uid     session)]
    (println "Unhandled event: from " uid " - " event)
    (when ?reply-fn
      (?reply-fn {:umatched-event-as-echoed-from-from-server event}))))

;; TODO Add your (defmethod -event-msg-handler <event-id> [ev-msg] <body>)s here...

;;;; Sente event router (our `event-msg-handler` loop)

(defonce router_ (atom nil))
(defn  stop-router! [] (when-let [stop-f @router_] (stop-f)))
(defn start-router! []
  (stop-router!)
  (reset! router_
          (sente/start-server-chsk-router!
            ch-chsk event-msg-handler)))


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
     (include-css (if (env :dev) "css/site.css" "css/site.min.css"))]
    [:body
     mount-target
     (include-js "js/app.js")]))

(defn index [req]
  (if (-> req :session :uid)
    loading-page
    {:status  200
     :body    loading-page
     :headers {"Content-Type" "text/html"}
     :session (assoc (:session req) :uid (.toString (UUID/randomUUID)))}))

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
  (GET "/" [] index)
  (GET "/about" [] index)
  (GET "/cap.png" [] pic)
  (GET  "/chsk" req (ring-ajax-get-or-ws-handshake req))
  (POST "/chsk" req (ring-ajax-post                req))

  (resources "/")
  (not-found "Not Found"))

(def app
  (wrap-middleware #'routes))
