(ns igoki.web.handler
  (:require [compojure.core :refer [GET POST defroutes]]
            [compojure.route :refer [not-found resources]]
            [hiccup.page :refer [include-js include-css html5]]
            [igoki.web.middleware :refer [wrap-middleware]]
            [environ.core :refer [env]]
            [igoki.ui :as ui]
            [igoki.util :as util]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :refer (sente-web-server-adapter)]
            [clojure.set :as set])
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
  (try
    (-event-msg-handler ev-msg)
    (catch Exception e
      ;; Like, don't kill the messenger?
      (.printStackTrace e))))

(defmethod -event-msg-handler
  :default ; Default/fallback case (no other matching handler)
  [{:as ev-msg :keys [event id ?data ring-req ?reply-fn send-fn]}]
  (let [session (:session ring-req)
        uid     (:uid     session)]
    (println "Unhandled event: from " uid " - " event)
    (when ?reply-fn
      (?reply-fn {:umatched-event-as-echoed-from-from-server event}))))

;; TODO Add your (defmethod -event-msg-handler <event-id> [ev-msg] <body>)s here...

(defn reply [{:keys [send-fn ring-req] :as req} ev msg]
  (send-fn (-> ring-req :session :uid) [ev msg]))

(def cameralist
         (atom [{:id 1 :slot 0
                 :corners [[20 20] [20 80] [80 80] [80 20]]}
                {:id 2 :slot 1
                 :corners [[10 10] [20 80] [80 80] [80 12]]}]))

(defmethod -event-msg-handler
  :camera/list
  [{:keys [?data] :as req}]
  (.println System/out "hi")
  (reply req :camera/updatelist @cameralist))

(defmethod -event-msg-handler
  :camera/new
  [{:keys [?data] :as req}]
  (swap!
    cameralist
    (fn [c] (conj
              c
              {:id      (inc (apply max (map :id c)))
               :slot    0
               :corners [[100 100] [100 200] [200 200] [200 100]]
               :viewing :start})))
  (reply req :camera/updatelist @cameralist))

;;;; Sente event router (our `event-msg-handler` loop)

(defonce router_ (atom nil))
(defn stop-router! [] (when-let [stop-f @router_] (stop-f)))
(defn start-router! [ctx]
  (add-watch ctx ::router
    (fn [k r o n]
      (if-not (= (-> o :kifu :kifu-board)
                (-> n :kifu :kifu-board))
        (doseq [u (:any @connected-uids)]
          (println "Sending to " u)
          (chsk-send! u [:kifu/updated (-> n :kifu :kifu-board)])))))

  (add-watch connected-uids ::router
    (fn [k r o n]
      (doseq [u (set/difference (:any n) (:any o) )]
        (println "Sending board to " u)
        (chsk-send! u [:kifu/updated (-> @ctx :kifu :kifu-board)]))
      ))

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
     (include-css (if (env :dev) "css/site.css" "css/site.min.css"))
     (include-css "css/bootstrap.css")
     (include-css "css/material-design-iconic-font.min.css")
     (include-css "css/re-com.css")
     (include-css "css/roboto-italic.css")
     (include-css "css/roboto-condensed.css")]
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
    (let [bufimage (util/mat-to-buffered-image raw nil)
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
