(ns igoki.ogs
  (:require
    [clj-http.client :as client]
    [clojure.tools.logging :as log])
  (:import (io.socket.client Socket IO Ack)
           (io.socket.emitter Emitter$Listener)
           (org.json JSONObject)))

;; http://docs.ogs.apiary.io/
;; https://ogs.readme.io/docs/real-time-api

(def url "https://online-go.com")
(def cm (clj-http.conn-mgr/make-reusable-conn-manager {:timeout 10 :threads 3 :insecure? true}))

(comment
  (def cm (clj-http.conn-mgr/make-reusable-conn-manager {:timeout 2 :threads 3 :insecure? true}))
  )

(defn ogs-auth
  [conn]
  (client/post
    (str url "/oauth2/access_token")
    {:connection-manager cm
     :form-params (-> conn (assoc :grant_type "password") (dissoc :url))
     :insecure? true
     :as :json}))

(defn ogs-headers
  [auth]
  {:connection-manager cm
   :insecure? true
   :headers   {"Authorization" (str "Bearer " (-> auth :body :access_token))}
   :as :json})

(defn me
  [auth]
  (client/get
    (str url "/api/v1/me/")
    (ogs-headers auth)))

(defn my-settings
  [auth]
  (client/get
    (str url "/api/v1/me/settings/")
    (ogs-headers auth)))

(defn my-games
  [auth]
  (client/get
    (str url "/api/v1/me/games/")
    (ogs-headers auth)))

(defn game-detail
  [auth id]
  (client/get
    (str url "/api/v1/games/" id)
    (ogs-headers auth)))

(defn game-sgf
  [auth id]
  (client/get
    (str url "/api/v1/games/" id "/sgf/")
    (ogs-headers auth)))

(defn move
  [auth id coords]
  (client/post
    (str url "/api/v1/games/" id "/move/")
    (assoc
      (ogs-headers auth)
      :form-params {:move coords}
      :content-type :json)))

(defn socket-echo [& xs]
  (println "Echo: " xs))


(defn socket-listener [socket event lfn]
  (.on socket
       event
       (proxy [Emitter$Listener] []
         (call [xs]
           (println "Socket event: " event)
           (apply lfn (seq xs))))))

(defn socket-emit [sock event msg]
  (let [m (JSONObject.)]
    (doseq [[k v] msg]
      (.put m (name k) v))
    (.emit sock event
           (into-array JSONObject [m]))))

(defn socket-callback [sock event msg callback-fn]
  (let [m (JSONObject.)]
    (doseq [[k v] msg]
      (.put m (name k) v))
    (.emit sock event
           (into-array JSONObject [m])
           (proxy [Ack] []
             (call [xs] (apply callback-fn (seq xs)))))))

(defn setup-socket []
  (let [sock (IO/socket "https://ggs.online-go.com")]
    (doseq [e [Socket/EVENT_CONNECT Socket/EVENT_CONNECT_ERROR
               Socket/EVENT_CONNECT_TIMEOUT Socket/EVENT_DISCONNECT
               Socket/EVENT_ERROR Socket/EVENT_MESSAGE
               Socket/EVENT_RECONNECT Socket/EVENT_RECONNECT_ATTEMPT
               Socket/EVENT_RECONNECT_ERROR Socket/EVENT_RECONNECT_FAILED
               Socket/EVENT_RECONNECTING]]
      (socket-listener sock e socket-echo))
    (.connect sock)
    sock))

(comment
  (.on Socket/EVENT_CONNECT
       (proxy [Emitter$Listener] []
         (call [xs] (apply socket-connect (seq xs)))))

  (SLF4JBridgeHandler/install)
  (.setLevel (.getLogger (LogManager/getLogManager) "") Level/FINEST)
  ;; Get clientid and secret by auth2 client here: https://online-go.com/developer
  ;; Get app password from user profile

  (def auth
    (ogs-auth
      {:client_id     ""
       :client_secret ""
       :username      ""
       :password      ""})
    )
  (def auth (ogs-auth (read-string (slurp ".creds"))))
  (def socket (setup-socket))

  (def player (:body (me auth)))
  (def game (:body (client/get (str url "/api/v1/games/3374557") (ogs-headers auth))))
  (socket-emit socket "game/connect" {:game_id (:id game) :player_id (:id player) :chat false})
  (socket-emit socket "game/move" {:game_id (:id game) :move "rg" :player_id (:id player) :auth (:auth game)}))