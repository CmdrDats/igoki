(ns igoki.ogs
  (:require
    [clj-http.client :as client]
    [clojure.tools.logging :as log]
    [clojure.string :as str]
    [cheshire.core :as json]
    [igoki.inferrence :as inferrence]
    [igoki.sgf :as sgf]
    [igoki.ui :as ui])
  (:import (io.socket.client Socket IO Ack)
           (io.socket.emitter Emitter$Listener)
           (org.json JSONObject)
           (java.util Date)
           (java.text SimpleDateFormat)))

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
    (dissoc (ogs-headers auth) :as)))

(defn move
  [auth id coords]
  (client/post
    (str url "/api/v1/games/" id "/move/")
    (assoc
      (ogs-headers auth)
      :form-params {:move coords}
      :content-type :json)))

(defn socket-echo [& xs]
  (log/info "Echo: " xs))


(defn socket-listener [^Socket socket event lfn]
  (.on socket
       event
       (proxy [Emitter$Listener] []
         (call [xs]
           (log/info "Socket event: " event)
           #_(apply lfn (seq xs))
           (apply lfn (map #(if (instance? JSONObject %) (json/decode (.toString %) keyword) %) (seq xs)))))))

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

(defn add-move [game [x y time]]
  (inferrence/play-move game [x y 0 (case (-> game :constructed :player-turn) :black :b :w)]))

(defn initialize-game [game]
  (let [initial-node
        (cond->
          {:branches     []
           :player-start [(case (:initial_player game) "white" "W" "B")]
           :application  ["Igoki"]
           :file-format  ["4"]
           :gametype     ["1"]
           :size         [(:width game) (:height game)]
           :date         [(.format (SimpleDateFormat. "YYYY-MM-dd") (Date. (* 1000 (:start_time game))))]
           :game-name    [(:game_name game)]
           :black-rank   [(-> game :players :black :rank)]
           :black-name   [(-> game :players :black :name)]
           :white-rank   [(-> game :players :white :rank)]
           :white-name   [(-> game :players :white :name)]}
          (not (str/blank? (-> game :initial_state :white)))
          (assoc :add-white (map (partial apply str) (partition 2 (-> game :initial_state :white))))
          (not (str/blank? (-> game :initial_state :black)))
          (assoc :add-black (map (partial apply str) (partition 2 (-> game :initial_state :black)))))

        game-setup
        (inferrence/reconstruct
          {:moves               initial-node
           :current-branch-path []
           :movenumber          0})]
    (reduce add-move game-setup (:moves game))))

(def game-events
  ["gamedata" "clock" "phase" "undo_requested" "undo_accepted" "move" "conditional_moves"
   "removed_stones" "removed_stones_accepted" "chat" "error" "reset"])

(defn disconnect [ctx]
  (let [ogs (:ogs @ctx)]
    (doseq [en game-events]
      (.off (:socket ogs) (str "game/" (:gameid ogs) "/" en)))
    (socket-emit (:socket ogs) "game/disconnect" {:game_id (:gameid ogs)})))

(defn connect-record [ctx socket auth gameid]
  (let [player (:body (me auth))
        action #(str "game/" gameid "/" %)
        listen
        (fn [eventname]
          (socket-listener
            socket (action eventname)
            #(do
              (log/info eventname ":" %)
              (swap! ctx update-in [:ogs :event-stream] (fnil conj []) {:eventname eventname :data %}))))]
    (doseq [en game-events]
      (listen en))

    (socket-listener
      socket (action "move")
      (fn [data]
        (swap! ctx update-in [:kifu]
               #(do
                 (ui/sound :click)
                 (add-move % (:move data))))))

    (socket-listener
      socket (action "gamedata")
      (fn [data]
        (cond
          (= "play" (:phase data))
          (swap! ctx update-in
                 [:kifu] #(merge % (initialize-game (-> @ctx :ogs :event-stream first :data))))

          (= "finished" (:phase data))
          (do
            (disconnect ctx)
            (let [game {:sgf          (:body (game-sgf auth gameid))
                        :event-stream (:event-stream (:ogs @ctx))
                        :gameid       gameid}]
              (spit (str "resources/ogs-game." gameid ".edn")
                    (pr-str game)))))))

    (socket-emit socket "game/connect" {:game_id gameid :player_id (:id player) :chat true})

    (swap! ctx assoc
      :ogs
      {:socket socket :auth auth
       :gameid gameid})))



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
  #_(def game (:body (client/get (str url "/api/v1/games/3374557") (ogs-headers auth))))
  #_(def ctx (atom {}))
  (connect-record ui/ctx socket auth "3374557")
  (socket-emit socket "game/connect" {:game_id (:id game) :player_id (:id player) :chat false})
  (socket-emit socket "game/move" {:game_id (:id game) :move "rg" :player_id (:id player) :auth (:auth game)}))