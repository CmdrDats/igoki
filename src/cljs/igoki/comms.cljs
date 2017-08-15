(ns igoki.comms
  (:require
    [taoensso.encore :as encore]
    [taoensso.sente :as sente :refer (cb-success?)]))

(defn ->output! [fmt & args]
  (.log js/console (apply encore/format fmt args)))

(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! "/chsk" ; Note the same path as before
                                  {:type :auto ; e/o #{:auto :ajax :ws}
                                   })]
  (def chsk       chsk)
  (def ch-chsk    ch-recv) ; ChannelSocket's receive channel
  (def chsk-send! send-fn) ; ChannelSocket's send API fn
  (def chsk-state state)   ; Watchable, read-only atom
  )

(defn send
  ([eventtype message]
   (.log js/console "Sending " eventtype " :: " message)
   (chsk-send! [eventtype message]))
  ([eventtype message timeout success-fn & [fail-fn]]
    (chsk-send!
      [eventtype message] timeout
      (fn [reply]
        (if (sente/cb-success? reply)
          (success-fn reply)
          (if fail-fn (fail-fn)))))))

(defmulti -event-msg-handler
          "Multimethod to handle Sente `event-msg`s"
          :id ; Dispatch on event-id
          )

(defmethod -event-msg-handler
  :default ; Default/fallback case (no other matching handler)
  [{:as ev-msg :keys [id ?data ctx]}]
  (->output! "Nothing to do for event: " id))

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [app {:as ev-msg :keys [id ?data event]}]

  (-event-msg-handler (assoc ev-msg :ctx app)))


(defonce router_ (atom nil))

(defn stop-router! []
  (when-let [stop-f @router_] (stop-f)))

(defn start-router! [app]
  (stop-router!)
  (reset! router_
    (sente/start-client-chsk-router!
      ch-chsk (partial event-msg-handler app))))




(defmethod -event-msg-handler :chsk/state
  [{:as ev-msg :keys [?data]}]
  (if (= ?data {:first-open? true})
    (->output! "Channel socket successfully established!")
    (->output! "Channel socket state change: %s" ?data)))

(defmethod -event-msg-handler :chsk/recv
  [{:as ev-msg :keys [?data id]}]
  (->output! (str "Message recieved: " id))
  (-event-msg-handler (assoc ev-msg :id (first ?data) :?data (second ?data))))

(defmethod -event-msg-handler :chsk/handshake
  [{:as ev-msg :keys [?data]}]
  (let [[?uid ?csrf-token ?handshake-data] ?data]
    (->output! "Handshake: %s" ?data)))


(defmethod -event-msg-handler :kifu/updated
  [{:as ev-msg :keys [?data ctx]}]
  (->output! (str "Message data: " ?data))
  (swap! ctx assoc-in [:board :contents] ?data))

(defmethod -event-msg-handler :ogs/creds
  [{:as ev-msg :keys [?data ctx]}]
  (swap! ctx assoc-in [:ogs :creds]))





