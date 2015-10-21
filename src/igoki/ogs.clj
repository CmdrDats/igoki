(ns igoki.ogs
  (:require
    [clj-http.client :as client]))

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

(comment
  ;; Get clientid and secret by auth2 client here: https://online-go.com/developer
  ;; Get app password from user profile

  (def auth
    (ogs-auth
      {:client_id     ""
       :client_secret ""
       :username      ""
       :password      ""})
    ))