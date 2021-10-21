(ns igoki.ui.ogs
  (:require
    [seesaw.core :as s]
    [seesaw.font :as f]
    [seesaw.mig :as sm]
    [igoki.ogs :as ogs])
  (:import
    (java.awt.event KeyEvent)
    (java.awt Desktop)
    (java.net URI)))

(defn ogs-login-panel [ctx]
  (let [settings (ogs/load-settings)
        panel
        (sm/mig-panel
          :constraints ["center" "" ""]
          :items
          [["Online Go Bot Credentials" "span, center, gapbottom 15"]
           ["Generate API details:" "align label"]
           [(s/button :text "Open Browser" :id :open) "wrap"]
           ["Client ID: " "align label"]
           [(s/text :id :client-id :columns 32) "wrap"]
           ["Client Secret: " "align label"]
           [(s/password :id :client-secret :columns 24) "wrap"]
           ["Client Type: " "align label"]
           ["Secret" "wrap"]
           ["Authorization Grant Type: " "align label"]
           ["Password" "wrap"]
           ["" "span, center, gapbottom 15"]
           ["User login credentials" "span, center, gapbottom 15"]
           ["Username: " "align label"]
           [(s/text :id :username :columns 24) "wrap"]
           ["Password: " "align label"]
           [(s/password :id :password :columns 20) "wrap"]
           ["" "align label"]
           [(s/checkbox :id :remember :text "Remember password") "wrap"]
           [(s/label :id :progress :text "Progress")]
           [(s/button :text "Connect" :id :save) "tag ok, span, split 3, sizegroup bttn, gaptop 15"]])

        login
        (fn []
          (let [result
                (ogs/connect ctx (s/value panel)
                  (fn [e]
                    (s/config! (s/select panel [:#progress])
                      :text (str e))))]
            (s/config! (s/select panel [:#progress])
              :text
              (if (:success result)
                (str "Connected, "
                  (count (get-in @ctx [:ogs :overview :active_games]))
                  " games active.")
                (:message result)))))
        ]
    (s/value! panel settings)

    ;; Form listening for submit.
    (s/listen
      (s/select panel [:#open])
      :action
      (fn [e]
        (.browse
          (Desktop/getDesktop)
          (URI. "https://online-go.com/oauth2/applications/"))))

    (s/listen
      (concat
        (s/select panel [:JPasswordField])
        (s/select panel [:JTextField]))
      :key-released
      (fn [e]
        (when (= KeyEvent/VK_ENTER (.getKeyCode e))
          (login))))

    (s/listen
      (s/select panel [:#save])
      :action (fn [e] (login)))
    panel))



(defn ogs-panel [ctx]
  (ogs-login-panel ctx))