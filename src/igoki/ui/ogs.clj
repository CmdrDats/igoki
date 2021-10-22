(ns igoki.ui.ogs
  (:require
    [seesaw.core :as s]
    [seesaw.font :as f]
    [seesaw.mig :as sm]
    [seesaw.graphics :as sg]
    [seesaw.color :as sc]
    [igoki.ogs :as ogs]
    [igoki.ui.util :as ui.util]
    [igoki.util :as util])
  (:import
    (java.awt.event KeyEvent)
    (java.awt Graphics2D)
    (java.awt.geom Ellipse2D$Double)))

(defn paint-game-panel [ctx game c ^Graphics2D g]
  (.setColor g (sc/color "#dcb35c"))
  (let [[w h] [(.getWidth c) (.getHeight c)]
        board-width (:width game)
        board-height (:height game)
        cellsize (/ h (+ board-width 2))
        grid-start (+ cellsize (/ cellsize 2))
        board-size (* cellsize (dec board-width))
        extent (+ grid-start board-size)
        tx (+ h (/ cellsize 2))]
    (.fillRect g 0 0 w h)

    (.setColor g (sc/color :black))
    (doseq [x (range board-width)]
      (let [coord (+ grid-start (* x cellsize))]
        (.drawLine g coord grid-start coord extent)
        (.drawLine g grid-start coord extent coord)))

    ;; Draw star points
    (doseq [[x y] (util/star-points board-width)]
      (let [e (Ellipse2D$Double. (+ grid-start (- (* x cellsize) 2.5))
                (+ grid-start (- (* cellsize y) 2.5)) 5 5)]
        (.fill g e)))))

(defn game-panel [ctx game]
  (let [black (:black game)
        white (:white game)]
    (sm/mig-panel
      :constraints ["center"]
      :items
      [[(str (:username black) " [" (ogs/display-rank (:ranking black)) "]") "wrap, left, gapbottom 10"]
       [(s/canvas
          :size [200 :by 200]
          :paint (partial #'paint-game-panel ctx game)) "wrap"]
       [(str (:username white) " [" (ogs/display-rank (:ranking white)) "]") "wrap, right, gaptop 10"]])))

(defn game-list-panel [ctx]
  (s/scrollable
    (sm/mig-panel
      :constraints ["center" "" ""]
      :items
      (map
        (fn [g] [(game-panel ctx g) "wrap"])
        (get-in @ctx [:ogs :overview :active_games])))
    :vscroll :always
    :hscroll :never))

(defn ogs-login-panel [ctx]
  (let [settings (ogs/load-settings)
        login-panel
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

        panel (s/border-panel :id :ogs-panel :center login-panel)
        login
        (fn []
          (doto
            (Thread.
              #(let [result
                     (ogs/connect ctx (s/value login-panel)
                       (fn [e]
                         (s/config! (s/select login-panel [:#progress])
                           :text (str e))))]
                 (s/config! (s/select login-panel [:#progress])
                   :text
                   (if (:success result)
                     (str "Connected.")
                     (:message result)))
                 (s/remove! panel login-panel)
                 (s/config! panel :center (game-list-panel ctx))))
            (.setDaemon true)
            (.start)))]
    (s/value! login-panel settings)

    ;; Form listening for submit.
    (s/listen
      (s/select login-panel [:#open])
      :action
      (fn [e]
        (ui.util/open "https://online-go.com/oauth2/applications/")))

    (s/listen
      (concat
        (s/select login-panel [:JPasswordField])
        (s/select login-panel [:JTextField]))
      :key-released
      (fn [e]
        (when (= KeyEvent/VK_ENTER (.getKeyCode e))
          (login))))

    (s/listen
      (s/select login-panel [:#save])
      :action (fn [e] (login)))
    panel))



(defn ogs-panel [ctx]
  (ogs-login-panel ctx))