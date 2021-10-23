(ns igoki.ui.ogs
  (:require
    [seesaw.core :as s]
    [seesaw.mig :as sm]
    [seesaw.color :as sc]
    [igoki.ogs :as ogs]
    [igoki.ui.util :as ui.util]
    [igoki.util :as util])
  (:import
    (java.awt.event KeyEvent)
    (java.awt Graphics2D)
    (java.awt.geom Ellipse2D$Double)
    (javax.swing DefaultListCellRenderer)))

(defn paint-game-panel [ctx game c ^Graphics2D g]
  (.setColor g (sc/color "#dcb35c"))
  (let [[w h] [(.getWidth c) (.getHeight c)]
        board-width (:width game)
        ;; TODO: Need to do a few fixes for non-square boards....
        board-height (:height game)
        cellsize (/ h (+ board-width 2))
        grid-start (+ cellsize (/ cellsize 2))
        board-size (* cellsize (dec board-width))
        extent (+ grid-start board-size)
        tx (+ h (/ cellsize 2))

        constructed (:constructed game)
        kifu-board (get-in constructed [:kifu :kifu-board])
        ]
    (.fillRect g 0 0 w h)

    (.setColor g (sc/color :black))
    (doseq [x (range board-width)]
      (let [coord (+ grid-start (* x cellsize))]
        (.drawLine g coord grid-start coord extent)
        (.drawLine g grid-start coord extent coord)))

    ;; Draw star points
    (doseq [[x y] (util/star-points board-width)]
      (let [e (Ellipse2D$Double. (+ grid-start (- (* x cellsize) 1.5))
                (+ grid-start (- (* cellsize y) 1.5)) 4 4)]
        (.fill g e)))


    (doseq [y (range board-height)]
      (doseq [x (range board-width)]
        (let [stone (nth (nth kifu-board y) x)]
          (when stone
            (let [e (Ellipse2D$Double. (+ grid-start (- (* x cellsize) 4))
                      (+ grid-start (- (* cellsize y) 4)) 8 8)]
              (.setColor g (sc/color (if (= stone :w) :white :black)))
              (.fill g e)
              (when (= stone :w)
                (.setColor g (sc/color :black))
                (.draw g e)))))))))

(defn game-panel [ctx game selected?]
  (let [black (:black game)
        white (:white game)]
    (sm/mig-panel
      :background (if selected? :steelblue nil)
      :constraints [""]
      :items
      [[(s/canvas
          :size [200 :by 200]
          :paint (partial #'paint-game-panel ctx game)) "left, spany"]
       [(str (:username black) " [" (ogs/display-rank (:ranking black)) "]") "wrap, gapleft 10"]
       [(str (:username white) " [" (ogs/display-rank (:ranking white)) "]") "wrap, gapleft 10"]])))

(defn game-list-panel [ctx parent-panel]
  (let [setup-model
        #(map
           (fn [g]
            (assoc g :constructed
              (ogs/initialize-game {} (:json g))))
           (get-in @ctx [:ogs :overview :active_games]))

        gamelist
        (s/listbox
          :id :ogs-game-list
          :model (setup-model)
          :renderer
          (proxy [DefaultListCellRenderer] []
            (getListCellRendererComponent [component value index selected? foxus?]
              (game-panel ctx value selected?))))

        container
        (s/border-panel
          :south
          (s/flow-panel
            :align :center
            :items
            [(s/button :text "Refresh" :id :ogs-refresh)
             (s/button :text "Connect to selected" :id :ogs-connect-selected)])
          :center
          (s/scrollable gamelist
            :vscroll :always))]

    (s/listen
      (s/select container [:#ogs-connect-selected])
      :action
      (fn [e]
        (let [game (s/value gamelist)
              ogs (:ogs @ctx)]
          (ogs/connect-record ctx (:socket ogs)
            (str (:id game)) (:auth ogs)))))

    (s/listen
      (s/select container [:#ogs-refresh])
      :action
      (fn [e]
        (ogs/refresh-games ctx)
        (s/config!
          (s/select container [:#ogs-game-list])
          :model (setup-model))))

    container))

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
                 (s/replace! panel login-panel (game-list-panel ctx panel))))
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