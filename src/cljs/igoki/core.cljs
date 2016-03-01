(ns igoki.core
  (:require [reagent.core :as r]
            [reagent.session :as session]
            [igoki.sound :as snd]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [igoki.xutil :as xu]
            [clojure.string :as string]))

(defonce app
  (r/atom {:selection "Library"
           :board     (for [y (range 19)]
                        (for [x (range 19)]
                          (case (int (rand 4))
                            1 :b
                            2 :w
                            nil)))}))

;; -------------------------
;; Views

(defn game-card []
  [:div.ui.card
   [:div.content
    [:i.right.floated.like.icon]
    [:i.right.floated.star.icon]
    [:div.header "Game 1"]
    [:div.description [:p "Good morning"]
     [:div.ui.labeled.button {:tabindex 0}
      [:div.ui.red.button [:i.heart.icon] "Like"]
      [:a.ui.basic.label "2,048"]]]]
   [:div.extra.content
    [:div.left.floated.author
       [:img.ui.avatar.image {:src "//robohash.org/alphago"}]
       "Alpha Go"]
    [:div.right.floated.author
     "vs. " [:img.ui.avatar.image {:src "//robohash.org/leesedol"}]
     "Lee Sedol"]]])

(defn sidebar []
  [:div.sidebar
   #_[:section.accordion
    [:div.title
     {:class    (if (= (:selection @app) "Library") "active")
      :on-click #(swap! app assoc :selection "Library")}
     [:i.dropdown.icon] "Library"]
    [:div.content
     {:class (if (= (:selection @app) "Library") "active")}
     [:p "A dog is a type of domesticated animal. Known for its loyalty and faithfulness, it can be found as a welcome guest in many households across the world."]]
    [:div.title
     {:class    (if (= (:selection @app) "Online") "active")
      :on-click #(swap! app assoc :selection "Online")}
     [:i.dropdown.icon] "Online Games"]
    [:div.content
     {:class (if (= (:selection @app) "Online") "active")}
     [:p "A dog is a type of domesticated animal. Known for its loyalty and faithfulness, it can be found as a welcome guest in many households across the world."]]
    [:div.title
     {:class    (if (= (:selection @app) "Active") "active")
      :on-click #(swap! app assoc :selection "Active")}
     [:i.dropdown.icon] "Active Games"]
    [:div.content
     {:class (if (= (:selection @app) "Active") "active")}
     [:p
      [game-card]
      [game-card]]]



    ]])

(defn block [x y]
  [:rect {:x x
          :y y
          :width 1
          :height 1
          :stroke "black"
          :stroke-width 0.01
          :rx 0.1
          :fill "#ddeeee"}])

(defn board [state]
  (let [stars (set (xu/star-points (count state)))
        hover (r/atom nil)]
    (fn []
      [:div.boardcontainer
       [:div.board
        [:div.inner
         [:table
          [:tbody
           (for [[y row] (butlast (map-indexed vector state))]
             [:tr {:key (str "board-linesrow-" y)}
              (for [[x cell] (butlast (map-indexed vector row))]
                [:td {:key (str "board-lines-" x "-" y) :data-x x :data-y y}])])]]]

        [:div.matrix
         [:table
          (into
            [:tbody]
            (for [[y row] (map-indexed vector state)]
              (into
                [:tr {:key (str "board-row-" y)}]
                (for [[x cell] (map-indexed vector row)]
                  (into
                    [:td {:key            (str "boardpos-" x "-" y)
                          :data-x         x :data-y y
                          :on-mouse-enter (fn [_] (reset! hover [x y]))}]
                    [(if (stars [x y]) [:div.starpoint])
                     (cond
                       (= cell :b) [:div.stone.black]
                       (= cell :w) [:div.stone.white]
                       (= @hover [x y]) [:div.stone.white.ghost])])))))]]]])))

(defn config-dialog [config]
  [(if (:visible config) :div.modal.active :div.modal) {:on-click #(swap! app assoc-in [:config :visible] false)}
   [:div.modalview
    (if (:visible config)
      [:div.titlebuttons
       [:span
        [:input.tabradio {:type      "radio" :name "config-tabs" :id "config-info"
                          :on-change #(.log js/console "Info")}]
        [:label.tab {:for "config-info"} "Info"]
        [:article.config "opened!"]]
       [:span
        [:input.tabradio {:type "radio" :name "config-tabs" :id "config-online"}]
        [:label.tab {:for "config-online"} "Online"]
        [:article.config "online"]]
       [:span
        [:input.tabradio {:type "radio" :name "config-tabs" :id "config-camera"}]
        [:label.tab {:for "config-camera"} "Camera"]
        [:article.config
         [:img {:src "/cap.png" :width "100%" :height "100%"}]]]])]

   ])

(defn home-page []
  [:div.page

   [sidebar]

   [:div.maincontent
    [:div [:h2 "Welcome to igoki"]
     [:div [:a {:href "/about"} "go to about page"]
      [board (:board @app)]
      [:div.clear]
      [:button {:on-click #(swap! app update-in [:config :visible] (fn [i] (not i)))} "Game Config"]
      [config-dialog (:config @app)]

      #_[:img {:src "/cap.png"}]]]]])

(defn about-page []
  [:div [:h2 "About igoki"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [(session/get :current-page)])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (mount-root))






