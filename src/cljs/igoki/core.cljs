(ns igoki.core
  (:require
    [cljsjs.material-ui]
    [cljs-react-material-ui.core :as uicore :refer [color]]
    [cljs-react-material-ui.icons :as ic]
    [cljs-react-material-ui.rum :as ui]
    [rum.core :as rum]
    [igoki.sound :as snd]
    [igoki.comms :as comms]
    [igoki.xutil :as xu]))

(enable-console-print!)

(def coords
  {:y (vec (reverse (map inc (range 19))))
   :x (vec (remove #(= "I" %) (map char (range 65 87))))})

(defonce app
  (atom {:selection "Library"
         :board {:size 19
                 :coords true
                 :contents
                 (for [y (range 19)]
                   (for [x (range 19)]
                     nil))}
         :config {:tab {:selected :info}}}))


;; -------------------------
;; Views

(rum/defc game-card []
  #_[:div.ui.card
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

(rum/defc sidebar []

  #_[:div.sidebar
   [:section.accordion
    [:div.title
     {:class    (if (= (:selection @app) "Library") "active")
      :onClick #(swap! app assoc :selection "Library")}
     [:i.dropdown.icon] "Library"]
    [:div.content
     {:class (if (= (:selection @app) "Library") "active")}
     [:p "A dog is a type of domesticated animal. Known for its loyalty and faithfulness, it can be found as a welcome guest in many households across the world."]]
    [:div.title
     {:class    (if (= (:selection @app) "Online") "active")
      :onClick #(swap! app assoc :selection "Online")}
     [:i.dropdown.icon] "Online Games"]
    [:div.content
     {:class (if (= (:selection @app) "Online") "active")}
     [:p "A dog is a type of domesticated animal. Known for its loyalty and faithfulness, it can be found as a welcome guest in many households across the world."]]
    [:div.title
     {:class    (if (= (:selection @app) "Active") "active")
      :onClick #(swap! app assoc :selection "Active")}
     [:i.dropdown.icon] "Active Games"]
    [:div.content
     {:class (if (= (:selection @app) "Active") "active")}
     [:p
      [game-card]
      [game-card]]]



    ]])

(defn block [x y]
  [:rect
   {:x x
    :y y
    :width 1
    :height 1
    :stroke "black"
    :stroke-width 0.01
    :rx 0.1
    :fill "#ddeeee"}])

(rum/defc newboard < rum/reactive [size state]
  ;; TODO: should use local state
  (let [hover (rum/react (rum/cursor app :hover))
        bowls? (> size 300)
        xsize size
        ysize (if bowls? (* size 0.70) size)
        xlayoutgrid (/ xsize 50)
        ylayoutgrid (/ ysize 50)

        bxoffset (* xlayoutgrid 10)
        byoffset (* ylayoutgrid 1.65)

        bxmax (* xlayoutgrid 40.3)
        bymax (* ylayoutgrid 48.7)

        bxsize (- bxmax bxoffset)
        bysize (- bymax byoffset)

        bxborder (* (/ bxsize (dec (:size state))) (if (:coords state) 1 0.55))
        byborder (* (/ bysize (dec (:size state))) (if (:coords state) 1 0.55))

        xstep (/ (- bxsize (* bxborder 2)) (dec (:size state)))
        ystep (/ (- bysize (* byborder 2)) (dec (:size state)))

        stars (set (xu/star-points (:size state)))
        stroke (max 0.1 (min (/ ysize 600) 1))

        cxfn #(+ 0.5 (int (+ bxoffset bxborder (* xstep %))))
        cyfn #(+ 0.5 (int (+ byoffset byborder (* ystep %))))
        fxfn #(+ bxoffset bxborder (* xstep %))
        fyfn #(+ byoffset (+ byborder (* ystep %)))]
    [:div.gobancontainer
     {:style
      {:width xsize :height ysize}}
     [:div.goban

      (into
        [:svg.gobansvg
         [:defs
          [:pattern {:id "image" :x 0 :y 0 :patternUnits "userSpaceOnUse" :height 100 :width 100}
           [:image {:x 0 :y 0 :height 100 :width 100 :xlinkHref "/images/calibration.png"}]]

          [:radialGradient {:id "white-radial" :cx "40%" :cy "40%" :r "80%" :fx "30%" :fy "30%"}
           [:stop {:offset "0%" :style {:stop-color "rgb(255,255,255)" :stop-opacity "1"}}]
           [:stop {:offset "100%" :style {:stop-color "#ecebeb" :stop-opacity "1"}}]]
          [:radialGradient {:id "black-radial" :cx "40%" :cy "40%" :r "80%" :fx "30%" :fy "30%"}
           [:stop {:offset "0%" :style {:stop-color "#444444" :stop-opacity "1"}}]
           [:stop {:offset "100%" :style {:stop-color "#111111" :stop-opacity "1"}}]]
          [:radialGradient {:id "shadow-radial" :cx "50%" :cy "50%" :r "100%" :fx "50%" :fy "50%"}
           [:stop {:offset "30%" :style {:stop-color "black" :stop-opacity "0.5"}}]
           [:stop {:offset "100%" :style {:stop-color "black" :stop-opacity "0"}}]]
          ;; TODO: Add stone shadows when react is upgraded in cljsjs
          #_[:filter {:id "f2" :x 0 :y 0 :width "200%" :height "200%"}
             [:feOffset {:result "offOut" :in "SourceGraphic" :dx 20 :dy 20}]
             [:feGaussianBlur {:result "blurOut" :in "offOut" :stdDeviation 10}]
             [:blend {:in "SourceGraphic" :in2 "blurOut" :mode "normal"}]]]
         #_[:ellipse {:cx 200 :cy 70 :rx 55 :ry 55 :fill "url(#white-radial)"
                      :stroke-width stroke :stroke "black"}]
         #_[:circle {:id "top" :cx 100 :cy 100 :r 80 :fill "url(#image)"}]


         [:image {:x (* xlayoutgrid 0) :y (* ylayoutgrid 1) :width (* xlayoutgrid 12) :height (* ylayoutgrid 25) :xlinkHref "/images/wbowl.svg"}]
         [:image {:x (* xlayoutgrid 35) :y (* ylayoutgrid 17) :width (* xlayoutgrid 20) :height (* ylayoutgrid 40) :xlinkHref "/images/bbowl.svg"}]
         [:image {:x (* xlayoutgrid 6) :y (* ylayoutgrid 1) :width (* xlayoutgrid 39) :height (* ylayoutgrid 48) :xlinkHref "/images/board.svg"}]
         [:rect
          {:x bxoffset :y byoffset
           :width bxsize :height (+ bysize 1)
           :fill "rgba(0,0,0,0.1)"}]]
        (concat
          (let [[x y] hover]
            [[:line
              {:key (str "board-hoverlinex-" x)
               :x1 (cxfn x) :y1 (+ 0.5 (int (+ byoffset byborder)))
               :x2 (cxfn x) :y2 (+ 0.5 (int (+ byoffset (- bysize byborder))))
               :opacity 0.6
               :style {:stroke "rgb(128,128,255)" :stroke-width (* stroke 4)}}]
             [:line
              {:key (str "board-hoverliney-" y)
               :x1 (+ 0.5 (int (+ bxoffset bxborder))) :y1 (cyfn y)
               :x2 (+ 0.5 (int (+ bxoffset (- bxsize bxborder)))) :y2 (cyfn y)
               :opacity 0.6
               :style {:stroke "rgb(128,128,255)" :stroke-width (* stroke 4)}}]])
          (if (:coords state)
            (for [x (range (:size state))]
              [:text {:x (cxfn x) :y (+ byoffset (* ystep 0.6)) :font-size (* 16 stroke) :fill "black" :text-anchor "middle"} (get-in coords [:x x])]))
          (if (:coords state)
            (for [y (range (:size state))]
              [:text {:x (+ bxoffset (* xstep 0.3)) :y (+ (cyfn y) (* ystep 0.1)) :font-size (* 16 stroke) :fill "black" :text-anchor "middle"} (get-in coords [:y y])]))
          (for [x (range (:size state))]
            [:line
             {:key (str "board-linex-" x)
              :x1 (cxfn x) :y1 (+ 0.5 (int (+ byoffset byborder)))
              :x2 (cxfn x) :y2 (+ 0.5 (int (+ byoffset (- bysize byborder))))
              :style {:stroke "rgb(0,0,0)" :stroke-width stroke}}])

          (for [y (range (:size state))]
            [:line
             {:key (str "board-liney-" y)
              :x1 (+ 0.5 (int (+ bxoffset bxborder))) :y1 (cyfn y)
              :x2 (+ 0.5 (int (+ bxoffset (- bxsize bxborder)))) :y2 (cyfn y)
              :style {:stroke "rgb(0,0,0)" :stroke-width stroke}}])
          (for [[x y] stars]
            [:circle
             {:key (str "board-star-" x "-" y)
              :cx (cxfn x)
              :cy (cyfn y)
              :fill "black"
              :stroke "black"
              :stroke-width 0
              :r (* stroke 3)}])

          (for [[y row] (map-indexed vector (:contents state))
                [x cell] (map-indexed vector row)]
            (if
              (or (= cell :b) (= cell :w))
              [:ellipse
               {:key (str "board-stone-shadow-" x "-" y)
                :cx (+ (fxfn x) (* 3 stroke)) :cy (+ (fyfn y) (* 3 stroke))
                :rx (- (/ xstep 2) stroke) :ry (- (/ xstep 2) stroke)
                :stroke-width 0
                :opacity 0.5
                :fill "url(#shadow-radial)"}]))
          (for [[y row] (map-indexed vector (:contents state))
                [x cell] (map-indexed vector row)]
            (cond
              (= cell :b)
              [:ellipse
               {:key (str "board-stone-" x "-" y)
                :cx (fxfn x) :cy (fyfn y)
                :rx (- (/ xstep 2) stroke) :ry (- (/ xstep 2) stroke)
                :stroke "rgba(68,68,68,1)"
                :stroke-width stroke
                :fill "url(#black-radial)"}]

              (= cell :w)
              [:ellipse
               {:key (str "board-stone-" x "-" y)
                :cx (fxfn x) :cy (fyfn y)
                :rx (- (/ xstep 2) stroke) :ry (- (/ xstep 2) stroke)
                :stroke "rgba(128,128,128,1)"
                :stroke-width stroke
                :fill "url(#white-radial)"}]
              (= hover [x y])
              [:ellipse
               {:key (str "board-stone-" x "-" y)
                :cx (fxfn x) :cy (fyfn y)
                :rx (- (/ xstep 2) stroke) :ry (- (/ xstep 2) stroke)
                :stroke "rgba(0,0,0,0.3)"
                :stroke-width stroke
                :opacity 0.5
                :fill "url(#white-radial)"}]
              :else
              [:rect
               {:on-mouse-enter #(swap! app assoc :hover [x y])
                :key (str "board-stone-" x "-" y)
                :x (- (fxfn x) (/ xstep 2)) :y (- (fyfn y) (/ ystep 2))
                :width xstep :height xstep
                :stroke "rgba(0,0,0,0)"
                :stroke-width stroke
                :opacity 0}])
            )))]]))


(rum/defc board < rum/reactive [state]
  (let [b (:board (rum/react state))
        stars (set (xu/star-points (count b)))]
    [:div.playarea {:style {:background-color "#31731A"}}

     #_[newboard 50 state]
     #_[newboard 200 state]
     #_[newboard 500 state]
     #_[newboard 800 state]

     (newboard (min
                 800
                 (.-clientWidth (first (array-seq (.getElementsByTagName js/document "body"))))) b)

     ]))

(rum/defc input-text < rum/reactive [atm ks props]
  (let [drv (rum/cursor-in atm ks)]
    [:input
     {:type "text"
      :value (or (rum/react drv) "")
      :on-change (fn [e] (swap! atm assoc-in ks (.. e -target -value)))}]))

(rum/defc input-textarea < rum/reactive [atm ks props]
  (let [drv (rum/cursor-in atm ks)]
    [:textarea
     {:type "text"
      :on-change (fn [e] (swap! atm assoc-in ks (.. e -target -value)))}
     (rum/react drv)]))

(rum/defc game-info [config]
  [:div.form-horizontal
   [:div.form-group
    [:label.col-sm-2.control-label {:for "info-title"} "Game Title"]
    [:div.col-sm-10
     (input-text config [:info :title]
       {:width "490px"})]]

   [:div.form-group
    [:label.col-sm-2.control-label {:for "info-dateplayed"} "Date Played"]
    [:div.col-sm-10
     (input-text config [:info :dateplayed] {})]]

   [:div.form-group
    [:label.col-sm-2.control-label {:for "info-copyright"} "Copyright Notice"]
    [:div.col-sm-10
     (input-text config [:info :copyright] {})]]

   [:div.form-group
    [:label.col-sm-2.control-label {:for "info-event"} "Event Detail"]
    [:div.col-sm-10
     (input-text config [:info :event] {:width "490px"})]]

   [:div.form-group
    [:label.col-sm-2.control-label {:for "info-opening"} "Opening"]
    [:div.col-sm-10
     (input-text config [:info :opening] {:width "490px"})]]

   [:div.form-group
    [:label.col-sm-2.control-label {:for "info-timing"} "Timing"]
    [:div.col-sm-4
     (input-text config [:info :timing] {:width "130"})
     ]

    [:label.col-sm-2.control-label {:for "info-komi"} "Komi"]
    [:div.col-sm-4
     (input-text config [:info :komi] {:width "50"})]]

   [:div.form-group
    [:label.col-sm-2.control-label {:for "info-comments"} "Comments"]
    [:div.col-sm-10
     (input-textarea config [:info :comments]
       {:width "500"})]]

   [:div
    [:div
     {:size "auto"}
     [:div
      [:div {:style {:margin-left "15px" :margin-bottom "10px"}} [:b "Black Player"]]
      [:div.col-sm-5
       [:div.avatar-frame
        [:img {:src "http://robohash.org/test1"}]]]
      [:div.col-sm-5
       [:div.form-group
        [:label.sr-only {:for "info-blackname"} "Name"]
        (input-text config [:info :black :name] {:width "150" :placeholder "Name"})]
       [:div.form-group
        [:label.sr-only {:for "info-blackname"} "Rank"]
        (input-text config [:info :black :rank] {:width "75" :placeholder "Rank"})]]]
     ]
    [:hr]

    [:div
     {:size "auto"}
     [:div
      [:div {:style {:margin-left "15px" :margin-bottom "10px"}} [:b "White Player"]]
      [:div.col-sm-5
       [:div.avatar-frame
        [:img {:src "http://robohash.org/test2" :width "80px"}]]]
      [:div.col-sm-5
       [:div.form-group
        [:label.sr-only {:for "info-blackname"} "Name"]
        (input-text config [:info :white :name] {:width "150" :placeholder "Name"})]
       [:div.form-group
        [:label.sr-only {:for "info-blackname"} "Rank"]
        (input-text config [:info :white :rank] {:width "75" :placeholder "Rank"})]]]
     ]]
   ])


(defn ogs-config [config]
  (ui/divider)
  )

(rum/defc config-dialog < rum/reactive [app]
  (let [config-atm (rum/cursor app :config)
        config (rum/react config-atm)]



    (ui/dialog
      {:title "Config"
       :open (:visible config)
       :modal false
       :onRequestClose #(swap! config-atm assoc :visible false)}
      (ui/tabs
        (ui/tab {:label "Info"}
          (game-info config-atm))
        (ui/tab {:label "Online"}
          (ogs-config (rum/cursor app :ogs)))))))

(rum/defc home-page < rum/reactive [app]
  (ui/mui-theme-provider
    {:mui-theme (uicore/get-mui-theme) }
    [:div.page
     (sidebar)
     [:div.maincontent
      [:div.header
       [:img.logo
        {:src "/images/igoki96.png" :width 48}]


       [:h2 {:style {:margin "10px" :float "left"}} "Welcome to igoki"]
       [:div
        [:button {:onClick #(comms/send :setup/webcam {})}
         "Webcam Setup"]
        [:button {:onClick #(comms/send :setup/projector {})}
         "Projector Setup"]
        [:button {:onClick #(do (swap! app assoc-in [:config :visible] true) nil)}
         "Game Config"]
        ]
       [:div {:style {:clear "all"}}]]
      (board app)
      [:div.clear]

      (config-dialog app)

      #_[:img {:src "/cap.png"}]]]))

(rum/defc about-page []
  [:div [:h2 "About igoki"]
   [:div [:a {:href "/"} "go to the home page"]]])

;; -------------------------
;; Initialize app



(defn mount-root []
  (rum/mount (home-page app) (.getElementById js/document "app")))


(defn init! []
  (mount-root)
  (comms/start-router! app)
  )

(set!
  (.-onload js/window)
  (fn [& _] (init!)))

