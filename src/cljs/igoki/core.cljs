(ns igoki.core
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer (go go-loop)])
  (:require
    [reagent.core :as r]
    [reagent.session :as session]
    [reagent-forms.core :as forms]
    [igoki.sound :as snd]
    [igoki.comms :as comms]
    [igoki.state :as state]
    [secretary.core :as secretary :include-macros true]
    [accountant.core :as accountant]
    [igoki.xutil :as xu]
    [clojure.string :as string]
    [cljs-time.core :as time]
    [re-frame.core :as rf]
    [re-com.core :as rc]))

(enable-console-print!)

(def coords
  {:y ["一" "二" "三" "四" "五" "六" "七" "八" "九" "十"
       "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九"]
   :x [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19]})

(defonce app
  (r/atom {:selection "Library"
           :board     {:size 19
                       :coords true
                       :contents
                       (for [y (range 19)]
                         (for [x (range 19)]
                           nil))}
           :config    {:tab {:selected :info}}}))


;; -------------------------
;; Views

(defn game-card []
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

(defn sidebar []

  #_[:div.sidebar
   [:section.accordion
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

(defn newboard [size state]
  (fn [size state]
    (let [bowls? (> size 300)
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
          fyfn #(+ byoffset (+ byborder (* ystep %)))

          hover (r/atom nil)]
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
             :width bxsize :height bysize
             :fill "rgba(0,0,0,0.1)"}]]
          (concat
            (let [[x y] @hover]
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
                (= @hover [x y])
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
                 {:on-mouse-enter #(reset! hover [x y])
                  :key (str "board-stone-" x "-" y)
                  :x (- (fxfn x) (/ xstep 2)) :y (- (fyfn y) (/ ystep 2))
                  :width xstep :height xstep
                  :stroke "rgba(0,0,0,0)"
                  :stroke-width stroke
                  :opacity 0}])
              )))]])))


(defn board [state]
  (fn [state]
    (let [b (:board @state)
          stars (set (xu/star-points (count b)))
          hover (r/atom nil)]
      [:div.playarea {:style {:background-color "#31731A"}}

       #_[newboard 50 state]
       #_[newboard 200 state]
       #_[newboard 500 state]
       #_[newboard 800 state]

       [newboard (.-clientWidth (first (array-seq (.getElementsByTagName js/document "body")))) b]

       ])))

(defn radio [tag form form-id valuepath attrs]
  [tag
   (merge attrs
          {:type      :radio
           :checked   (= (get-in @form valuepath) (:value attrs))
           :on-change #(do
                        (rf/dispatch [:form/update form-id valuepath (:value attrs)])
                        ((or (:on-change attrs) (fn []))))})])

(defn game-info [config]
  [:article.config
   [rc/v-box
    :children
    [[rc/gap :size "15px"]
     [rc/box
      :child
      [:div.form-horizontal
       [:div.form-group
        [:label.col-sm-2.control-label {:for "info-title"} "Game Title"]
        [:div.col-sm-10
         [rc/input-text
          :width "490px"
          :attr {:id "info-title"}
          :model (or (-> @config :info :title) "")
          :on-change #(rf/dispatch [:form/update :config [:info :title] %])]]]

       [:div.form-group
        [:label.col-sm-2.control-label {:for "info-dateplayed"} "Date Played"]
        [:div.col-sm-10
         [rc/datepicker-dropdown
          :attr {:id "info-dateplayer"}
          :model (or (-> @config :info :dateplayed) (time/now))
          :on-change #(rf/dispatch [:form/update :config [:info :dateplayed] %])]]]

       [:div.form-group
        [:label.col-sm-2.control-label {:for "info-copyright"} "Copyright Notice"]
        [:div.col-sm-10
         [rc/input-text
          :attr {:id "info-copyright"}
          :model (or (-> @config :info :copyright) "")
          :on-change #(rf/dispatch [:form/update :config [:info :copyright] %])]]]

       [:div.form-group
        [:label.col-sm-2.control-label {:for "info-event"} "Event Detail"]
        [:div.col-sm-10
         [rc/input-text
          :attr {:id "info-event"}
          :width "490px"
          :model (or (-> @config :info :event) "")
          :on-change #(rf/dispatch [:form/update :config [:info :event] %])]]]

       [:div.form-group
        [:label.col-sm-2.control-label {:for "info-opening"} "Opening"]
        [:div.col-sm-10
         [rc/input-text
          :attr {:id "info-opening"}
          :model (or (-> @config :info :opening) "")
          :width "490px"
          :on-change #(rf/dispatch [:form/update :config [:info :opening] %])]]]

       [:div.form-group
        [:label.col-sm-2.control-label {:for "info-timing"} "Timing"]
        [:div.col-sm-4
         [rc/input-text
          :width "130"
          :attr {:id "info-timing"}
          :model (or (-> @config :info :timing) "")
          :on-change #(rf/dispatch [:form/update :config [:info :timing] %])]]

        [:label.col-sm-2.control-label {:for "info-komi"} "Komi"]
        [:div.col-sm-4
         [rc/input-text
          :width "50"
          :attr {:id "info-komi"}
          :model (or (-> @config :info :komi) "")
          :on-change #(rf/dispatch [:form/update :config [:info :komi] %])]]
        ]

       [:div.form-group
        [:label.col-sm-2.control-label {:for "info-comments"} "Comments"]
        [:div.col-sm-10
         [rc/input-textarea
          :width "500"
          :attr {:id "info-comments"}
          :model (or (-> @config :info :comments) "")
          :on-change #(rf/dispatch [:form/update :config [:info :comments] %])]]]

       [rc/h-box
        :children
        [[rc/box
          :size "auto"
          :child
          [:div
           [:div {:style {:margin-left "15px" :margin-bottom "10px"}} [:b "Black Player"]]
           [:div.col-sm-5
            [:div.avatar-frame
             [:img {:src "http://robohash.org/test1"}]]]
           [:div.col-sm-5
            [:div.form-group
             [:label.sr-only {:for "info-blackname"} "Name"]
             [rc/input-text
                :width "150"
                :attr {:id "info-blackname" :placeholder "Name"}
                :model (or (-> @config :info :black :name) "")
                :on-change #(rf/dispatch [:form/update :config [:info :black :name] %])]]
            [:div.form-group
             [:label.sr-only {:for "info-blackname"} "Rank"]
             [rc/input-text
              :width "75"
              :attr {:id "info-blackname" :placeholder "Rank"}
              :model (or (-> @config :info :black :rank) "")
              :on-change #(rf/dispatch [:form/update :config [:info :black :rank] %])]]]]
          ]
         [rc/line :size "1px" :color "silver" :style {:margin-left "3px" :margin-right "15px"}]
         [rc/box
          :size "auto"
          :child
          [:div
           [:div {:style {:margin-left "15px" :margin-bottom "10px"}} [:b "White Player"]]
           [rc/gap :size "15px"]
           [:div.col-sm-5
            [:div.avatar-frame
             [:img {:src "http://robohash.org/test2" :width "80px"}]]]
           [:div.col-sm-5
            [:div.form-group
             [:label.sr-only {:for "info-blackname"} "Name"]
             [rc/input-text
              :width "150"
              :attr {:id "info-blackname" :placeholder "Name"}
              :model (or (-> @config :info :white :name) "")
              :on-change #(rf/dispatch [:form/update :config [:info :white :name] %])]]
            [:div.form-group
             [:label.sr-only {:for "info-blackname"} "Rank"]
             [rc/input-text
              :width "75"
              :attr {:id "info-blackname" :placeholder "Rank"}
              :model (or (-> @config :info :white :rank) "")
              :on-change #(rf/dispatch [:form/update :config [:info :white :rank] %])]]]]
          ]]]
       ]]]]])

(defn game-camera [config cameralist]
  (let [dragger (r/atom nil)]
    (fn []
      [:article.config
       [rc/v-box
        :children
        [[rc/h-box
          :align :center
          :children
          [[rc/box
            :size "auto"
            :child
            [rc/horizontal-bar-tabs
             :tabs
             (for [{:keys [id] :as cam} @cameralist]
               {:label (str id) :id id})
             :on-change #(rf/dispatch [:camera/select %])
             :model (or (:selected-camera @config) 1)]]
           [rc/box
            :size "initial"
            :child
            [:button {:on-click #(rf/dispatch [:camera/new])} "New"]]]]
         (let [{:keys [corners] :as selected}
               (first (filter #(= (:id %) (or (:selected-camera @config) 1)) @cameralist))
               [c1 c2 c3 c4] corners]
           (println "SELECTED CAMERA" selected)
           [:div {:style {:position "relative"}}
            [:img {:src "/cap.png" :width 600 :position "absolute" :top 0 :left 0}]
            [:svg {:on-mouse-down #(reset! dragger [(.-clientX %) (.-clientY %)]) :style {:position "absolute" :top 0 :left 0 :width "100%" :height "100%"}
                   :on-mouse-move #(when (-> @dragger)
                                    (let [[dx dy] @dragger]
                                      (reset! dragger [(.-clientX %) (.-clientY %)]))
                                    (.log js/console %))
                   :on-mouse-up   #(when @dragger
                                    (reset! dragger nil)
                                    (rf/dispatch [:camera/corner-move-finish]))}
             [:defs
              [:pattern {:id "camera-zoom" :x 0 :y 0 :patternUnits "objectBoundingBox" :width 1 :height 1
                         :preserveAspectRatio "xMaxYmin meet"}
               [:image {:x -100 :y 0 :xlinkHref "/cap.png" :width 200 :height 200 :verticalAlign "top"
                        }]]]

             [:line {:x1    (str (first c1) "%")
                     :y1    (str (second c1) "%")
                     :x2    (str (first c2) "%")
                     :y2    (str (second c2) "%")
                     :style {:stroke "rgb(128,128,255)" :stroke-width 2}}]
             [:line {:x1    (str (first c2) "%")
                     :y1    (str (second c2) "%")
                     :x2    (str (first c3) "%")
                     :y2    (str (second c3) "%")
                     :style {:stroke "rgb(128,128,255)" :stroke-width 2}}]
             [:line {:x1    (str (first c3) "%")
                     :y1    (str (second c3) "%")
                     :x2    (str (first c4) "%")
                     :y2    (str (second c4) "%")
                     :style {:stroke "rgb(128,128,255)" :stroke-width 2}}]
             [:line {:x1    (str (first c4) "%")
                     :y1    (str (second c4) "%")
                     :x2    (str (first c1) "%")
                     :y2    (str (second c1) "%")
                     :style {:stroke "rgb(128,128,255)" :stroke-width 2}}]
             [:circle {:id "camera-zoom-circle" :cx 140 :cy 100 :r 80 :fill "url(#camera-zoom)"}]]])]]])))

(defn config-dialog []
  (let [config (r/cursor (rf/subscribe [:forms]) [:config])
        cameralist (rf/subscribe [:cameralist])]
    (fn []
      (when (:visible @config)
        [rc/modal-panel
         :backdrop-on-click #(rf/dispatch [:form/update :config [:visible] false])
         :child
         [rc/box
          :child
          [:div
           [rc/horizontal-tabs
            :tabs [{:label "Info" :id :info}
                   {:label "Online" :id :online}
                   {:label "Camera" :id :camera}]
            :on-change #(rf/dispatch [:form/update :config [:tab] %])
            :model (or (:tab @config) :info)]
           (case (:tab @config)
             :online [:article.config "online"]
             :camera
             [game-camera config cameralist]
             [game-info config]
             )]]
         ]))
    #_(fn []
      [(if (:visible @config) :div.modal.active :div.modal) {:on-click #(rf/dispatch [:form/update :config [:visible] false])}
       [:div.modalview {:on-click (fn [e] (.stopPropagation e))}
        (if (:visible @config)
          [rct/horizontal-bar-tabs
           :tabs [{:label "Info" :id :info}
                  {:label "Online" :id :online}
                  {:label "Camera" :id :camera}]
           :on-change (fn [_])
           :model (or (:tab @config) :info)]

          [:div.titlebuttons
           [:span
            (radio :input.tabradio config :config [:tab] {:value :info :id "config-info" :name :tab-selected})
            [:label.tab {:for "config-info"} "Info"]
            [:article.config "opened!"]]
           [:span
            (radio :input.tabradio config :config [:tab] {:value :online :id "config-online" :name :tab-selected})
            [:label.tab {:for "config-online"} "Online"]
            [:article.config "online"]]
           [:span
            (radio :input.tabradio config :config [:tab]
                   {:value :camera :id "config-camera" :name :tab-selected
                    :on-click #(rf/dispatch [:camera/list])})

            [:label.tab {:for "config-camera"} "Camera"]
            [:article.config
             (for [[id cam] @cameralist]
               [:span {:key (str "camera-span" id)}
                [:input {:type :radio :name :camera.selected
                         :value id
                         :id    (str "camera-selected-" id)}]
                [:label {:for (str "camera-selected-" id)} (str id)]])
             [:button {:on-click #()} "New"]
             [:img {:src "/cap.png" :width "100%" :height "100%"}]]]])]])))

(defn home-page []
  [:div.page
   [sidebar]
   [:div#output]
   [:div.maincontent
    [:img.logo
     {:src "/images/igoki96.png" :width 48}]
    [:div
     [:div {:style {:margin "15px" :height "60px"}}
      [:h2 {:style {:margin "10px":float "left"}} "Welcome to igoki"]
      [:div
       [rc/button :on-click #(comms/send :setup/webcam {}) :label "Webcam Frame"]
       [rc/button :on-click #(comms/send :setup/projector {}) :label "Projector Frame"]]
      [:div {:clear "all"}]]
     #_[:a {:href "/about"} "go to about page"]
     [board app]
     [:div.clear]
     [rc/button
      :on-click #(rf/dispatch [:form/update :config [:visible] true])
      :label "Game Config"]
     [config-dialog]

     #_[:img {:src "/cap.png"}]]]])

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
  (comms/start-router! app)
  (mount-root)
  (rf/dispatch [:init])
  )






