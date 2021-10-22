(ns igoki.ui.main
  (:require
    [seesaw.core :as s]
    [igoki.ui.game :as ui.game]
    [igoki.ui.calibration :as calibration]
    [igoki.ui.ogs :as ogs]
    [igoki.projector :as projector]
    [igoki.game :as game]
    [igoki.simulated :as sim]
    [igoki.camera :as camera]))

(s/native!)

(defn file-component []
  nil
  #_(s/listbox
    :model
    ["File 1"
     "File 2"
     "File 3"]))

(defn button-bar [ctx]
  (s/flow-panel
    :hgap 15
    :align :left
    :items
    [(s/button :text "Projector Window"
       :listen
       [:action
        (fn [e]
          (projector/start-cframe ctx))])
     (s/toggle :text "Dev Tools")
     (s/toggle :text "Show Branches"
       :listen
       [:action
        (fn [e]
          (game/toggle-branches ctx (s/value (.getSource e))))])]))

(defn ogs-panel [ctx]
  (s/tabbed-panel
    :placement :bottom
    :overflow :scroll
    :tabs
    [{:title "OGS"
      :tip "Online-go.com integration"
      :content (ogs/ogs-panel ctx)}
     {:title "Simulation"
      :tip "Simulation (dev tools)"
      :content
      (sim/simulation-panel ctx)}]))

(defn tree-panel [ctx]
  (s/tabbed-panel
    :placement :bottom
    :overflow :scroll
    :tabs
    [{:title "Tree"
      :tip "SGF Move tree"
      :content "Move tree"}
     {:title "Log"
      :tip "Output log (dev tools)"
      :content "output log"}]))

(defn primary-splits [ctx]
  (let [cl
        (s/top-bottom-split
          (calibration/calibration-panel ctx)
          (ogs-panel ctx)
          :border 0
          :resize-weight 0.5
          :divider-location 0.5)

        gt
        (s/top-bottom-split
          (ui.game/game-panel ctx)
          (tree-panel ctx)
          :border 0
          :resize-weight 0.5
          :divider-location 0.5)]
    (s/left-right-split
      cl gt
      :resize-weight 0.5
      :divider-location 0.5)))



(defn main-menu [ctx]
  (s/menubar
    :items
    [(s/menu :text "File"
       :items
       [(s/action
          :mnemonic \n
          :name "New SGF..."
          :key "menu N"
          :handler
          (fn [e]
            (when
              (s/confirm "Reset to new SGF recording, are you sure?"
                :title "New SGF"
                :type :warning
                :option-type :yes-no)
              (game/reset-kifu ctx))))
        (s/action
          :mnemonic \o
          :name "Open SGF"
          :key "menu O"
          :handler
          (fn [e]
            (ui.game/load-sgf ctx)))

        (s/action
          :mnemonic \s
          :name "Save SGF"
          :key "menu S"
          :handler
          (fn [e]
            (ui.game/export-sgf ctx)))


        :separator
        (s/action
          :mnemonic \x
          :name "Exit"
          :handler
          (fn [e]
            (when
              (s/confirm "Exiting, are you sure?"
                :title "Exit"
                :type :warning
                :option-type :yes-no)
              (camera/stop-read-loop ctx)
              (System/exit 0))))])]))

(defn frame-content [ctx]
  (let [b
        (s/border-panel
          :north (button-bar ctx)
          :center (primary-splits ctx))]
    #_(ogs/ogs-panel ctx)
    (s/left-right-split
      (file-component)
      b
      :resize-weight 0
      :divider-location 0.1)))

(defonce app-frame (atom nil))
(defn main-frame [ctx]
  (let [frame
        (s/frame
          :icon "igoki48.png"
          :title "igoki"
          :size [1024 :by 768]
          :menubar (main-menu ctx)
          :on-close :exit)]
    (-> frame s/show!)
    (s/config! frame :content (frame-content ctx))
    (reset! app-frame frame))
  #_(open
    {:title "igoki"
     :body
     [:button "Push me"]}))

(defn refresh [ctx]
  (s/config! @app-frame :menubar
    (main-menu ctx)
    :content (frame-content ctx)))