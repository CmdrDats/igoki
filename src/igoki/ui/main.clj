(ns igoki.ui.main
  (:require
    [seesaw.core :as s]
    [igoki.ui.game :as ui.game]
    [igoki.ui.calibration :as calibration]
    [igoki.ui.ogs :as ogs]
    [igoki.ui.tree :as tree]
    [igoki.game :as game]
    [igoki.simulated :as sim]
    [igoki.camera :as camera])
  (:import (javax.swing JFrame)))

(s/native!)

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
      :content
      (tree/tree-panel ctx)}
     #_{:title "Log"
      :tip "Output log (dev tools)"
      :content
      (logging/log-panel ctx)
      }]))

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
  (s/border-panel
    :center (primary-splits ctx)))

(defonce app-frame (atom nil))
(defn main-frame [ctx]
  (let [frame
        (s/frame
          :icon "igoki48.png"
          :title "igoki"
          :size [1024 :by 768]
          :menubar (main-menu ctx)
          :on-close :exit)]
    (.setExtendedState frame JFrame/MAXIMIZED_BOTH)
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