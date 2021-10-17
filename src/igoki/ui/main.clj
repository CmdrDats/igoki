(ns igoki.ui.main
  (:require
    [seesaw.core :as s]
    [seesaw.mig :as mig]
    [igoki.ui :as ui]
    [igoki.calibration :as goban]
    [igoki.game :as game]))

(s/native!)

(defn file-component []
  (s/listbox
    :model
    ["File 1"
     "File 2"
     "File 3"]))

(defn button-bar []
  (s/flow-panel
    :hgap 15
    :align :left
    :items
    [(s/button :text "Projector Window")
     (s/toggle :text "Dev Tools")]))

(defn calibrate-panel []
  (goban/calibration-panel ui/ctx))

(defn ogs-panel []
  (s/tabbed-panel
    :placement :bottom
    :overflow :scroll
    :tabs
    [{:title "OGS"
      :tip "Online-go.com integration"
      :content "OGS"}
     {:title "Simulation"
      :tip "Simulation (dev tools)"
      :content "Simulation"}]))

(defn game-panel []
  (game/game-panel ui/ctx))

(defn tree-panel []
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

(defn primary-splits []
  (let [cl
        (s/top-bottom-split
          (calibrate-panel)
          (ogs-panel)
          :border 0
          :resize-weight 0.5
          :divider-location 0.5)

        gt
        (s/top-bottom-split
          (game-panel)
          (tree-panel)
          :border 0
          :resize-weight 0.5
          :divider-location 0.5)]
    (s/left-right-split
      cl gt
      :resize-weight 0.5
      :divider-location 0.5)))



(defn main-menu []
  (s/menubar
    :items
    [(s/menu :text "File"
       :items
       [(s/menu-item :text "New SGF..." :mnemonic \n)
        (s/menu-item :text "Open SGF" :mnemonic \o)
        (s/menu-item :text "Save" :mnemonic \s)
        :separator
        (s/menu-item :text "Exit" :mnemonic \x)])]))

(defn frame-content []
  (let [b
        (s/border-panel
          :north (button-bar)
          :center (primary-splits))]
    (s/left-right-split
      (file-component)
      b
      :resize-weight 0
      :divider-location 0.1)))

(defonce app-frame (atom nil))
(defn main-frame []
  (let [frame
        (s/frame
          :title "igoki"
          :size [1024 :by 768]
          :menubar (main-menu)
          :on-close :exit)]
    (-> frame s/show!)
    (s/config! frame :content (frame-content))
    (reset! app-frame frame))
  #_(open
    {:title "igoki"
     :body
     [:button "Push me"]}))