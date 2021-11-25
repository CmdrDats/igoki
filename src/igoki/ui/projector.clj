(ns igoki.ui.projector
  (:require [seesaw.core :as s]
            [igoki.projector :as projector]
            [seesaw.mig :as sm]))

(defn refresh-button-states [ctx container]
  (let [{:keys [sketch calibrate?] :as s} @projector/proj-ctx
        _ (println "proj-ctx" s)
        {:keys [frame] :as f} (when sketch @sketch)
        _ (println "sketch" f)
        _ (println "frame" frame)
        {:keys [robot]} @ctx
        states
        [[:#projector-open-button (not frame)]
         [:#projector-calibration-grid (and frame (not calibrate?))]
         [:#projector-calibration-accept (and frame calibrate?)]
         [:#projector-close-button frame]]]

    (println "STATES" states)
    (doseq [[id state] states]
      (println "Setting " (s/select container [id])
        "to " state)
      ((if state s/show! s/hide!)
       (s/select container [id])))))

(defn projector-panel [ctx]
  (let [container (s/border-panel)
        refresh
        #(refresh-button-states ctx container)]
    (s/config! container :center
      (s/border-panel
        :north
        (sm/mig-panel
          :constraints ["center"]
          :items
          [[(s/button
              :id :projector-open-button
              :text "Projector Window"
              :listen
              [:action
               (fn [e]
                 (projector/start-cframe ctx refresh)
                 (refresh))]) "wrap"]
           [(s/button
              :id :projector-calibration-grid
              :text "Calibration Grid"
              :listen
              [:action
               (fn [e]
                 (projector/show-calibration)
                 (refresh))]) "wrap"]

           [(s/button
              :id :projector-calibration-accept
              :text "Accept Calibration"
              :listen
              [:action
               (fn [e]
                 (projector/accept-calibration ctx)
                 (refresh))]) "wrap"]

           [(s/button
              :id :projector-close-button
              :text "Close capture frame"
              :listen
              [:action
               (fn [e]
                 (projector/stop-cframe ctx refresh))]) "wrap"]])

        #_#_:center
            (game-setup-panel ctx container)

        #_#_:south
        (s/flow-panel
          :items
          [(s/button
             :id :robot-start-capture
             :text "Start Recording"
             :visible? false
             :listen
             [:action
              (fn [e] (robot-start-capture ctx container))])

           (s/button
             :id :robot-pause-capture
             :text "Pause Recording"
             :visible? false
             :listen
             [:action
              (fn [e] (robot-pause-capture ctx container))])

           (s/button
             :id :robot-unpause-capture
             :text "Unpause Recording"
             :visible? false
             :listen
             [:action
              (fn [e] (robot-unpause-capture ctx container))])

           (s/button
             :id :robot-stop-capture
             :text "Stop Recording"
             :visible? false
             :listen
             [:action
              (fn [e] (robot-stop-capture ctx container))])])
        ))
    (refresh)
    container))