(ns igoki.ui.robot
  (:require
    [seesaw.core :as s]
    [seesaw.color :as sc]
    [igoki.litequil :as lq]
    [igoki.util :as util]
    [igoki.integration.robot :as i.robot]
    [igoki.camera :as camera]
    [seesaw.mig :as sm])
  (:import
    (java.awt.event MouseEvent)
    (javax.swing JFrame)
    (java.awt Rectangle Cursor Graphics2D BasicStroke)
    (java.awt.image BufferedImage)
    (org.nd4j.linalg.exception ND4JIllegalStateException)))


(defn get-mouse-op [ctx x y sx sy]
  (let [started? (get-in @ctx [:robot :started])]
    (cond
      started? [:none :none]

      :else
      [(cond
         (< x 25) :west
         (> x (- sx 25)) :east
         :else :move)

       (cond
         (< y 25) :north
         (> y (- sy 25)) :south
         :else :move)])))

(defn apply-xop [^Rectangle bounds [xop yop] mdx]
  ;; Yes, creating duplicate objects. Mutation sucks. A hill I'll die on.
  (let [result (Rectangle. bounds)]
    (case xop
      :west
      (.setBounds result
        (+ (.getX bounds) mdx) (.getY bounds)
        (- (.getWidth bounds) mdx) (.getHeight bounds))
      :east
      (.setSize result
        (+ (.getWidth bounds) mdx) (.getHeight bounds))

      ;; Only move if the other is _also_ move, else it's non-standard behaviour
      :move
      (when (= yop :move)
        (.setLocation result
          (+ (.getX bounds) mdx) (.getY bounds)))

      nil)
    result))

(defn apply-yop [^Rectangle bounds [xop yop] mdy]
  ;; Yes, creating duplicate objects. Mutation sucks. A hill I'll die on.
  (let [result (Rectangle. bounds)]
    (case yop
      :north
      (.setBounds result
        (.getX bounds) (+ (.getY bounds) mdy)
        (.getWidth bounds) (- (.getHeight bounds) mdy))
      :south
      (.setSize result
        (.getWidth bounds) (+ (.getHeight bounds) mdy))

      ;; Only move if the other is _also_ move, else it's non-standard behaviour
      :move
      (when (= xop :move)
        (.setLocation result
          (.getX bounds) (+ (.getY bounds) mdy)))

      nil)
    result))

(defn setup-resize-bounds [ctx ^JFrame frame]
  (let [state (atom {})]
    (s/listen frame
      :mouse-moved
      (fn [^MouseEvent e]
        (let [size (.getSize frame)
              [xop yop] (get-mouse-op ctx (.getX e) (.getY e) (.getWidth size) (.getHeight size))
              cursor
              (Cursor/getPredefinedCursor
                (cond
                  (and (= yop :north) (= xop :east)) Cursor/NE_RESIZE_CURSOR
                  (and (= yop :north) (= xop :west)) Cursor/NW_RESIZE_CURSOR
                  (and (= yop :south) (= xop :east)) Cursor/SE_RESIZE_CURSOR
                  (and (= yop :south) (= xop :west)) Cursor/SW_RESIZE_CURSOR
                  (= yop :north) Cursor/N_RESIZE_CURSOR
                  (= yop :south) Cursor/S_RESIZE_CURSOR
                  (= xop :east) Cursor/E_RESIZE_CURSOR
                  (= xop :west) Cursor/W_RESIZE_CURSOR
                  (= xop :none) Cursor/DEFAULT_CURSOR
                  :else Cursor/MOVE_CURSOR))]
          (.setCursor frame cursor)))

      :mouse-pressed
      (fn [^MouseEvent e]
        (let [size (.getSize frame)
              op (get-mouse-op ctx (.getX e) (.getY e) (.getWidth size) (.getHeight size))]
          (swap! state assoc
            :op op
            :x (.getXOnScreen e) :y (.getYOnScreen e))))

      :mouse-dragged
      (fn [^MouseEvent e]
        (let [{:keys [x y mx my op] :as s} @state
              dx (- (.getXOnScreen e) x)
              dy (- (.getYOnScreen e) y)
              bounds
              (->
                (.getBounds frame)
                (apply-xop op dx)
                (apply-yop op dy))]

          ;; Only shift bounds if we've not making it too small.
          (when
            (and
              (> (.getWidth bounds) 150)
              (> (.getHeight bounds) 150))

            (.setBounds frame bounds)))

        (swap! state assoc :x (.getXOnScreen e) :y (.getYOnScreen e))))))

(defn paint-robot-frame [ctx frame ^Graphics2D g2d]
  (let [{:keys [robot goban]} @ctx

        framesize (.getSize frame)
        size (or (:size goban) 19)

        cellwidth (/ (.getWidth framesize) size)
        cellheight (/ (.getHeight framesize) size)
        gridx-start (/ cellwidth 2)
        gridy-start (/ cellheight 2)
        boardx-size (* cellwidth (dec size))
        boardy-size (* cellheight (dec size))
        extentx (+ gridx-start boardx-size)
        extenty (+ gridy-start boardy-size)]

    (cond
      (:started robot)
      ;; Recording started, full transparent and red border.
      (do
        (.setBackground g2d (sc/color 0 0 0 0))
        (.setColor g2d
          (sc/color (if (= :paused (:started robot)) :red :green)))

        (.drawRect g2d 0 0 (dec (.getWidth framesize)) (dec (.getHeight framesize)))
        ;; DEBUG: Show what we're interpreting from
        #_(when (:scaled robot)
          (let [scaled (:scaled robot)
                bufimg (BufferedImage. (.getWidth scaled) (.getHeight scaled) BufferedImage/TYPE_INT_ARGB)
                bgd (.getGraphics bufimg)]
            (try
              (.setBackground g2d (sc/color :red))
              (.fillRect g2d 0 0 (+ 2 (.getWidth scaled)) (+ 2 (.getHeight scaled)))
              (.drawImage bgd scaled 1 1 nil)
              (.drawImage g2d bufimg 1 1 nil)
              (finally (.dispose bgd))))


          (doseq [y (range size)]
            (doseq [x (range size)]
              (try
                (let [pt
                      (.getSubimage (:scaled robot)
                        (* x camera/block-size) (* y camera/block-size)
                        camera/block-size camera/block-size)]
                  (.setBackground g2d (sc/color :red))
                  (.fillRect g2d (* x cellwidth) (* y cellwidth) 15 15)
                  (.drawImage g2d pt (* x cellwidth) (* y cellheight)
                    cellwidth cellheight nil)
                  #_(.drawString g2d (str [x y]) (int (* x cellwidth)) (int (+ 10 (* y cellheight))))
                  #_[b e w]
                  #_(cond
                      (> b 0.5) :b
                      (> w 0.3) :w))
                (catch Exception e)))))

        ;; DEBUG - show the board state
        #_(when (:board robot)
          (try
            (doseq [[y rows] (map-indexed vector (:board robot))
                    [x stone] (map-indexed vector rows)]
              (when stone
                (.setStroke g2d (BasicStroke. 3))
                (.setColor g2d (sc/color :red))
                (.setBackground g2d (sc/color (if (= stone :b) :black :white)))
                #_(.drawString g2d (str (vec (map #(int (* 10 %)) stone)))
                  (int (+ gridx-start (* x cellwidth)))
                  (int (+ gridy-start (* y cellheight))))
                (lq/ellipse g2d
                  (+ gridx-start (* x cellwidth))
                  (+ gridx-start (* y cellheight))
                  (- cellwidth 2) (- cellheight 2))))
            (catch Exception e))))

      :else
      (do
        (.setColor g2d (sc/color 0 0 0 128))
        (.fillRect g2d 0 0 (dec (.getWidth framesize)) (dec (.getHeight framesize)))
        (.setColor g2d (sc/color :white))
        (doseq [x (range size)]
          (let [coordx (+ gridx-start (* x cellwidth))
                coordy (+ gridy-start (* x cellheight))]
            (.drawLine g2d gridx-start coordy extentx coordy)
            (.drawLine g2d coordx gridy-start coordx extenty)))

        ;; Draw star points
        (doseq [[x y] (util/star-points size)]
          (lq/ellipse g2d
            (+ gridx-start (* x cellwidth))
            (+ gridy-start (* y cellheight)) 6 6))))))

;; Because, gosh, life is too short to be managing this state manually.
;; I really miss Rum/React.
(defn refresh-button-states [ctx container]
  (let [{:keys [robot]} @ctx
        states
        [[:#robot-open-button (not (:frame robot))]
         [:#robot-close-button (:frame robot)]
         [:#robot-game-detail-panel (and (:frame robot) (not (:started robot)))]
         [:#robot-start-capture
          (and (:frame robot) (not (:started robot)))]
         [:#robot-stop-capture
          (and (:frame robot) (:started robot))]
         [:#robot-pause-capture
          (and (:frame robot) (true? (:started robot)))]
         [:#robot-unpause-capture
          (and (:frame robot) (= :paused (:started robot)))]]]

    (doseq [[id state] states]
      ((if state s/show! s/hide!)
       (s/select container [id])))))

(defn robot-close-frame [ctx container]
  (let [{:keys [robot]} @ctx]
    (when (:frame robot)
      (s/dispose! (:frame robot))
      (swap! ctx update :robot dissoc :frame))

    (i.robot/stop-capture ctx)
    (refresh-button-states ctx container)))

(defn robot-capture-frame [ctx container]
  (let [{:keys [robot]} @ctx]
    ;; Get rid of an existing frame
    (robot-close-frame ctx container)


    (try
      (let [frame
            ;; Size chosen selfishly as my OGS default size (half-left screen)
            (s/window
              :width 714 :height 714
              :content (s/canvas :paint (partial #'paint-robot-frame ctx)))]
        (swap! ctx update :robot assoc :frame frame :started false)
        #_(.setUndecorated frame true)
        #_(.setResizable frame true)
        (.setBackground frame (sc/color 0 0 0 0))
        (.setVisible frame true)
        (.setAlwaysOnTop frame true)

        (setup-resize-bounds ctx frame)
        (refresh-button-states ctx container))
      (catch Exception e
        (.printStackTrace e))))

  #_(.getGraphicsConfiguration (JFrame.)))



(defn robot-start-capture [ctx container]
  (let [{:keys [robot]} @ctx
        frame (:frame robot)
        bounds [(.getX frame) (.getY frame) (.getWidth frame) (.getHeight frame)]]

    ;; Get the actual frame out the way before
    (.setVisible frame false)
    #_(Thread/sleep 10)

    (i.robot/start-capture ctx (.getDevice (.getGraphicsConfiguration frame)) bounds
      (s/value (s/select container [:#robot-game-detail])))

    (.setVisible frame true)
    (refresh-button-states ctx container)
    (.repaint frame)
    ))

(defn robot-pause-capture [ctx container]
  (let [{:keys [robot]} @ctx
        frame (:frame robot)]
    (i.robot/pause-capture ctx)
    (refresh-button-states ctx container)
    (.repaint frame)))

(defn robot-unpause-capture [ctx container]
  (let [{:keys [robot]} @ctx
        frame (:frame robot)]
    (i.robot/unpause-capture ctx)
    (refresh-button-states ctx container)
    (.repaint frame)))

(defn robot-stop-capture [ctx container]
  (let [{:keys [robot]} @ctx
        frame (:frame robot)]
    (i.robot/stop-capture ctx)
    (refresh-button-states ctx container)
    (.repaint frame)))

(defn game-setup-panel [ctx container]
  (s/scrollable
    (sm/mig-panel
      :constraints ["center" "" ""]
      :id :robot-game-detail
      :items
      [["Game Setup" "span, center, gapbottom 15"]
       ["Next Player (NB!): " "align label"]
       [(s/combobox
          :id :initial-player
          :model ["Black" "White"]) "wrap"]

       ["igoki player: " "align label"]
       [(s/combobox
          :id :robot-player
          :model ["None" "Black" "White" "Both"]) "wrap"]

       ["Game Details (optional)" "span, center, gapbottom 15"]
       ["Game name:" "align label"]
       [(s/text :id :game-name :columns 64) "wrap"]
       ["Black Player " "span, center, gapbottom 15"]
       ["Name: " "align label"]
       [(s/text :id :black-name :columns 32) "wrap"]
       ["Rank: " "align label"]
       [(s/text :id :black-rank :columns 10) "wrap"]

       ["White Player " "span, center, gapbottom 15"]
       ["Name: " "align label"]
       [(s/text :id :white-name :columns 32) "wrap"]
       ["Rank: " "align label"]
       [(s/text :id :white-rank :columns 10) "wrap"]])
    :id :robot-game-detail-panel
    :visible? false
    :hscroll :never))

(defn robot-panel [ctx]
  (let [container (s/border-panel)]
    (s/config! container :center
      (s/border-panel
        :north
        (s/flow-panel
          :items
          [(s/button
             :id :robot-open-button
             :text "Open capture frame"
             :visible? true
             :listen
             [:action
              (fn [e]
                (robot-capture-frame ctx container))])

           (s/button
             :id :robot-close-button
             :text "Close capture frame"
             :visible? false
             :listen
             [:action
              (fn [e] (robot-close-frame ctx container))])])

        :center
        (game-setup-panel ctx container)

        :south
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
    container))