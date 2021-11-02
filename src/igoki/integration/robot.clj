(ns igoki.integration.robot
  (:require
    [seesaw.core :as s]
    [igoki.camera :as camera]
    [igoki.integration.ogs :as ogs]
    [igoki.sgf :as sgf]
    [clojure.string :as str]
    [igoki.inferrence :as inferrence]
    [igoki.sound.sound :as snd]
    [igoki.sound.announce :as announce])
  (:import
    (java.awt Robot Rectangle)
    (java.awt.image BufferedImage)
    (org.nd4j.linalg.exception ND4JIllegalStateException)
    (java.util Date)
    (java.text SimpleDateFormat)))


; BufferedImage before = getBufferedImage(encoded);
; int w = before.getWidth();
; int h = before.getHeight();
; BufferedImage after = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
; AffineTransform at = new AffineTransform();
; at.scale(2.0, 2.0);
; AffineTransformOp scaleOp =
;    new AffineTransformOp(at, AffineTransformOp.TYPE_BILINEAR);
; after = scaleOp.filter(before, after);

(defn rescale-image [bufimg [width height]]
  (let [result (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        g2d (.getGraphics result)]

    (.drawImage g2d bufimg 0 0 width height nil)
    (.dispose g2d)
    result))

(defn read-frame [ctx]
  (let [{:keys [goban] :as c} @ctx
        {:keys [frame bounds ^Robot robot captured-list]} (:robot c)
        size (or (:size goban) 19)
        [x y w h] bounds
        #_#__ (.setVisible frame false)
        #_#__ (Thread/sleep 50)
        bufimg (.createScreenCapture robot (Rectangle. x y w h))
        scaled (rescale-image bufimg (camera/ref-size-vec (dec size)))
        board
        (doall
          (for [y (range size)]
            (doall
              (for [x (range size)]
                (try
                  (let [pt
                        (.getSubimage scaled (* x camera/block-size) (* y camera/block-size)
                          camera/block-size camera/block-size)
                        [b e w]
                        (try
                          (camera/eval-spot pt)
                          (catch ND4JIllegalStateException e
                            (.printStackTrace e)))]
                    (cond
                      (> b 0.5) :b
                      (> w 0.5) :w))
                  (catch Exception e))))))]

    (swap! ctx update :robot assoc
      :scaled scaled
      :update-list (conj (or captured-list []) board)
      :board board)

    #_(.setVisible frame true)
    (.repaint frame)))

(defn read-robot-loop [ctx]
  (let [{:keys [goban robot]} @ctx]
    ;; If frame is paused, skip reading.
    (when (true? (:started robot))
      (read-frame ctx))
    (Thread/sleep 250)

    (when (:started robot)
      (recur ctx))))

(defn initialize [ctx game]
  (let [initial-node
        (cond->
          {:branches []
           :player-start [(case (:initial_player game) "white" "W" "B")]
           :application [(str "igoki v" (System/getProperty "igoki.version"))]
           :file-format ["4"]
           :gametype ["1"]
           :size [(:width game) (:height game)]
           :date [(.format (SimpleDateFormat. "YYYY-MM-dd") (Date.))]
           :game-name [(:game_name game)]
           :black-rank [(-> game :players :black :rank)]
           :black-name [(-> game :players :black :name)]
           :white-rank [(-> game :players :white :rank)]
           :white-name [(-> game :players :white :name)]}
          (not (str/blank? (-> game :initial_state :white)))
          (assoc :add-white (map (partial apply str) (partition 2 (-> game :initial_state :white))))

          (not (str/blank? (-> game :initial_state :black)))
          (assoc :add-black (map (partial apply str) (partition 2 (-> game :initial_state :black)))))

        game-setup
        (inferrence/reconstruct
          {:moves initial-node
           :current-branch-path [[]]
           :movenumber 0})]

    (swap! ctx
      (fn [c]
        (->
          c
          (update :kifu merge game-setup)
          (update :robot assoc
            :gameinfo game
            :current-branch-path (:current-branch-path game-setup)
            :movenumber (:movenumber game-setup)))))))

(defn initialize-game [ctx]
  (let [{:keys [goban robot]} @ctx
        {:keys [game-detail board]} robot

        converted
        (->>
          (for [[y rows] (map-indexed vector board)
                [x cell] (map-indexed vector rows)]
            (when cell
              [cell (sgf/convert-coord x y)]))
          (remove nil?))
        white
        (->>
          converted
          (filter (fn [[m _]] (= m :w)))
          (map second)
          (apply str))

        black
        (->>
          converted
          (filter (fn [[m _]] (= m :b)))
          (map second)
          (apply str))
        ]
    (initialize ctx
      {:initial_player (str/lower-case (:initial-player game-detail))
       :width (:size goban)
       :height (:size goban)
       :start_time (int (/ (System/currentTimeMillis) 1000))
       :game_name (:game-name game-detail)
       :players
       {:black {:rank (:black-rank game-detail)
                :name (:black-name game-detail)}
        :white {:rank (:white-rank game-detail)
                :name (:white-name game-detail)}}
       :moves []
       :initial_state
       {:white white
        :black black}})))

(defn infer-board-play [ctx {:keys [robot kifu]}]
  (let [{:keys [update-list]} robot
        new (inferrence/infer-moves kifu update-list (last update-list))]
    (when (and new (not= (:kifu-board new) (:kifu-board kifu)))
      (swap! ctx
        (fn [c]
          (-> c
              (assoc :kifu (dissoc new :submit))
              (update :robot assoc :update-list []))))
      (snd/play-sound :click)

      (announce/comment-move ctx
        (last
          (sgf/current-branch-node-list
            (take (:movenumber new) (:current-branch-path new)) (:moves new)))
        (:constructed new)))))

(defn start-capture [ctx bounds game-detail]
  (try
    (swap! ctx update :robot assoc
      :started true :robot (Robot.) :bounds bounds
      :game-detail game-detail)

    (read-frame ctx)

    (initialize-game ctx)

    (add-watch ctx ::robot-capture
      (fn [k r o n]
        ;; See if there's a new board state in the capture
        (let [oldboard (get-in o [:robot :board])
              newboard (get-in n [:robot :board])]
          (when
            (and
              (true? (get-in o [:robot :started]))
              (not= oldboard newboard))
            (infer-board-play ctx n)))

        ;; See if there's a new board state that we need to infer

        ))
    (doto
      (Thread. (partial #'read-robot-loop ctx))
      (.setDaemon true)
      (.start))
    (catch Exception e
      (s/alert (str "Could not start capturing: " (.getName (.getClass e)) " - " (.getMessage e)) :type :error)
      (.printStackTrace e))))

(defn pause-capture [ctx]
  (when (-> @ctx :robot :started)
    (swap! ctx update :robot assoc :started :paused)))

(defn unpause-capture [ctx]
  (when (-> @ctx :robot :started)
    (swap! ctx update :robot assoc :started true)))

(defn stop-capture [ctx]
  (when (-> @ctx :robot :started)
    (remove-watch ctx ::robot-capture)
    (swap! ctx update :robot assoc :started false)))