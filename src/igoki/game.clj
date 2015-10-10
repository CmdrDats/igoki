(ns igoki.game
  (:require
    [igoki.ui :as ui]
    [igoki.view :as view]
    [igoki.util :as util]
    [igoki.sgf :as sgf]
    [quil.core :as q])
  (:import (java.awt Toolkit)
           (javax.swing JOptionPane JFileChooser)
           (java.io FileFilter File)
           (processing.core PImage)
           (java.util Date)
           (java.text SimpleDateFormat)))

(defn board-diff [b1 b2]
  (remove
    nil?
    (mapcat
      (fn [[y b1row] b2row]
        (map
          (fn [[x b1i] b2i]
            (if-not (= b1i b2i)
              [x y b1i b2i]))
          (map-indexed vector b1row) b2row))
      (map-indexed vector b1) b2)))

(defn simple-board-view [{:keys [board size]}]
  (let [[width height] size]
    (for [y (range height)]
      (for [x (range width)]
        (case (get-in board [(sgf/convert-coord x y) :stone])
          :white :w :black :b nil)))))

(defn submit-move
  [ctx move]
  (let [board (:board @ctx)]
    (println "Move at: " move ", debouncing")
    (swap!
      ctx
      (fn [c]
        (-> c
            (update :kifu assoc :submit {:move move :latch 3 :board board})
            (update :camera assoc :read-delay 150))))))

(defn reconstruct [{:keys [moves current-branch-path movenumber] :as game}]
  (let [visiblepath (vec (take movenumber (mapcat identity current-branch-path)))
        constructed (sgf/construct-board moves [visiblepath])]
    (assoc game
      :constructed constructed
      :kifu-board (simple-board-view constructed))))

(defn play-move [{:keys [moves current-branch-path movenumber] :as game} [x y o n :as move]]
  (println "Play move:" move)
  (let [visiblepath (vec (take movenumber (mapcat identity current-branch-path)))
        [node path] (sgf/collect-node moves {(if (= n :b) :black :white) [(sgf/convert-coord x y)]} [visiblepath])
        updatedgame
        (->
          game
          (assoc :moves node :dirty false :current-branch-path path)
          (update :movenumber (fnil inc 0)))]
    (println node " ---- " path)
    (reconstruct updatedgame)))



(defn board-updated [_ ctx _ board]
  (println "Board updated.")
  (let [{{:keys [kifu-board constructed dirty] :as game} :kifu} @ctx
        nodes (sgf/current-branch-node-list (:current-branch-path game) (:moves game))
        lastmove (last nodes)
        [[_ _ mo mn :as mv] :as diff] (board-diff kifu-board board)
        [[_ _ o n :as move] :as added] (remove (fn [[_ _ o n]] (and (not (nil? o))) (nil? n)) diff)
        {sim-board :kifu-board} (if (= 1 (count added)) (play-move game move))]
    (println "Diff:" diff)
    (println "Added:" (vec added))
    (cond
      (and (empty? diff) dirty)
      (do
        (println "Clean state, marking as such.")
        (swap! ctx assoc-in [:kifu :dirty] false))
      (and (not (empty? diff)) dirty)
      (println "Not actioning board updates until clean state is reached")
      ;; Special case to undo last move
      (and
        lastmove
        (= (count diff) 1)
        (nil? mn) (not (nil? mo))
        (first (or (:black lastmove) (:white lastmove)))
        (= (take 2 mv) (sgf/convert-sgf-coord (first (or (:black lastmove) (:white lastmove))))))
      (do
        (swap!
          ctx
          (fn [c]
            (-> c
                (update-in [:kifu :current-branch-path] #(update % (dec (count %)) (comp vec butlast)))
                (update :kifu reconstruct)))))

      (not= (count added) 1)
      (println "Not exactly one stone added, invalid move")
      (not= (:player-turn constructed) ({:b :black :w :white} n))
      (println "Not the correct next player colour")
      (not= sim-board board)
      (do
        (println "Board in incorrect state for the proposed next move, ignoring")
        (println sim-board))
      (or (not (nil? o)) (nil? n))
      (println "Can't move to existing location?")
      :else
      (submit-move ctx move))))



(defn camera-updated [wk ctx old new]
  (view/camera-updated wk ctx old new)
  (let [{{{:keys [latch board move] :as submit} :submit} :kifu
         cboard :board} @ctx]
    (cond
      (nil? submit) nil
      (not= cboard board)
      (do
        (println "Debounce dirty - move discarded")
        (swap!
          ctx
          (fn [c]
            (-> c
                (update :kifu dissoc :submit)
                (update :camera dissoc :read-delay)))))
      (pos? latch)
      (swap! ctx update-in [:kifu :submit :latch] dec)
      :else
      (do
        (println "Debounce success - move submitted")
        (.beep (Toolkit/getDefaultToolkit))
        (swap!
          ctx update :kifu
          #(-> %
               (play-move move )
               (dissoc :submit)))
        (swap! ctx update :camera dissoc :read-delay)))))

(defn add-initial-points [node board]
  (let [initial
        (for [[y row] (map-indexed vector board)
              [x v] (map-indexed vector row)
              :when v]
          [v x y])
        black (seq (filter (comp #(= :b %) first) initial))
        white (seq (filter (comp #(= :w %) first) initial))]
    (cond->
      node
      black (assoc :add-black (map (fn [[_ x y]] (sgf/convert-coord x y)) black))
      white (assoc :add-white (map (fn [[_ x y]] (sgf/convert-coord x y)) white)))))

(defn reset-kifu [ctx]
  (let [board (-> @ctx :board)]
    (swap! ctx assoc :kifu
           (->
             {:moves               (add-initial-points
                                     {:branches     []
                                      :player-start ["B"]
                                      :application  ["Igoki"]
                                      :file-format  ["4"]
                                      :gametype     ["1"]
                                      :size         [(-> @ctx :goban :size)]
                                      :date         [(.format (SimpleDateFormat. "YYYY-MM-dd") (Date.))]
                                      :komi         ["5.5"]}
                                     board)
              :movenumber          1
              :current-branch-path [[]]}
             reconstruct))))

(defmethod ui/construct :kifu [ctx]
  (if-not (-> @ctx :kifu)
    (reset-kifu ctx))
  (util/add-watch-path ctx :kifu-camera [:camera] #'camera-updated )
  (util/add-watch-path ctx :kifu-board [:board] #'board-updated))

(defmethod ui/destruct :kifu [ctx]
  (remove-watch ctx :kifu-camera)
  (remove-watch ctx :kifu-board))

(defn star-points [size]
  (case size
    9 [[2 2] [4 4] [2 6] [6 2] [6 6]]
    13 [[3 3] [6 6] [3 9] [9 3] [9 9]]
    (for [x (range 3) y (range 3)]
      [(+ 3 (* x 6)) (+ 3 (* y 6))])))


(defmethod ui/draw :kifu [ctx]
  (q/stroke-weight 1)
  (q/stroke 0)
  (q/fill 128 64 78)
  (q/rect 0 0 (q/width) (q/height))

  ;; Draw the board
  (let [{{:keys [submit kifu-board constructed movenumber] :as game} :kifu
         {:keys [^PImage pimg]}                                      :camera
         board                                                       :board
         {:keys [size]}                                              :goban} @ctx
        cellsize (/ (q/height) (+ size 2))
        grid-start (+ cellsize (/ cellsize 2))
        tx (+ (q/height) (/ cellsize 2))
        visiblepath (take movenumber (mapcat identity (:current-branch-path game)))
        actionlist (sgf/current-branch-node-list [visiblepath] (:moves game))
        lastmove (last actionlist)]
    (when pimg
      (q/image-mode :corners)
      (q/image pimg
               (q/height)
               (- (q/height) (* (- (q/width) (q/height)) (/ (.height pimg) (.width pimg))))
               (q/width) (q/height)))

    (ui/shadow-text "Recording Kifu..." tx 25)
    (ui/shadow-text (str "Move " (:movenumber game)  ", " (if (= (:player-turn constructed) :black) "Black" "White") " to play") tx 50)
    (ui/shadow-text "<R> Reset Kifu" tx 100)
    (ui/shadow-text "<V> Back to camera diff view" tx 125)
    (ui/shadow-text "<C> Calibrate board" tx 150)
    (ui/shadow-text "<E> Export SGF" tx 175)
    (ui/shadow-text "<L> Load SGF" tx 200)
    (ui/shadow-text "<M> Toggle show branches" tx 225)

    (q/fill 220 179 92)
    (q/rect 0 0 (q/height) (q/height))
    (q/stroke-weight 1.1)
    (q/stroke-cap :square)
    (q/stroke 0 196)

    (q/fill 0)

    ;; Draw the grid
    (q/text-font (q/create-font "Helvetica" 20))
    (doseq [x (range size)]
      (let [coord (+ grid-start (* x cellsize))
            extent (+ grid-start (* cellsize (dec size)))
            x-offset (if (> x ) 66 65)]
        (q/text-align :center :bottom)
        (q/text (str (char (+ x-offset x))) coord (- grid-start (/ cellsize 2)))
        (q/text-align :center :top)
        (q/text (str (char (+ x-offset x))) coord (+ extent (/ cellsize 2)))

        (q/text-align :right :center)
        (q/text (str (- size x)) (- grid-start (/ cellsize 2)) coord)
        (q/text-align :left :center)
        (q/text (str (- size x)) (+ extent (/ cellsize 2)) coord)

        (q/line coord grid-start coord extent)
        (q/line grid-start coord extent coord)))

    ;; Draw star points
    (doseq [[x y] (star-points size)]
      (q/stroke-weight 1)
      (q/stroke 0 32)
      (q/fill 0)
      (q/ellipse (+ grid-start (* x cellsize) 0.5)
                 (+ grid-start (* y cellsize) 0.5) 6 6))

    ;; Draw camera board (shadow)
    (doseq [[y row] (map-indexed vector board)
            [x d] (map-indexed vector row)]
      (when d
        (q/stroke-weight 1)
        (q/stroke 0 32)
        (q/fill (if (= d :w) 255 0) 32)
        (q/ellipse (+ grid-start (* x cellsize))
                   (+ grid-start (* y cellsize)) (- cellsize 3) (- cellsize 3))))

    (q/text-size 12)
    ;; Draw the constructed sgf board stones
    (doseq [[p {:keys [stone movenumber]}] (:board constructed)]
      (let [[x y] (sgf/convert-sgf-coord p)]
        (when stone
          (q/stroke-weight 1)
          (q/stroke 0)
          (q/fill (if (= stone :white) 255 0))
          (q/ellipse (+ grid-start (* x cellsize))
                     (+ grid-start (* y cellsize)) (- cellsize 3) (- cellsize 3))

          (q/fill (if (= stone :white) 0 255)))

        (when (and (not stone) movenumber)
          (q/stroke-weight 0)
          (q/stroke 220 179 92)
          (q/fill 220 179 92)
          (q/ellipse (+ grid-start (* x cellsize))
                     (+ grid-start (* y cellsize)) 20 20)

          (q/fill 0))

        (when movenumber
          (let [movenum (mod (inc movenumber) 100)]
            (when (>= (inc movenumber) 100)
              (q/fill 255 0 0))
            (q/text-size 12)
            (q/text-align :center :center)
            (q/text (str movenum) (+ grid-start (* x cellsize)) (- (+ grid-start (* y cellsize)) 1))))))

    ;; Mark the last move
    (when lastmove
      (let [{:keys [black white]} lastmove]
        (doseq [m (or black white)]
          (let [[x y] (sgf/convert-sgf-coord m)]
            (q/stroke (if white 0 255))
            (q/stroke-weight 3)
            (q/fill 0 0)
            (q/ellipse (+ grid-start (* x cellsize))
                       (+ grid-start (* y cellsize)) (/ cellsize 2) (/ cellsize 2)))))

      ;; Mark next branches
      (when (:show-branches game)
        (doseq [{:keys [black white]} (:branches lastmove)
                m (or black white)]
          (let [[x y] (sgf/convert-sgf-coord m)]
            (q/stroke (if white 255 0))
            (q/stroke-weight 3)
            (q/fill 0 0)
            (q/ellipse (+ grid-start (* x cellsize))
                       (+ grid-start (* y cellsize)) (/ cellsize 2) (/ cellsize 2))))))

    ;; If in the process of submitting, mark that stone.
    (when submit
      (let [[x y _ d] (:move submit)]
        (q/stroke-weight 1)
        (q/stroke 0 128)
        (q/fill (if (= d :w) 255 0) 128)
        (q/ellipse (+ grid-start (* x cellsize))
                   (+ grid-start (* y cellsize)) (- cellsize 3) (- cellsize 3))
        (q/fill (if (= d :w) 0 255))
        (q/text-align :center :center)
        (q/text "?" (+ grid-start (* x cellsize)) (+ grid-start (* y cellsize)))))

    ;; Highlight differences between constructed and camera board (visual syncing)
    (when (and board kifu-board)
      (doseq [[x y d _]
              (board-diff kifu-board board)]
        (q/stroke-weight 3)
        (q/stroke 255 0 0)
        (q/fill 0 0)
        (q/ellipse (+ grid-start (* x cellsize))
                   (+ grid-start (* y cellsize)) (- cellsize 3) (- cellsize 3))))))

(defn export-sgf [ctx]
  (ui/save-dialog #(spit % (sgf/sgf (-> @ctx :kifu :moves)))))

(defn load-sgf [ctx]
  (ui/load-dialog
    (fn [^File f]
      (println "Opening sgf: " (.getAbsolutePath f))
      (swap! ctx assoc :kifu
             (reconstruct {:moves (sgf/read-sgf (.getAbsolutePath f)) :movenumber 0 :current-branch-path []})))))

(defn toggle-branches [ctx]
  (swap! ctx update-in [:kifu :show-branches] not))

(defmethod ui/key-pressed :kifu [ctx]
  #_(.showSaveDialog (JFileChooser.) (:sketch @ctx))

  (case
    (q/key-code)
    67 (ui/transition ctx :goban)
    86 (ui/transition ctx :view)
    82 (reset-kifu ctx)
    69 (export-sgf ctx)
    76 (load-sgf ctx)
    77 (toggle-branches ctx)
    37 (swap! ctx #(-> %
                       (update-in [:kifu :movenumber] (fnil dec 1))
                       (update-in [:kifu :current-branch-path 0] conj 0)
                       (update-in [:kifu] reconstruct)))
    39 (swap! ctx #(-> %
                       (update-in [:kifu :movenumber] (fnil inc 1))
                       (update-in [:kifu] reconstruct)))
    (println "Key code not handled: " (q/key-code))))

