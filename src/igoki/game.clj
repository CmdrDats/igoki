(ns igoki.game
  (:require
    [igoki.ui :as ui]
    [igoki.view :as view]
    [igoki.util :as util]
    [igoki.sgf :as sgf]
    [quil.core :as q])
  (:import (java.awt Toolkit)
           (javax.swing JOptionPane JFileChooser)
           (java.io FileFilter)
           (processing.core PImage)
           (java.util Date)
           (java.text SimpleDateFormat)))

;; TODO: Pause mode - don't record anything until unpaused
;; TODO: Disallow ko's from being played
;; TODO: fix moving back and forward in history - mark board 'dirty' so that the players can see the diff.
;; TODO: make 'dirty' state visible.
;; TODO: update game state with invalid board information
;; TODO: Move undo needs to deal with branching

;; TODO: Branch mode, lock down kifu so that you can explore branches
;; TODO: Walk kifu with left + right arrows (implicit locking/branching)
;; TODO: Record branches

;; Some other thoughts :
;;
;; Online Go integration
;;  - Record games between known players (possibly ranked live games?)
;;  - Upload kifu in realtime to OGS
;;  - Load games from OGS with guided step-by-step setup, resume correspondence game that way?


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

(defn reconstruct [{:keys [moves] :as game}]
  (let [constructed (sgf/construct-board (:moves game) (:current-branch-path game) (:movenumber game))]
    (assoc game
      :constructed constructed
      :kifu-board (simple-board-view constructed))))

(defn play-move [{:keys [moves] :as game} [x y o n :as move]]
  (println "Play move:" move)
  (let [updatedgame
        (->
          game
          (sgf/collect-node {(if (= n :b) :black :white) [(sgf/convert-coord x y)]})
          (assoc :dirty false)
          (update :movenumber (fnil inc 0)))]
    (reconstruct updatedgame)))



(defn board-updated [_ ctx _ board]
  (println "Board updated.")
  (let [{{:keys [kifu-board constructed dirty] :as game} :kifu} @ctx
        nodes (sgf/current-branch-node-list (:current-branch-path game) (:moves game))
        moves (filter #(or (:black %) (:white %)) nodes)
        lastmove (last moves)
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
        (= (take 2 mv) (sgf/convert-sgf-coord (first (or (:black lastmove) (:white lastmove))))))
      (do
        (println "Last move undone - this is currently very broken.")
        (swap!
          ctx
          (fn [c]
            (-> c
                (update-in (concat [:kifu :moves] (:current-branch-path game)) #(vec (butlast %)))
                (update :kifu reconstruct))))
        #_(swap! ctx update :kifu assoc
               :kifu-board board
               :moves (vec (butlast moves))))

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
  ;; TODO: Implement this as a list of add-black and add-white actions
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
  (let [board (-> @ctx :board)
        construct
        (fn [{:keys [moves] :as kifu}]
          (let [constructed (sgf/construct-board moves [] (count moves))]
            (assoc
              kifu
              :constructed constructed
              :kifu-board (simple-board-view constructed))))]
    (swap! ctx assoc :kifu
           (->
             {:moves nil :movenumber 1}
             (sgf/branch)
             (sgf/collect-node
               (add-initial-points
                 {:player-start ["B"]
                  :application  ["Igoki"]
                  :file-format  ["4"]
                  :gametype     ["1"]
                  :size         [(-> @ctx :goban :size)]
                  :date         [(.format (SimpleDateFormat. "YYYY-MM-dd") (Date.))]
                  :komi         ["5.5"]}
                 board))
             construct))))

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
  (let [{{:keys [submit kifu-board] :as game} :kifu
         {:keys [^PImage pimg]}               :camera
         board                                :board
         {:keys [size]}                       :goban} @ctx
        cellsize (/ (q/height) (+ size 2))
        grid-start (+ cellsize (/ cellsize 2))
        tx (+ (q/height) (/ cellsize 2))
        actionlist (sgf/current-branch-node-list (:current-branch-path game) (:moves game))
        movelist (take (:movenumber game) (filter #(or (:black %) (:white %)) actionlist))
        lastmove (last movelist)]
    (when pimg
      (q/image-mode :corners)
      (q/image pimg (q/height) (* (q/height) (/ (.height pimg) (.width pimg))) (q/width) (q/height)))

    (ui/shadow-text "Recording Kifu..." tx 25)
    (ui/shadow-text (str "Move " (:movenumber game) "/" (inc (count movelist)) ", " (if (:black lastmove) "White" "Black") " to play") tx 50)
    (ui/shadow-text "<R> Reset Kifu" tx 100)
    (ui/shadow-text "<V> Back to camera diff view" tx 125)
    (ui/shadow-text "<C> Calibrate board" tx 150)
    (ui/shadow-text "<E> Export SGF" tx 175)

    (q/fill 220 179 92)
    (q/rect 0 0 (q/height) (q/height))
    (q/stroke-weight 1.1)
    (q/stroke-cap :square)
    (q/stroke 0 196)

    (q/fill 0)

    (q/text-font (q/create-font "Helvetica" 20))
    (doseq [x (range size)]
      (let [coord (+ grid-start (* x cellsize))
            extent (+ grid-start (* cellsize (dec size)))]
        (q/text-align :center :bottom)
        (q/text (str (char (+ 65 x))) coord (- grid-start (/ cellsize 2)))
        (q/text-align :center :top)
        (q/text (str (char (+ 65 x))) coord (+ extent (/ cellsize 2)))

        (q/text-align :right :center)
        (q/text (str (- size x)) (- grid-start (/ cellsize 2)) coord)
        (q/text-align :left :center)
        (q/text (str (- size x)) (+ extent (/ cellsize 2)) coord)

        (q/line coord grid-start coord extent)
        (q/line grid-start coord extent coord)))

    (doseq [[x y] (star-points size)]

      (q/stroke-weight 1)
      (q/stroke 0 32)
      (q/fill 0)
      (q/ellipse (+ grid-start (* x cellsize) 0.5)
                 (+ grid-start (* y cellsize) 0.5) 6 6))

    (doseq [[y row] (map-indexed vector board)
            [x d] (map-indexed vector row)]
      (when d
        (q/stroke-weight 1)
        (q/stroke 0 32)
        (q/fill (if (= d :w) 255 0) 32)
        (q/ellipse (+ grid-start (* x cellsize))
                   (+ grid-start (* y cellsize)) (- cellsize 3) (- cellsize 3))))

    (q/text-size 12)
    (doseq [[p {:keys [stone movenumber]}] (-> game :constructed :board)]
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

    (when lastmove
      (let [{:keys [black white]} lastmove]
        (doseq [m (or black white)]
          (let [[x y] (sgf/convert-sgf-coord m)]
            (q/stroke (if white 0 255))
            (q/stroke-weight 3)
            (q/fill 0 0)
            (q/ellipse (+ grid-start (* x cellsize))
                       (+ grid-start (* y cellsize)) (/ cellsize 2) (/ cellsize 2))))))

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

    (when (and board kifu-board))
    (doseq [[x y d _]

            (board-diff kifu-board board)]
      (q/stroke-weight 3)
      (q/stroke 255 0 0)
      (q/fill 0 0)
      (q/ellipse (+ grid-start (* x cellsize))
                 (+ grid-start (* y cellsize)) (- cellsize 3) (- cellsize 3)))))

(defn export-sgf [ctx]
  (ui/save-dialog #(spit % (sgf/sgf (:kifu @ctx)))))

(defmethod ui/key-pressed :kifu [ctx]
  #_(.showSaveDialog (JFileChooser.) (:sketch @ctx))

  (case
    (q/key-code)
    67 (ui/transition ctx :goban)
    86 (ui/transition ctx :view)
    82 (reset-kifu ctx)
    69 (export-sgf ctx)
    37 (swap! ctx update-in [:kifu :movenumber] (fnil dec 1))
    39 (swap! ctx update-in [:kifu :movenumber] (fnil inc 0))
    (println "Key code not handled: " (q/key-code))))