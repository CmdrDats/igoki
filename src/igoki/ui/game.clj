(ns igoki.ui.game
  (:require
    [igoki.util :as util]
    [igoki.litequil :as lq]
    [igoki.sgf :as sgf]
    [igoki.ui.util :as ui.util]
    [igoki.game :as game])
  (:import
    (java.io File)))


(defn export-sgf [ctx]
  (ui.util/save-dialog
    (:current-file @ctx)
    #(spit % (game/convert-sgf ctx))))

(defn load-sgf [ctx]
  (ui.util/load-dialog
    (fn [^File f]
      (println "Opening sgf: " (.getAbsolutePath f))
      (game/load-sgf ctx f))))


(def move-colours
  {0 {:white [0 0 0] :black [255 255 255]}
   1 {:white [255 64 64] :black [255 96 96]}
   2 {:white [0 150 0] :black [64 255 64]}
   3 {:white [32 32 255] :black [128 128 255]}
   4 {:white [255 255 0] :black [255 255 0]}
   5 {:white [0 255 255] :black [0 255 255]}
   6 {:white [255 0 255] :black [255 0 255]}})

(defn draw [ctx]
  (lq/stroke-weight 1)
  (lq/color 0)
  (lq/background 255 255 255)
  (lq/rect 0 0 (lq/width) (lq/height))

  ;; Draw the board
  (let [{{:keys [submit kifu-board constructed movenumber] :as game} :kifu
         {:keys [pimg flattened-pimage]} :camera
         board :board
         {:keys [size ]} :goban} @ctx
        cellsize (/ (lq/height) (+ size 2))
        grid-start (+ cellsize (/ cellsize 2))
        board-size (* cellsize (dec size))
        extent (+ grid-start board-size)
        tx (+ (lq/height) (/ cellsize 2))
        visiblepath (take (or movenumber 0) (mapcat identity (:current-branch-path game)))
        actionlist (sgf/current-branch-node-list [visiblepath] (:moves game))
        lastmove (last actionlist)
        canvas-size (max 250 (min (lq/width) (lq/height)))]

    (lq/shadow-text (str "Recording: Img #" (:camidx game)) tx 25)
    (when (:filename game)
      (lq/shadow-text (:filename game) tx 50))
    (lq/shadow-text (str "Move " (inc (or movenumber 0)) ", " (if (= (:player-turn constructed) :black) "Black" "White") " to play") tx 75)
    (lq/shadow-text "<P> Pass" tx 225)


    (when flattened-pimage
      (lq/image (:bufimg flattened-pimage)
        (- grid-start cellsize) (- grid-start cellsize)
        (+ board-size (* cellsize 2)) (+ board-size (* cellsize 2))))

    (lq/color 220 179 92 150)
    (lq/fillrect 0 0 canvas-size canvas-size)


    (lq/stroke-weight 0.8)
    (lq/color 0 196)
    (lq/background 0)

    ;; Draw the grid
    (lq/text-font "helvetica-20pt")

    (doseq [x (range size)]
      (let [coord (+ grid-start (* x cellsize))]
        (lq/text (str (inc x)) coord (- grid-start (/ cellsize 2))
          {:align [:center :bottom]})
        (lq/text (str (inc x)) coord (+ extent (/ cellsize 2))
          {:align [:center :top]})

        (lq/text (str (inc x))
          (- grid-start (/ cellsize 2)) coord
          {:align [:right :center]})
        (lq/text (str (inc x)) (+ extent (/ cellsize 2)) coord
          {:align [:left :center]})

        (lq/line coord grid-start coord extent)
        (lq/line grid-start coord extent coord)))

    ;; Draw star points
    (doseq [[x y] (util/star-points size)]
      (lq/stroke-weight 1)
      (lq/color 0 32)
      (lq/background 0)
      (lq/ellipse
        (+ grid-start (* x cellsize))
        (+ grid-start (* y cellsize)) 6 6))

    ;; Draw camera board (shadow)
    (doseq [[y row] (map-indexed vector board)
            [x d] (map-indexed vector row)]
      (when d
        (lq/stroke-weight 1)
        (lq/color 0 32)
        (lq/background (if (= d :w) 255 0) 32)
        (lq/ellipse
          (+ grid-start (* x cellsize))
          (+ grid-start (* y cellsize))
          (- cellsize 3) (- cellsize 3))))

    (lq/text-size 12)

    ;; Draw the constructed sgf board stones
    (doseq [[pt {:keys [stone] mn :movenumber}] (:board constructed)]
      (let [[x y :as p] (sgf/convert-sgf-coord pt)]
        (when (and p stone)
          (lq/stroke-weight 0.5)
          (lq/color 0)
          (lq/background (if (= stone :white) 255 0))
          (lq/ellipse (+ grid-start (* x cellsize))
            (+ grid-start (* y cellsize)) (- cellsize 2) (- cellsize 2))

          (lq/background (if (= stone :white) 0 255)))

        (when (and (not stone) mn)
          (lq/stroke-weight 0)
          (lq/color 220 179 92)
          (lq/background 220 179 92)
          (lq/ellipse (+ grid-start (* x cellsize))
            (+ grid-start (* y cellsize)) 20 20)

          (lq/background 0))

        (when (and mn (< (- movenumber mn) 40))
          (let [movediff (- movenumber mn)
                movenum (mod (inc mn) 100)
                movecol (get-in move-colours [(int (/ mn 100)) (or stone :black)] [0 0 0])
                movecol
                (if (> movediff 20)
                  (conj movecol (- 255 (* 255 (/ (- movediff 20) 20))))
                  movecol)]
            (apply lq/color movecol)

            (lq/text-size 12)
            (lq/text (str movenum) (+ grid-start (* x cellsize)) (- (+ grid-start (* y cellsize)) 1)
              {:align [:center :center]})))))

    ;; TODO: This should go out to its own panel.
    (when (:comment lastmove)
      (lq/color 255)
      (lq/text-size 12)
      (lq/text (first (:comment lastmove)) tx 240 (- (lq/width) tx) (lq/height)
        {:align [:left :top]}))


    ;; Draw labels
    (doseq [label (:label lastmove)]
      (let [[pt text] (.split label ":" 2)
            [x y :as p] (sgf/convert-sgf-coord pt)
            stone (nth (nth kifu-board y) x)]

        (cond
          (= stone :w)
          (do
            (lq/background 255)
            (lq/color 255))

          (= stone :b)
          (do
            (lq/background 0)
            (lq/color 0))

          :else
          (do
            (lq/background 220 179 92)
            (lq/color 220 179 92)))

        (lq/stroke-weight 0)
        (lq/ellipse (+ grid-start (* x cellsize))
          (+ grid-start (* y cellsize)) (/ cellsize 1.5) (/ cellsize 1.5))
        (lq/background (if (= stone :b) 255 0))

        (lq/text
          text
          (+ grid-start (* x cellsize))
          (- (+ grid-start (* y cellsize)) 1)
          {:align [:center :center]})))

    ;; Draw annotated triangles.
    (doseq [pt (:triangle lastmove)]
      (let [[x y :as p] (sgf/convert-sgf-coord pt)
            stone (nth (nth kifu-board y) x)]

        (lq/stroke-weight 0)
        (apply lq/background (cond (= stone :b) [0] (= stone :w) [255] :else [220 179 92]))
        (lq/ellipse (+ grid-start (* x cellsize))
          (+ grid-start (* y cellsize)) (/ cellsize 1.1) (/ cellsize 1.1))

        (lq/stroke-weight 2)
        (lq/color (if (= stone :b) 255 0))
        (lq/triangle
          (+ grid-start (* x cellsize)) (- (+ grid-start (* y cellsize)) 6)
          (- (+ grid-start (* x cellsize)) 6) (+ (+ grid-start (* y cellsize)) 4.5)
          (+ (+ grid-start (* x cellsize)) 6) (+ (+ grid-start (* y cellsize)) 4.5))))

    ;; If in the process of submitting, mark that stone.
    (when submit
      #_(let [[x y _ d] (:move submit)]
          (lq/stroke-weight 1)
          (lq/stroke 0 128)
          (lq/background (if (= d :w) 255 0) 128)
          (lq/ellipse
            (+ grid-start (* x cellsize))
            (+ grid-start (* y cellsize))
            (- cellsize 3) (- cellsize 3))
          (lq/background (if (= d :w) 0 255))
          (lq/text "?" (+ grid-start (* xcellsize)) (+ grid-start (* y cellsize))
            {:align [:center :center]})))

    ;; Mark the last move
    (when lastmove
      (let [{:keys [black white]} lastmove]
        (doseq [m (or black white)]
          (let [[x y :as p] (sgf/convert-sgf-coord m)]
            (when p
              (lq/color (if white 0 255))
              (lq/stroke-weight 3)
              (lq/background 0 0)
              (lq/ellipse (+ grid-start (* x cellsize))
                (+ grid-start (* y cellsize)) (/ cellsize 2) (/ cellsize 2))))))

      ;; Mark next branches
      (when (:show-branches game)
        (doseq [[idx {:keys [black white]}] (map-indexed vector (:branches lastmove))
                m (or black white)]
          (let [[x y :as p] (sgf/convert-sgf-coord m)]
            (when p
              (if (zero? idx)
                (lq/color (if white 255 0))
                (apply lq/color (if white [255 0 0] [0 0 255])))
              (lq/stroke-weight 3)
              (lq/background 0 0)
              (lq/ellipse (+ grid-start (* x cellsize))
                (+ grid-start (* y cellsize)) (/ cellsize 2) (/ cellsize 2))
              (lq/background 0)

              (when (pos? idx)
                (lq/text-size 9)
                (lq/text
                  (str idx)
                  (- (+ grid-start (* x cellsize)) 9)
                  (- (+ grid-start (* y cellsize)) 9))))))))

    ;; Highlight differences between constructed and camera board (visual syncing)
    (when (and board kifu-board)
      (doseq [[x y _ _]
              (game/board-diff kifu-board board)]
        (lq/stroke-weight 3)
        (lq/color 255 0 0)
        (lq/background 0 0)
        (lq/ellipse (+ grid-start (* x cellsize))
          (+ grid-start (* y cellsize)) (- cellsize 3) (- cellsize 3))))))


(defn key-typed [ctx e]
  (case (lq/key-code e)
    37 (game/move-backward ctx)
    39 (game/move-forward ctx)
    80 (game/pass ctx)
    (println "Key code not handled: " (lq/key-code e))))


(defn game-panel [ctx]
  (:panel
    (lq/sketch-panel
      {:draw (partial #'draw ctx)
       :key-typed (partial #'key-typed ctx)})))