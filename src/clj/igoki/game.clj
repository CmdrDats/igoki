(ns igoki.game
  (:require
    [igoki.ui :as ui]
    [igoki.view :as view]
    [igoki.util :as util]
    [igoki.sgf :as sgf]
    [igoki.inferrence :as inferrence]
    [igoki.xutil :as xu]
    [quil.core :as q]
    [igoki.sound.sound :as snd])
  (:import (java.awt Toolkit)
           (java.io File ByteArrayInputStream)
           (processing.core PImage)
           (java.util Date UUID)
           (java.text SimpleDateFormat)
           (org.opencv.highgui Highgui)
           (org.opencv.core MatOfByte)
           (de.schlichtherle.truezip.file TVFS)))

;; ========================================================
;; TODO: Display sibling branches
;; TODO: Support swapping last move to a different point (traversing to the applicable branch)
;; TODO: Display other annotations (circle, mark, selected, square, territory-black, territory-white)
;; TODO: Cache last n moves for backtracking to prevent rebuilding it every time.
;; TODO: Mainline variation
;; TODO: 0 and 1 steps on SGF?
;; This will immensely speed up the end game performance.
;; ====================================================



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

(defonce captured-boardlist (atom []))

(defn submit-move
  [ctx]
  (let [board (:board @ctx)]
    (println "Change detected, debouncing")
    (swap!
      ctx
      (fn [c]
        (-> c
            (update :kifu assoc :submit {:latch 1 :board board})
            (update :camera assoc :read-delay 300))))))

(defn board-history [{:keys [current-branch-path movenumber moves] :as game}]
  (->>
    (range movenumber)
    ;; This is a slow operation, so just checking the last few moves.
    (take-last 20)
    (map
      (fn [m]
        (let [g (inferrence/reconstruct (assoc game :movenumber m))]
          [(:kifu-board g) g])))
    (into {})))

(defn board-updated [_ ctx _ board]
  #_(println "Board updated.")
  (swap! captured-boardlist conj board)
  (let [{{:keys [kifu-board dirty] :as game} :kifu} @ctx
        nodes (sgf/current-branch-node-list (:current-branch-path game) (:moves game))
        lastmove (last nodes)
        [[_ _ mo mn :as mv] :as diff] (board-diff kifu-board board)
        history-game (if (and lastmove (> (count diff) 0)) (get (board-history game) board))]
    (cond
      (and (empty? diff) dirty)
      (do
        (println "Clean state, marking as such.")
        (swap! ctx assoc-in [:kifu :dirty] false))
      (and (not (empty? diff)) dirty)
      (println "Not actioning board updates until clean state is reached")
      ;; Special case to undo last move
      ;; Disabled temporarily for issues with online integration.
      history-game
      (do
        (snd/play-sound :undo)
        (swap! ctx (fn [c] (assoc c :kifu history-game))))
      :else
      (submit-move ctx))))

(defn dump-camera [filename camidx raw updatelist]
  (when filename
    (util/with-release [out (MatOfByte.)]
      (println "Writing jpg: " filename "/" (str camidx ".jpg"))
      (Highgui/imencode ".jpg" raw out)
      (util/zip-add-file filename (str camidx ".jpg") (ByteArrayInputStream. (.toArray out)))
      (util/zip-add-file-string filename (str camidx ".edn") (pr-str updatelist))
      (println "Done writing jpg: " filename "/" (str camidx ".jpg"))
      )))

(defn camera-updated [wk ctx old new]
  (view/camera-updated wk ctx old new)
  (let [{{{:keys [latch board] :as submit} :submit
          :keys [filename camidx last-dump] :as game} :kifu
         {:keys [raw]}                      :camera
         cboard                             :board} @ctx
        updatelist @captured-boardlist
        t (System/nanoTime)]
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
        (snd/play-sound :submit)
        (println "Debounce success - move submitted")

        (let [new (inferrence/infer-moves game updatelist (last updatelist))]
          (if (and new (not= (:kifu-board new) (:kifu-board game)))
            (do
              (snd/play-sound :click)
              (reset! captured-boardlist [])
              (swap! ctx assoc :kifu (assoc (dissoc new :submit) :cam-update true)))
            (swap! ctx update :kifu #(assoc (dissoc % :submit) :cam-update true))))
        (swap! ctx update :camera dissoc :read-delay)))

    ;; Dump camera on a regular basis, ignore time if board is updated.
    #_(println last-dump t (- t last-dump) (> (- t last-dump 20e9)))
    (when (or (get-in @ctx [:kifu :cam-update])
            (nil? last-dump)
            (> (- t last-dump) 20e9 ))
      (dump-camera filename camidx raw updatelist)
      (swap! ctx update :kifu
        #(-> %
             (assoc :cam-update false :last-dump t)
             (update :camidx (fnil inc 0)))))
    ))

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
      white (assoc :add-white (map (fn [[_ x y]] (sgf/convert-coord x y)) white))
      (> (count black) (count white)) (assoc :player-start ["W"]))))

(defn reset-kifu [ctx]
  (let [context @ctx
        board (-> context :board)
        camfile (or (-> context :kifu :filename) (str "capture/" (.toString (UUID/randomUUID)) ".zip"))
        camidx (or (-> context :kifu :camidx) 0)
        new-game
        (->
          {:filename            camfile
           :camidx              (inc camidx)
           :moves               (add-initial-points
                                  {:branches     []
                                   :player-start ["B"]
                                   :application  ["Igoki"]
                                   :file-format  ["4"]
                                   :gametype     ["1"]
                                   :size         [(-> @ctx :goban :size)]
                                   :date         [(.format (SimpleDateFormat. "YYYY-MM-dd") (Date.))]
                                   :komi         ["5.5"]}
                                  board)
           :movenumber          0
           :current-branch-path [[]]}
          inferrence/reconstruct)]

    (when-not (.exists (File. "capture"))
      (.mkdir (File. "capture")))

    (util/zip-add-file-string
      (:filename new-game)
      (str camidx ".config.edn")
      (pr-str
        {:board     (:board context)
         :goban     (:goban context)
         :view      (dissoc (:view context) :homography)
         :kifu      new-game}))
    (dump-camera (:filename new-game) camidx (-> context :camera :raw) [board])
    (swap! ctx assoc :kifu new-game)))

(defmethod ui/construct :kifu [ctx]
  (when-not (-> @ctx :kifu)
    (TVFS/umount)
    (reset-kifu ctx))
  (util/add-watch-path ctx :kifu-camera [:camera :raw] #'camera-updated )
  (util/add-watch-path ctx :kifu-board [:board] #'board-updated))

(defmethod ui/destruct :kifu [ctx]
  (remove-watch ctx :kifu-camera)
  (remove-watch ctx :kifu-board))



;; In hundreds..
(def move-colours
  {0 {:white [0 0 0] :black [255 255 255]}
   1 {:white [255 64 64] :black [255 96 96]}
   2 {:white [0 150 0] :black [64 255 64]}
   3 {:white [32 32 255] :black [128 128 255]}
   4 {:white [255 255 0] :black [255 255 0]}
   5 {:white [0 255 255] :black [0 255 255]}
   6 {:white [255 0 255] :black [255 0 255]}})

(defn find-last-move [ctx]
  (let [{{:keys [movenumber] :as game} :kifu} @ctx
        visiblepath (if movenumber (take movenumber (mapcat identity (:current-branch-path game))))
        actionlist (if visiblepath (sgf/current-branch-node-list [visiblepath] (:moves game)))]
    (last actionlist)))

(defmethod ui/draw :kifu [ctx]
  (q/stroke-weight 1)
  (q/stroke 0)
  (q/fill 128 64 78)
  (q/rect 0 0 (q/width) (q/height))

  ;; Draw the board
  (let [{{:keys [submit kifu-board constructed movenumber] :as game} :kifu
         {:keys [pimg]}                                      :camera
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
      (q/image (:pimg pimg)
        (q/height)
        (- (q/height) (* (- (q/width) (q/height)) (/ (.height (:pimg pimg)) (.width (:pimg pimg)))))
        (q/width) (q/height)))

    (ui/shadow-text (str "Recording: Img #" (:camidx game)) tx 25)
    (when (:filename game)
      (ui/shadow-text (:filename game) tx 50))
    (ui/shadow-text (str "Move " (inc movenumber) ", " (if (= (:player-turn constructed) :black) "Black" "White") " to play") tx 75)
    (ui/shadow-text "<R> Reset, <V> Back" tx 125)
    (ui/shadow-text "<V> Back to camera diff view" tx 150)
    (ui/shadow-text "<E> Export, <L> Load SGF" tx 175)
    (ui/shadow-text "<M> Toggle show branches" tx 200)
    (ui/shadow-text "<P> Pass" tx 225)

    (q/fill 220 179 92)
    (q/rect 0 0 (q/height) (q/height))
    (q/stroke-weight 0.8)
    (q/stroke-cap :square)
    (q/stroke 0 196)

    (q/fill 0)

    ;; Draw the grid
    (q/text-font (q/create-font "Helvetica" 20))
    (doseq [x (range size)]
      (let [coord (+ grid-start (* x cellsize))
            extent (+ grid-start (* cellsize (dec size)))
            x-offset (if (> x 7) 66 65)]
        (q/text-align :center :bottom)
        (q/text (str (inc x)) coord (- grid-start (/ cellsize 2)))
        (q/text-align :center :top)
        (q/text (str (inc x)) coord (+ extent (/ cellsize 2)))

        (q/text-align :right :center)
        (q/text (str (inc x)) (- grid-start (/ cellsize 2)) coord)
        (q/text-align :left :center)
        (q/text (str (inc x)) (+ extent (/ cellsize 2)) coord)

        (q/line coord grid-start coord extent)
        (q/line grid-start coord extent coord)))

    ;; Draw star points
    (doseq [[x y] (xu/star-points size)]
      (q/stroke-weight 1)
      (q/stroke 0 32)
      (q/fill 0)
      (q/ellipse (+ grid-start (* x cellsize))
                 (+ grid-start (* y cellsize)) 6 6))

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
    (doseq [[pt {:keys [stone] mn :movenumber}] (:board constructed)]
      (let [[x y :as p] (sgf/convert-sgf-coord pt)]
        (when (and p stone)
          (q/stroke-weight 0.5)
          (q/stroke 0)
          (q/fill (if (= stone :white) 255 0))
          (q/ellipse (+ grid-start (* x cellsize))
                     (+ grid-start (* y cellsize)) (- cellsize 2) (- cellsize 2))

          (q/fill (if (= stone :white) 0 255)))

        (when (and (not stone) mn)
          (q/stroke-weight 0)
          (q/stroke 220 179 92)
          (q/fill 220 179 92)
          (q/ellipse (+ grid-start (* x cellsize))
                     (+ grid-start (* y cellsize)) 20 20)

          (q/fill 0))

        (when (and mn (< (- movenumber mn) 40))
          (let [movediff (- movenumber mn)
                movenum (mod (inc mn) 100)
                movecol (get-in move-colours [(int (/ mn 100)) (or stone :black)] [0 0 0])
                movecol (if (> movediff 20) (conj movecol (- 255 (* 255 (/ (- movediff 20) 20)))) movecol)]
            (apply q/fill movecol)
            #_(when (>= (inc movenumber) 100)
              (q/fill 255 0 0))
            (q/text-size 12)
            (q/text-align :center :center)
            (q/text (str movenum) (+ grid-start (* x cellsize)) (- (+ grid-start (* y cellsize)) 1))))))



    (when (:comment lastmove)
      (when (not= (:comment-spoken game) visiblepath)
        (swap! ctx assoc-in [:kifu :comment-spoken] visiblepath)
        (.exec (Runtime/getRuntime) (str "say " (:comment lastmove))))
      (q/text-align :left :top)

      (q/fill 255)
      (q/text-size 12)
      (q/text (first (:comment lastmove)) tx 240 (- (q/width) tx) (q/height)))


    ;; Draw labels
    (doseq [label (:label lastmove)]
      (let [[pt text] (.split label ":" 2)
            [x y :as p] (sgf/convert-sgf-coord pt)
            stone (nth (nth kifu-board y) x)]

        (cond
          (= stone :w)
          (do
            (q/fill 255)
            (q/stroke 255))

          (= stone :b)
          (do
            (q/fill 0)
            (q/stroke 0))

          :else
          (do
            (q/fill 220 179 92)
            (q/stroke 220 179 92)))

        (q/stroke-weight 0)
        (q/ellipse (+ grid-start (* x cellsize))
          (+ grid-start (* y cellsize)) (/ cellsize 1.5) (/ cellsize 1.5))
        (q/fill (if (= stone :b) 255 0))

        (q/text-align :center :center)
        (q/text
          text
          (+ grid-start (* x cellsize))
          (- (+ grid-start (* y cellsize)) 1))))
    #_(println (:triangle lastmove))

    ;; Draw annotated triangles.
    (doseq [pt (:triangle lastmove)]
      (let [[x y :as p] (sgf/convert-sgf-coord pt)
            stone (nth (nth kifu-board y) x)]

        (q/stroke-weight 0)
        (apply q/fill (cond (= stone :b) [0] (= stone :w) [255] :else [220 179 92]))
        (q/ellipse (+ grid-start (* x cellsize))
          (+ grid-start (* y cellsize)) (/ cellsize 1.1) (/ cellsize 1.1))
        (q/stroke-weight 2)
        (q/stroke (if (= stone :b) 255 0))
        (q/triangle
          (+ grid-start (* x cellsize)) (- (+ grid-start (* y cellsize)) 6)
          (- (+ grid-start (* x cellsize)) 6) (+ (+ grid-start (* y cellsize)) 4.5)
          (+ (+ grid-start (* x cellsize)) 6) (+ (+ grid-start (* y cellsize)) 4.5))))

    ;; If in the process of submitting, mark that stone.
    (when submit
      #_(let [[x y _ d] (:move submit)]
        (q/stroke-weight 1)
        (q/stroke 0 128)
        (q/fill (if (= d :w) 255 0) 128)
        (q/ellipse (+ grid-start (* x cellsize))
                   (+ grid-start (* y cellsize)) (- cellsize 3) (- cellsize 3))
        (q/fill (if (= d :w) 0 255))
        (q/text-align :center :center)
        (q/text "?" (+ grid-start (* x cellsize)) (+ grid-start (* y cellsize)))))

    ;; Mark the last move
    (when lastmove
      (let [{:keys [black white]} lastmove]
        (doseq [m (or black white)]
          (let [[x y :as p] (sgf/convert-sgf-coord m)]
            (when p
              (q/stroke (if white 0 255))
              (q/stroke-weight 3)
              (q/fill 0 0)
              (q/ellipse (+ grid-start (* x cellsize))
                (+ grid-start (* y cellsize)) (/ cellsize 2) (/ cellsize 2))))))

      ;; Mark next branches
      (when (:show-branches game)
        (doseq [[idx {:keys [black white]}] (map-indexed vector (:branches lastmove))
                m (or black white)]
          (let [[x y :as p] (sgf/convert-sgf-coord m)]
            (when p
              (if (zero? idx)
                (q/stroke (if white 255 0))
                (apply q/stroke (if white [255 0 0] [0 0 255])))
              (q/stroke-weight 3)
              (q/fill 0 0)
              (q/ellipse (+ grid-start (* x cellsize))
                (+ grid-start (* y cellsize)) (/ cellsize 2) (/ cellsize 2))
              (q/fill 0)
              (when (pos? idx)
                (q/text-size 9)
                (q/text
                  (str idx)
                  (- (+ grid-start (* x cellsize)) 9)
                  (- (+ grid-start (* y cellsize)) 9))))))))

    ;; Highlight differences between constructed and camera board (visual syncing)
    (when (and board kifu-board)
      (doseq [[x y _ _]
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
             (inferrence/reconstruct {:moves (sgf/read-sgf (.getAbsolutePath f)) :movenumber 0 :current-branch-path []})))))

(defn toggle-branches [ctx]
  (swap! ctx update-in [:kifu :show-branches] not))

(defn move-backward [ctx]
  (-> ctx
      (update-in [:kifu :movenumber] (fnil (comp (partial max 0) dec) 1) )
      (assoc-in [:kifu :dirty] true)
      (update-in [:kifu] inferrence/reconstruct)))

(defn move-forward [ctx]
  (let [{:keys [movenumber current-branch-path moves]} (:kifu ctx)
        path (vec (take movenumber (mapcat identity current-branch-path)))
        {:keys [branches] :as node} (last (sgf/current-branch-node-list [path] moves))
        new-branch-path (if (<= (count path) movenumber) [(conj path 0)] current-branch-path)]
    ctx
    (if (zero? (count branches))
      ctx
      (-> ctx
          (update-in [:kifu :movenumber] (fnil inc 1))
          (assoc-in [:kifu :dirty] true)
          (assoc-in [:kifu :current-branch-path] new-branch-path)
          (update-in [:kifu] inferrence/reconstruct)))))

(defn pass [context]
  (let [{:keys [kifu]} context]
    (assoc context
      :kifu
      (inferrence/play-move kifu [-1 -1 nil ({:white :w :black :b} (-> kifu :constructed :player-turn))]))))

(defmethod ui/key-pressed :kifu [ctx]
  (case
    (q/key-code)
    67 (ui/transition ctx :goban)
    86 (ui/transition ctx :view)
    82 (reset-kifu ctx)
    69 (export-sgf ctx)
    76 (load-sgf ctx)
    77 (toggle-branches ctx)
    37 (swap! ctx move-backward)
    39 (swap! ctx move-forward)
    80 (swap! ctx pass)
    (println "Key code not handled: " (q/key-code))))

