(ns igoki.game
  (:require
    [igoki.ui :as ui]
    [igoki.view :as view]
    [igoki.util :as util]
    [igoki.sgf :as sgf]
    [igoki.inferrence :as inferrence]
    [igoki.sound.sound :as snd]
    [igoki.litequil :as lq])
  (:import
    (java.io File ByteArrayInputStream)
    (java.util Date UUID)
    (java.text SimpleDateFormat)
    (org.opencv.core MatOfByte)
    (de.schlichtherle.truezip.file TVFS)
    (org.opencv.imgcodecs Imgcodecs)))

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
  (when (and raw filename)
    (util/with-release [out (MatOfByte.)]
      (println "Writing jpg: " filename "/" (str camidx ".jpg"))
      (Imgcodecs/imencode ".jpg" raw out)
      (util/zip-add-file filename (str camidx ".jpg") (ByteArrayInputStream. (.toArray out)))
      (util/zip-add-file-string filename (str camidx ".edn") (pr-str updatelist))
      (println "Done writing jpg: " filename "/" (str camidx ".jpg")))))


(defn camera-updated [wk ctx old new]
  (view/camera-updated ctx)
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
            (> (- t last-dump) 20e9))
      (dump-camera filename camidx raw updatelist)
      (swap! ctx update :kifu
        #(-> %
             (assoc :cam-update false :last-dump t)
             (update :camidx (fnil inc 0)))))))


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
          {:filename camfile
           :camidx (inc camidx)
           :moves
           (add-initial-points
             {:branches []
              :player-start ["B"]
              :application ["Igoki"]
              :file-format ["4"]
              :gametype ["1"]
              :size [(-> @ctx :goban :size)]
              :date [(.format (SimpleDateFormat. "YYYY-MM-dd") (Date.))]
              :komi ["5.5"]}
             board)

           :movenumber 0
           :current-branch-path [[]]}
          inferrence/reconstruct)]

    (when-not (.exists (File. "capture"))
      (.mkdir (File. "capture")))

    (util/zip-add-file-string
      (:filename new-game)
      (str camidx ".config.edn")
      (pr-str
        {:board (:board context)
         :goban (:goban context)
         :view (dissoc (:view context) :homography)
         :kifu new-game}))
    (dump-camera (:filename new-game) camidx (-> context :camera :raw) [board])
    (swap! ctx assoc :kifu new-game :filename "game.sgf")))




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


(defn export-sgf [ctx]
  (ui/save-dialog
    (:current-file @ctx)
    #(spit % (sgf/sgf (-> @ctx :kifu :moves)))))

(defn load-sgf [ctx]
  (ui/load-dialog
    (fn [^File f]
      (println "Opening sgf: " (.getAbsolutePath f))
      (swap! ctx assoc
        :kifu
        (inferrence/reconstruct
          {:moves (sgf/read-sgf f) :movenumber 0 :current-branch-path []})
        :current-file f))))

(defn toggle-branches [ctx show-branches?]
  (swap! ctx assoc-in [:kifu :show-branches] show-branches?))

(defn move-backward [ctx]
  (-> ctx
      (update-in [:kifu :movenumber] (fnil (comp (partial max 0) dec) 1))
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



;; UI Code from here.


(defn construct [ctx]
  (when-not (-> @ctx :kifu)
    (TVFS/umount)
    (reset-kifu ctx))
  (util/add-watch-path ctx :kifu-camera [:camera :raw] #'camera-updated)
  (util/add-watch-path ctx :kifu-board [:board] #'board-updated))

(defn destruct [ctx]
  (remove-watch ctx :kifu-camera)
  (remove-watch ctx :kifu-board))


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
        visiblepath (take movenumber (mapcat identity (:current-branch-path game)))
        actionlist (sgf/current-branch-node-list [visiblepath] (:moves game))
        lastmove (last actionlist)
        canvas-size (max 250 (min (lq/width) (lq/height)))]

    (lq/shadow-text (str "Recording: Img #" (:camidx game)) tx 25)
    (when (:filename game)
      (lq/shadow-text (:filename game) tx 50))
    (lq/shadow-text (str "Move " (inc movenumber) ", " (if (= (:player-turn constructed) :black) "Black" "White") " to play") tx 75)
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
                movecol (if (> movediff 20) (conj movecol (- 255 (* 255 (/ (- movediff 20) 20)))) movecol)]
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
              (board-diff kifu-board board)]
        (lq/stroke-weight 3)
        (lq/color 255 0 0)
        (lq/background 0 0)
        (lq/ellipse (+ grid-start (* x cellsize))
          (+ grid-start (* y cellsize)) (- cellsize 3) (- cellsize 3))))))


(defn key-typed [ctx e]
  (case (lq/key-code e)
    37 (swap! ctx move-backward)
    39 (swap! ctx move-forward)
    80 (swap! ctx pass)
    (println "Key code not handled: " (lq/key-code e))))


(defn game-panel [ctx]
  (:panel
    (lq/sketch-panel
      {:setup (partial #'construct ctx)
       :close (partial #'destruct ctx)
       :draw (partial #'draw ctx)
       #_#_:mouse-dragged (partial #'mouse-dragged ctx)
       #_#_:mouse-pressed (partial #'mouse-pressed ctx)
       #_#_:mouse-released (partial #'mouse-released ctx)
       :key-typed (partial #'key-typed ctx)})))