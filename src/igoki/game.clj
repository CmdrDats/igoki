(ns igoki.game
  (:require
    [igoki.util :as util]
    [igoki.sgf :as sgf]
    [igoki.inferrence :as inferrence]
    [igoki.sound.sound :as snd]
    [igoki.sound.announce :as announce])
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
            ;; TODO: this read-delay.. whaat?? this should be a 'accept-delay' at this level.
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
  (let [{{:keys [kifu-board dirty] :as game} :kifu
         ogs :ogs} @ctx

        nodes (sgf/current-branch-node-list (:current-branch-path game) (:moves game))
        lastmove (last nodes)
        [[_ _ mo mn :as mv] :as diff] (board-diff kifu-board board)

        ;; Disabled temporarily for issues with online integration.
        ;; TODO: This causes issues for online integration, obviously, so will
        ;; need to check if undo/move is all
        history-game
        (when-not (:gameid ogs)
          (if (and lastmove (> (count diff) 0)) (get (board-history game) board)))]
    (cond
      (and (empty? diff) dirty)
      (do
        (println "Clean state, marking as such.")
        (swap! ctx assoc-in [:kifu :dirty] false))

      (and (not (empty? diff)) dirty)
      (println "Not actioning board updates until clean state is reached")

      ;; Special case to undo last move
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
  (let [{{{:keys [latch board] :as submit} :submit
          :keys [filename camidx last-dump] :as game} :kifu
         {:keys [raw]} :camera
         debug-capture :debug-capture
         cboard :board} @ctx

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
        ;; TODO: Sound playing shouldn't happen here, surely?
        (snd/play-sound :submit)
        (println "Debounce success - move submitted")

        (let [new (inferrence/infer-moves game updatelist (last updatelist))]
          (if (and new (not= (:kifu-board new) (:kifu-board game)))
            (do
              (snd/play-sound :click)

              (announce/comment-move ctx
                (last
                  (sgf/current-branch-node-list
                    (take (:movenumber new) (:current-branch-path new)) (:moves new)))
                (:constructed new))
              (reset! captured-boardlist [])
              (swap! ctx assoc :kifu (assoc (dissoc new :submit) :cam-update true)))
            (swap! ctx update :kifu #(assoc (dissoc % :submit) :cam-update true))))
        (swap! ctx update :camera dissoc :read-delay)))

    ;; Dump camera on a regular basis, ignore time if board is updated.
    #_(println last-dump t (- t last-dump) (> (- t last-dump 20e9)))
    (when (or (get-in @ctx [:kifu :cam-update])
            (nil? last-dump)
            (> (- t last-dump) 20e9))
      (when debug-capture
        (dump-camera filename camidx raw updatelist))
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
        size
        (or (-> context :goban :size) 19)
        board (or (-> context :board) [])
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
              :application [(str "igoki v" (System/getProperty "igoki.version"))]
              :file-format ["4"]
              :gametype ["1"]
              :size [size]
              :date [(.format (SimpleDateFormat. "YYYY-MM-dd") (Date.))]
              :komi ["5.5"]}
             board)

           :movenumber 0
           :current-branch-path [[]]}
          inferrence/reconstruct)]

    (when-not (.exists (File. "capture"))
      (.mkdir (File. "capture")))

    (when (:debug-capture ctx)
      (util/zip-add-file-string
        (:filename new-game)
        (str camidx ".config.edn")
        (pr-str
          {:board (:board context)
           :goban (:goban context)
           :view (dissoc (:view context) :homography)
           :kifu new-game}))
      (dump-camera (:filename new-game) camidx (-> context :camera :raw) [board]))
    (swap! ctx assoc :kifu new-game :filename "game.sgf")))


;; In hundreds..
(defn find-last-move [ctx]
  (let [{{:keys [movenumber] :as game} :kifu} @ctx
        visiblepath (if movenumber (take movenumber (mapcat identity (:current-branch-path game))))
        actionlist (if visiblepath (sgf/current-branch-node-list [visiblepath] (:moves game)))]
    (last actionlist)))

(defn convert-sgf [ctx]
  (sgf/sgf (:moves (:kifu @ctx))))

(defn load-sgf [ctx file]
  (let [sgf-string (slurp file)
        moves (sgf/read-sgf sgf-string)]
    (swap! ctx assoc
      :kifu
      (inferrence/reconstruct
        {:moves moves :movenumber 0 :current-branch-path []})
      :current-file file)))

(defn toggle-branches [ctx show-branches?]
  (swap! ctx assoc-in [:kifu :show-branches] show-branches?))

(defn move-backward [ctx]
  (swap! ctx
    #(->
       %
       (update-in [:kifu :movenumber] (fnil (comp (partial max 0) dec) 1))
       (assoc-in [:kifu :dirty] true)
       (update-in [:kifu] inferrence/reconstruct))))

(defn move-forward [ctx]
  (let [{:keys [movenumber current-branch-path moves]} (:kifu @ctx)
        path (vec (take movenumber (mapcat identity current-branch-path)))
        {:keys [branches] :as node} (last (sgf/current-branch-node-list [path] moves))
        new-branch-path (if (<= (count path) movenumber) [(conj path 0)] current-branch-path)]
    (cond
      (zero? (count branches))
      ctx

      :else
      (swap! ctx update-in [:kifu]
        #(->
           %
           (update :movenumber (fnil inc 1))
           (assoc :dirty true :current-branch-path new-branch-path)
           (inferrence/reconstruct))))))

(defn pass [ctx]
  (swap! ctx
    (fn [{:keys [kifu] :as state}]
      (assoc state
        :kifu
        (inferrence/play-move kifu
          [-1 -1 nil
           ({:white :w :black :b}
            (-> kifu :constructed :player-turn))])))))


;; TODO: Not happy with this
;; The add-watch creates an implicit binding to data structure which makes the
;; whole thing hard to reason about.
(defn init [ctx]
  (when-not (-> @ctx :kifu)
    (TVFS/umount)
    (reset-kifu ctx))
  (util/add-watch-path ctx :kifu-camera [:camera :raw] #'camera-updated)
  (util/add-watch-path ctx :kifu-board [:board] #'board-updated))