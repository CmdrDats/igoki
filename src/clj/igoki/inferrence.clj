(ns igoki.inferrence
  (:require
    [igoki.util :as util]
    [igoki.sgf :as sgf]))

(defn simple-board-view [{:keys [board size]}]
  (let [[width height] size]
    (for [y (range height)]
      (for [x (range width)]
        (case (get-in board [(sgf/convert-coord x y) :stone])
          :white :w :black :b nil)))))

(defn reconstruct [{:keys [moves current-branch-path movenumber] :as game}]
  (let [visiblepath (vec (take movenumber (mapcat identity current-branch-path)))
        constructed (sgf/construct-board moves [visiblepath])]
    (assoc game
      :constructed constructed
      :kifu-board (simple-board-view constructed))))

(defn play-move [{:keys [moves current-branch-path movenumber] :as game} [x y o n :as move]]
  (let [visiblepath (vec (take movenumber (mapcat identity current-branch-path)))
        [node path] (sgf/collect-node moves {(if (= n :b) :black :white) [(sgf/convert-coord x y)]} [visiblepath])
        updatedgame
        (->
          game
          (assoc :moves node :dirty false :current-branch-path path)
          (update :movenumber (fnil inc 0)))]
    (reconstruct updatedgame)))

(defn print-boards [& boards]
  (println
    (apply str
      (interpose "\n"
        (apply map
          #(apply str
             (interpose " | "
               (for [m %&]
                 (apply str (map (fn [i] (str " " (if i (name i) "."))) m)))))
          boards)))))

(defn walk-boards [cellfn & boards]
  (apply map
    (fn [& rs]
      (apply map cellfn rs))
    boards))

(defn subtract-board [a b]
  (walk-boards
    (fn [ca cb]
      (if cb nil ca)) a b))

(defn mask-board [a b]
  (walk-boards
    (fn [ca cb]
      (if (and ca cb) ca)) a b))

(defn point-map [board]
  (for [[y rows] (map-indexed vector board)
        [x cell] (map-indexed vector rows)
        :when cell]
    [x y cell]))

(defn clean-updatelist
  [initial updatelist final-position]
  (->>
    ;; We need to propagate the final-position backward
    (reverse updatelist)

    ;; Go through the board snapshot, ripping off any cells that were previously nil
    ;; so that only the stuff that stays put for the duration of the game is left
    (reduce
      (fn [[a result] u]
        (let [f (mask-board a u)]
          [f (conj result f)]))
      [(subtract-board final-position initial) []])
    ;; Get the result
    second
    ;; Remove some noise
    dedupe
    ;; Back to original direction (probably not needed)
    reverse
    ;; Convert into [x y c] point vectors
    (mapcat point-map)
    frequencies
    (group-by #(nth (first %) 2))
    (map (fn [[k v]] [k (map first (reverse (sort-by second v)))]))
    (into {})))

(defn infer-moves
  "From a base game state, a board updatelist and a final-position, infer the move
   sequence.

   The meat of the algorithm is the clean-updatelist - Once that's done, it can
   just interleave the black and white moves and play them to see if it ends up
   as the final-position. Will return nil if a match didn't come out of the woodwork."
  [game updatelist final-position]
  (let [{:keys [kifu-board constructed]} game
        {:keys [b w] :as clean} (clean-updatelist kifu-board updatelist final-position)
        moves (apply util/interleave-all (if (= (:player-turn constructed) :black) [b w] [w b]))
        inferred
        (reduce
          (fn [g [x y cell]]
            (if (= (-> g :constructed :player-turn) ({:b :black :w :white} cell))
              (play-move g [x y nil cell])
              g))
          game
          moves)]
    (print-boards (:kifu-board game) (:kifu-board inferred) final-position)
    (if (= (:kifu-board inferred) final-position) inferred)))