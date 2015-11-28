(ns igoki.sgf
  (:require
    [igoki.util :as util]
    [clojure.set :as set]))

;; According to http://www.red-bean.com/sgf/properties.html
(def property-lookup
  {
   ;; Moves
   "B"  :black
   "KO" :ko
   "MN" :move-number
   "W"  :white

   ;; Setup
   "AB" :add-black
   "AE" :add-erase
   "AW" :add-white
   "PL" :player-start

   ;; Annotation
   "C"  :comment
   "DM" :position-even
   "GB" :position-bias-black
   "GW" :position-bias-white
   "HO" :position-hotspot
   "N"  :name
   "UC" :position-unclear
   "V"  :value-to-white                                     ; Negative is good for black

   ;; Move annotation
   "BM" :move-bad
   "DO" :move-doubtful
   "IT" :move-interesting
   "TE" :move-tesuji

   ;; Markup
   "AR" :arrow                                              ; 'from:to' in value
   "CR" :circle
   "DD" :grayout
   "LB" :label                                              ; 'point:label'
   "LN" :line                                               ; 'from:to'
   "MA" :mark                                               ; with X
   "SL" :selected
   "SQ" :square
   "TR" :triangle
   "TB" :territory-black
   "TW" :territory-white

   ;; Root info
   "AP" :application
   "CA" :charset
   "FF" :file-format
   "GM" :gametype                                           ; 1=Go
   "ST" :variation-show-type
   "SZ" :size

   ;; Game info
   "AN" :annotator
   "BR" :black-rank
   "BT" :black-team
   "CP" :copyright
   "DT" :date
   "EV" :event
   "GN" :game-name
   "GC" :game-comment
   "ON" :opening
   "OT" :overtime-method
   "PB" :black-name
   "PC" :place
   "PW" :white-name
   "RE" :result                                             ; [WB][+]([RTF](esign|ime|orfeit)?)?, 0, Draw, Void, '?'
   "RO" :round
   "RU" :rules
   "SO" :source
   "US" :user
   "WR" :white-rank
   "WT" :white-team
   "HA" :handicap
   "KM" :komi

   ;; Timing
   "BL" :black-time-left
   "OB" :black-moves-left
   "OW" :white-moves-left
   "WL" :white-time-left

   ;; Miscellaneous
   "FG" :print-figure
   "PM" :print-move-numbers
   "VW" :view}                                              ; show only listed points or reset if empty
  )

(def reverse-property-lookup
  (into {} (map (fn [[k v]] [v k]) property-lookup)))

(defn convert-coord [x y]
  (if (or (neg? x) (neg? y))
    ""
    (str (char (+ 97 x)) (char (+ 97 y)))))

(defn convert-sgf-coord [[x y :as s]]
  (when (and x y)
    [(- (int x) 97) (- (int y) 97)]))

(defn inpath [branch-path]
  (concat [:branches] (mapcat (fn [i] [i :branches]) (mapcat identity branch-path))))

(defn current-branch-node-list [path rootnode]
  (reductions
    (fn [node p]
      (get (:branches node) p))
    rootnode (mapcat identity path)))

(defn accumulate-action [{:keys [action text node] :as state}]
  (let [a (get property-lookup action action)]
    (-> state
        (update-in [:node a] #(conj (or % []) text))
        (assoc :mode :collected))))

(defn collect-text [state c]
  (update state :text str c))

(defn collect-action [state c]
  (if (= (:mode state) :collected)
    (assoc (dissoc state :mode) :action (str c))
    (update state :action str c)))

(defn find-existing-branch [branches {:keys [black white]}]
  (->>
    (map-indexed vector branches)
    (filter (fn [[_ n]] (or (and black (= (:black n) black))
                            (and white (= (:white n) white)))))
    first))

(defn collect-node [game node branch-path & [branch?]]
  (let [branches (get-in game (inpath branch-path))
        [idx branch] (find-existing-branch branches node)]
    (cond
      (empty? node)
      [game (if branch? (conj branch-path []) branch-path)]

      idx
      [(util/iupdate-in game (conj (inpath branch-path) idx) merge node)
       (cond->
         branch-path true (update (dec (count branch-path)) conj idx)
         branch? (conj []))]

      :else
      [(util/iupdate-in game (inpath branch-path) (fnil conj []) node)
       (let [newpoint (count (get-in game (inpath branch-path)))]
         (cond->
           branch-path
           true (update (dec (count branch-path)) conj newpoint)
           branch? (conj [])))])))

(defn read-sgf [filename]
  (let [f (slurp filename)
        new-node {:branches []}
        initial-state {:mode nil :action "" :node nil}]
    (loop [[game branch-path :as g] [new-node []]
           state initial-state
           [c & o] f]
      (cond
        (nil? c)
        ;; TODO: This picks the first branch in an sgf as the root, might need to show a list
        ;; of games instead if the SGF actually has multiple root branches.
        (first (:branches game))

        (and (= c \\) (= (:mode state) :collect-text))
        (recur g state o) ;; Escape character in text.

        (= c \])
        (recur g (accumulate-action state) o)

        (= (:mode state) :collect-text)
        (recur g (collect-text state c) o)

        (= c \[)
        (recur g (assoc state :mode :collect-text :text "") o)

        (Character/isUpperCase ^char c)
        (recur g (collect-action state c) o)

        (= c \;)
        (do
          (recur (collect-node game (:node state) branch-path) initial-state o))

        (= c \()
        (recur (collect-node game (:node state) branch-path true) initial-state o)

        (= c \))
        (let [[g np] (collect-node game (:node state) branch-path)]
          (recur [g (vec (butlast np))] initial-state o))
        :else
        (recur g state o)
        ))))

(defn node-to-sgf [node]
  (let [nodestr
        (apply str
               (mapcat
                 (fn [[k v]]
                   (str
                     (get reverse-property-lookup k k)
                     "[" (apply str (interpose "][" v)) "]"))
                 (dissoc node :branches)))]
    (str
      ";" nodestr
      (cond
        (nil? (:branches node)) ""
        (> (count (:branches node)) 1) (str "(" (apply str (interpose ")(" (map node-to-sgf (:branches node)))) ")")
        :else (node-to-sgf (first (:branches node)))))))

(defn sgf [root]
  (str "(" (node-to-sgf root) ")"))


;; == Board construction ==
(defmulti
  step-action
  (fn [b k v]
    (cond
      (#{:annotator :black-rank :black-team :copyright :date :event :game-name :game-comment
         :opening :overtime-method :black-name :place :white-name :result :round :rules :source
         :user :white-rank :white-team :handicap :komi :application :charset :file-format :gametype
         :variation-show-type :comment :name :value-to-white} k)
      :annotate-game
      (#{:circle :mark :selected :square :triangle :territory-black :territory-white} k)
      :markup
      (#{:arrow :line} k)
      :line
      (#{:move-bad :move-doubtful :move-interesting :move-tesuji} k)
      :movequality
      (#{:position-even :position-bias-black :position-bias-white :position-hotspot :position-unclear} k)
      :positionquality
      :else k)))

(defmethod step-action :default [b k v]
  (println "Unhandled property: " k)
  b)

;; Annotations and metadata guck
(defn rectangle-point-list [v]
  (let [[[fx fy :as fp] tp] (.split v ":")
        [tx ty] (or tp fp)]
    (for [x (range (int fx) (inc (int tx)))
          y (range (int fy) (inc (int ty)))]
      (str (char x) (char y)))))

(defmethod step-action :annotate-game [b k v]
  (assoc b k v))

(defmethod step-action :move-number [b k v]
  (assoc b :move-offset (Integer/parseInt v)))

(defmethod step-action :markup [b k v]
  (update b :annotations
          concat (map (fn [sv] {:type k :point sv}) (rectangle-point-list v))))

(defmethod step-action :line [b k v]
  (let [[f t] (seq (.split (str v) ":"))]
    (update b :annotations conj {:type k :from f :to t})))

(defmethod step-action :label [b k v]
  (let [[p l] (seq (.split (str v) ":"))]
    (update b :annotations conj {:type k :point p :text l})))

(defmethod step-action :movequality [b k v]
  (assoc b :movequality [k v]))

(defmethod step-action :positionquality [b k v]
  (assoc b :positionquality [k v]))

(defmethod step-action :size [b k v]
  (let [[width height] (map #(Integer/parseInt %) (seq (.split (str v) ":")))
        height (or height width)]
    (assoc b :size [width height])))

(defmethod step-action :player-start [b k v]
  (let [v (if (= v "W") :white :black)]
    (assoc b
      :player-start v
      :player-turn v)))

;; Onto actual interesting board related stuff.
(defn set-color [color b k v]
  ;; Does not consider board size, simply adds it since the board size might be changed.
  (reduce #(assoc-in %1 [:board %2 :stone] color) b (rectangle-point-list v)))

(defmethod step-action :add-black [b k v]
  (set-color :black b k v))

(defmethod step-action :add-white [b k v]
  (set-color :white b k v))

(defmethod step-action :add-erase [b k v]
  (set-color nil b k v))

(defn inside-point? [{[width height] :size :as board} p]
  (let [a (int \a)
        [x y] (map int p)]
    (and
      (= (count p) 2)
      (>= x a) (< x (+ a width))
      (>= y a) (< y (+ a height)))))

(defn neighbour-points
  "Return a set of neighbour coords for a given point on a board, taking edges and corners into account"
  [board p]
  (when p
    (let [[x y] (map int p)]
      (->>
        [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
        (filter (partial inside-point? board))
        (map (fn [[x y]] (str (char x) (char y))))
        set))))

(defn find-group [board point]
  (let [color (get-in board [:board point :stone])]
    (loop [[p & po] [point]
           group #{}]
      (let [neighbours (set/difference (neighbour-points board p) group)]
        (cond
          (nil? p) group
          (= color (get-in board [:board p :stone]))
          (recur (concat po neighbours) (conj group p))
          :else
          (recur po group))))))

(defn count-liberties [board group]
  (let [neighbours (set/difference (set (mapcat (partial neighbour-points board) group)) group)
        liberties (filter nil? (map #(get-in board [:board % :stone]) neighbours))]
    (count liberties)))

(defn group-alive? [board group]
  (pos? (count-liberties board group)))

(defn remove-captured-group [board group]
  (let [color (get-in board [:board (first group) :stone])]
    (->
      (reduce #(assoc-in %1 [:board %2 :stone] nil) board group)
      (update-in [:captures (if (= color :white) :black :white)] (fnil + 0) (count group)))))

(defn check-capture-around [board color point]
  (let [opp-color (if (= color :white) :black :white)
        neighbours (neighbour-points board point)
        opp-points (filter #(= opp-color (get-in board [:board % :stone])) neighbours)
        captured-groups (remove (partial group-alive? board) (set (map (partial find-group board) opp-points)))]
    (reduce remove-captured-group board captured-groups)))

(defn check-suicide [board point]
  (let [group (find-group board point)]
    (if (group-alive? board group)
      board
      (remove-captured-group board group))))

(defn place-stone [board color point]
  ;; If the stone is 'placed' outside the board (either '' or 'tt') then count as a 'pass'
  (if (inside-point? board point)
    (-> board
        (assoc-in [:board point :stone] color)
        (assoc-in [:board point :movenumber] (:movenumber board))
        (check-capture-around color point)
        (check-suicide point))
    (assoc board :player-passed color)))

(defn move [board color point]
  (-> board
      (assoc :player-turn (if (= color :white) :black :white))
      (place-stone color point)
      (update :movenumber (fnil inc 0))))

(defmethod step-action :black [b k v]
  (move b :black v))

(defmethod step-action :white [b k v]
  (move b :white v))

(defmethod step-action :branches [b k v]
  b)

;; Tying it all together.

(defn step-node [board node]
  (reduce
    (fn [b [k v]]
      (reduce #(step-action %1 k %2) b v))
    (dissoc
      board
      :player-passed
      :name
      :comment
      :movequality
      :positionquality
      :annotations
      :value-to-white)
    node))

(defn construct-board [rootnode path]
  (let [nodelist (current-branch-node-list path rootnode)]
    (reduce step-node {:size [19 19] :player-turn :black :movenumber 0 :moveoffset 0} nodelist)))