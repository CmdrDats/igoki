(ns igoki.sgf
  (:require [clojure.set :as set]))

;; TODO: Handle passes in board reconstruction (update :player-turn on empty or invalid W or B position)


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
   "C" :comment
   "DM" :position-even
   "GB" :position-bias-black
   "GW" :position-bias-white
   "HO" :position-hotspot
   "N" :name
   "UC" :position-unclear
   "V" :value-to-white                                      ; Negative is good for black

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
   "VW" :view                                               ; show only listed points or reset if empty
   })

(def reverse-property-lookup
  (into {} (map (fn [[k v]] [v k]) property-lookup)))

(defn convert-coord [x y]
  (str (char (+ 97 x)) (char (+ 97 y))))

(defn convert-sgf-coord [[x y :as s]]
  [(- (int x) 97) (- (int y) 97)])


(defn current-branch-node-list [[p & ps :as path] moves]
  (cond
    (empty? (filter vector? moves)) moves
    :else
    (concat
      (remove vector? moves)
      (if p
        (current-branch-node-list ps (nth moves (or p 0)))
        (current-branch-node-list ps (first (filter vector? moves)))))))


(defn branch-at [moves movecount]
  (cond
    (zero? movecount) moves
    (> (count (remove vector? moves)) movecount)
    (vec (concat (subvec moves 0 movecount) [(subvec moves movecount)]))
    :else moves))

(defn branch-path [[p & ps :as path] moves movecount]
  (let [newcount (- movecount (count (remove vector? moves)))]
    (cond
      (nil? moves) [[] []]
      (or (nil? p) (zero? newcount) (neg? newcount))
      (let [branched (branch-at moves movecount)]
        [[(count branched)] (conj branched [])])
      (zero? newcount)
      [path moves]
      :else
      (let [[bp branched] (branch-path ps (nth moves p) (- movecount (count (remove vector? moves))))]
        [(concat [(first path)] bp) (assoc moves p branched)]))))

(defn branch [{:keys [current-branch-path moves] :as game}]
  (let [[newpath branched] (branch-path current-branch-path moves (count (current-branch-node-list current-branch-path moves)))]
    (assoc game :current-branch-path newpath :moves branched))

  #_(cond
    ;; Root node
    (nil? (:moves game))
    (assoc game :moves {} :current-branch-path [])
    :else
    (let [current-path (get-in (:moves game) (:current-branch-path game) [])]
      (->
        game
        (update-in (concat [:moves] (:current-branch-path game)) conj [])
        (update :current-branch-path #(conj % (count current-path)))))))

(defn unbranch [game]
  #_(println "Unbranching")
  (update game :current-branch-path #(vec (butlast %))))

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

(defn collect-node [game node]
  (if (empty? node)
    game
    (update-in game (concat [:moves] (:current-branch-path game)) conj node)))

(defn read-sgf [filename]
  (let [f (slurp filename)
        initial-state {:mode nil :action "" :node {}}]
    (loop [game {:moves nil}
           state initial-state
           [c & o] f]
      (cond
        (nil? c) game
        (and (= c \\) (= (:mode state) :collect-text)) (recur game state o) ;; Escape character in text.
        (= c \]) (recur game (accumulate-action state) o)
        (= (:mode state) :collect-text) (recur game (collect-text state c) o)
        (= c \() (recur (-> game (collect-node (:node state)) branch) initial-state o)
        (= c \)) (recur (-> game (collect-node (:node state)) unbranch) initial-state o)
        (= c \[) (recur game (assoc state :mode :collect-text :text "") o)
        (= c \;) (recur (-> game (collect-node (:node state)) ) initial-state o)
        (Character/isUpperCase ^char c) (recur game (collect-action state c) o)
        :else
        (recur game state o)
        ))))

(defn node-to-sgf [node]
  (apply
    str
    ";"
    (mapcat
      (fn [[k v]]
        (str
          (get reverse-property-lookup k k)
          "[" (apply str (interpose "][" v)) "]"))
      node)))

(defn sgf-move-tree [moves]
  (cond
    (vector? moves)
    (str
      "("
      (apply str (map sgf-move-tree moves))
      ")")
    (map? moves) (node-to-sgf moves)
    :else ""))

(defn sgf [game]
  (sgf-move-tree (:moves game)))


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
  (println "Unhandled property: " k))

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

(defn construct-board [moves path movenumber]
  (let [nodelist (take movenumber (current-branch-node-list path moves))]
    (reduce step-node {:size [19 19] :player-turn :black :movenumber 0 :moveoffset 0} nodelist)))