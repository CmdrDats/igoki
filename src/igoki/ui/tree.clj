(ns igoki.ui.tree
  (:require
    [seesaw.core :as s]
    [seesaw.border :as sb]
    [clojure.pprint :as ppr]
    [clojure.tools.logging :as log]
    [seesaw.color :as sc])
  (:import (java.awt Graphics2D)))


(defn prerender-branch
  ([idx node]
   (println idx)
   (let [n
         {:x (+ 10 (* idx 7))
          :y (or (:y node) 10)
          :width 7
          :height 10
          :colour :red}

         branches
         (reduce
           (fn [acc b]
             (let [r
                   (prerender-branch (inc idx)
                     (if (> idx 500)
                       (->
                         b
                         (assoc :y (:y acc))
                         (dissoc :branches))

                       (->
                         b
                         (assoc :y (:y acc)))))]
               (->
                 acc
                 (update :height + 2 (:height r))
                 (update :width max (:width r))
                 (update :y + 2 (:height r))
                 (update :nodes conj r))))
           {:y (:y n)
            :height 0
            :width 0
            :nodes []}
           (:branches node))]

     #_(println (:y branches))
     (assoc n
       :height (max (:height branches) (:height n))
       :width (+ (:width branches) (:width n))
       :branches (when-not (empty? (:nodes branches)) (:nodes branches))
       :idx idx
       :colour
       (cond
         (:black node) :black
         (:white node) :white
         :else :red)))))

#_(defn node-labels [{:keys [x y width height branches colour]}]
  (concat
    [(s/label
       :background colour :border (sb/line-border :color :black :thickness 1)
       :text " " :bounds [x y 7 10])]
    (mapcat node-labels branches)))

(defn paint-tree [ctx node c ^Graphics2D g]
  (.setColor g (sc/color (:colour node)))
  (.fillRect g (:x node) (:y node) 7 10)
  (.setColor g (sc/color :black))
  (.drawLine g (:x node) (:y node) (+ 7 (:x node)) (:y node))
  (.drawLine g (:x node) (+ 10 (:y node)) (+ 7 (:x node)) (+ 10 (:y node)))
  (doseq [n (:branches node)]
    (paint-tree ctx n c g)))

(defn rendered-tree-panel [ctx]
  #_(s/flow-panel :id :rendered-tree
    :items [(s/label :text "hi" :bounds [10 10 100 100])
            (s/button :text "fdsafdsafdfd")])
  (let [prerendered (prerender-branch 0 (get-in @ctx [:kifu :moves]))
        xyz
        (s/canvas
          :paint (partial #'paint-tree ctx prerendered)
          :size [(:width prerendered) :by (:height prerendered)])]
    (s/scrollable
      xyz
      :id :rendered-tree
      :hscroll :always
      :vscroll :always)))

(defn tree-panel [ctx]
  (let [tree (rendered-tree-panel ctx)
        container
        (s/border-panel
          :center tree
          :south
          (s/flow-panel
            :items
            [(s/button :text "Refresh" :id :tree-refresh)]))]
    (s/listen (s/select container [:#tree-refresh])
      :action
      (fn [e]
        (s/replace! container (s/select container [:#rendered-tree])
          (rendered-tree-panel ctx))))

    container))