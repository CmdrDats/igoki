(ns igoki.ui.tree
  (:require
    [seesaw.core :as s]
    [seesaw.border :as sb]
    [clojure.pprint :as ppr]
    [clojure.tools.logging :as log]))


#_(defn prerender-branch
  ([node]
   (prerender-branch nil
     {:x 0
      :y 0
      :width 7
      :height 10
      :colour :red}
     node))
  ([parent node]
   (cond
     (nil? node) nil


     )
   (let [basenode
         {:x (+ (:x parent) (:width parent))
          :y (:y parent)
          :width 7 :height 10
          :colour :red}

         root
         (cond
           (nil? node)
           nil

           (:black node)
           (assoc basenode :colour :black)


           (:white node)
           (assoc basenode :colour :white)

           :else
           basenode)

         children
         (map-indexed
           (fn [idx child]
             (prerender-branch (update root :y + (* idx 10)) child))
           (:branches node))]
     (assoc root
       :width (max ))
     )))

(defn rendered-tree-panel [ctx]
  (println "hello?")
  #_(s/flow-panel :id :rendered-tree
    :items [(s/label :text "hi" :bounds [10 10 100 100])
            (s/button :text "fdsafdsafdfd")])
  (let [#_#_prerendered (prerender-branch nil (get-in @ctx [:kifu :moves]))
        xyz
        (s/xyz-panel
          :size [1100 :by 100]
          :items [(s/label :background :white :border (sb/line-border :color :black :thickness 1) :text "hi" :bounds [1000 10 100 100])])]
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