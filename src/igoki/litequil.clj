(ns igoki.litequil
  (:import
    (javax.swing JPanel JFrame SwingUtilities WindowConstants)
    (java.awt Graphics2D Container Component Dimension Color Stroke Image BasicStroke RenderingHints Font Rectangle Polygon)
    (java.awt.event MouseListener MouseEvent MouseMotionListener KeyListener KeyEvent WindowStateListener)
    (java.awt.geom Ellipse2D$Double Rectangle2D)))

;; We desperately need to move off Processing. It doesn't compose well, so this implements the same
;; abstractions we use in quil, but directly in a jpanel, which will let us do more UI things
;; in basic Swing later.

(def ^:dynamic ^JPanel panel nil)
(def ^:dynamic ^Graphics2D g2d nil)

(defn input-action [local-panel options e k]
  (let [afn (get options k)]
    (if afn
      (with-bindings
        {#'panel local-panel
         #'g2d (.getGraphics local-panel)}
        (afn e)))))

(defn sketch [options]
  (let [{:keys [title size draw setup]} options

        local-frame (JFrame. ^String title)
        local-panel
        (proxy [JPanel] []
          (paint [^Graphics2D local-g2d]
            (when draw
              (with-bindings
                {#'panel this
                 #'g2d local-g2d}
                (draw)))))]

    (.setFocusable local-panel true)
    (when size
      (let [[w h] size]
        (.setSize local-panel (Dimension. w h))
        (.setPreferredSize local-panel (Dimension. w h))))

    (.addMouseListener local-panel
      (proxy [MouseListener] []
        (mouseClicked [^MouseEvent e]
          (input-action local-panel options e :mouse-clicked))
        (mousePressed [^MouseEvent e]
          (input-action local-panel options e :mouse-pressed))
        (mouseReleased [^MouseEvent e]
          (input-action local-panel options e :mouse-released))
        (mouseEntered [^MouseEvent e]
          (input-action local-panel options e :mouse-entered))
        (mouseExited [^MouseEvent e]
          (input-action local-panel options e :mouse-exited))))

    (.addMouseMotionListener local-panel
      (proxy [MouseMotionListener] []
        (mouseDragged [^MouseEvent e]
          (input-action local-panel options e :mouse-dragged))
        (mouseMoved [^MouseEvent e]
          (input-action local-panel options e :mouse-moved))))

    (.addKeyListener local-panel
      (proxy [KeyListener] []
        (keyPressed [^KeyEvent e]
          (input-action local-panel options e :key-pressed))
        (keyReleased [^KeyEvent e]
          (input-action local-panel options e :key-released))
        (keyTyped [^KeyEvent e]
          (input-action local-panel options e :key-typed))))


    (.add (.getContentPane local-frame) local-panel)
    ;
    (doto local-frame
      (.pack)
      (.setDefaultCloseOperation WindowConstants/EXIT_ON_CLOSE)
      (.setResizable true)
      (.setVisible true))


    (doto
      (Thread.
        #(while (and (.isVisible local-frame))
           (Thread/sleep 250)
           (.repaint local-panel)))

      (.setDaemon true)
      (.start))

    (.grabFocus local-panel)

    (when setup
      (with-bindings
        {#'panel local-panel
         #'g2d (.getGraphics local-panel)}
        (setup)))

    {:panel local-panel
     :frame local-frame
     :options options}))

(defn smooth []
  (doto g2d
    (.setRenderingHint RenderingHints/KEY_ANTIALIASING
      RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint RenderingHints/KEY_FRACTIONALMETRICS
      RenderingHints/VALUE_FRACTIONALMETRICS_ON)
    (.setRenderingHint RenderingHints/KEY_INTERPOLATION
      RenderingHints/VALUE_INTERPOLATION_BICUBIC)))

(defn frame-rate [rate])
(defn width []
  (.getWidth panel))

(defn height []
  (.getHeight panel))

(defn color
  ([g]
   (.setColor g2d (Color. (int g) (int g) (int g))))
  ([g a]
   (.setColor g2d (Color. (int g) (int g) (int g) (int a))))
  ([r g b]
   (.setColor g2d (Color. (int r) (int g) (int b))))
  ([r g b a]
   (.setColor g2d (Color. (int r) (int g) (int b) (int a)))))

(defn background
  ([g]
   (.setBackground g2d (Color. (int g) (int g) (int g))))
  ([g a]
   (.setBackground g2d (Color. (int g) (int g) (int g) (int a))))
  ([r g b]
   (.setBackground g2d (Color. (int r) (int g) (int b))))
  ([r g b a]
   (.setBackground g2d (Color. (int r) (int g) (int b) (int a)))))

(defn stroke-weight [width]
  (.setStroke g2d (BasicStroke. width)))



(defn rect [x y w h]
  (.clearRect g2d x y w h)
  (.drawRect g2d x y w h))

(defn ellipse [x y w h]
  (let [e (Ellipse2D$Double. (- x (/ w 2)) (- y (/ h 2)) w h)
        bg (.getBackground g2d)
        c (.getColor g2d)]
    (.setColor g2d bg)
    (.fill g2d e)
    (.setColor g2d c)
    (.draw g2d e)))

(defn triangle [x1 y1 x2 y2 x3 y3]
  (let [t (doto (Polygon.)
            (.addPoint x1 y1)
            (.addPoint x2 y2)
            (.addPoint x3 y3)
            (.addPoint x1 y1))
        bg (.getBackground g2d)
        c (.getColor g2d)]
    (.setColor g2d bg)
    (.fill g2d t)
    (.setColor g2d c)
    (.draw g2d t)))

(defn line
  ([[x1 y1] [x2 y2]]
   (line x1 y1 x2 y2))
  ([x1 y1 x2 y2]
   (.drawLine g2d x1 y1 x2 y2)))

(defn text-size [size]
  (.setFont g2d
    (.deriveFont (.getFont g2d) (float size))))

(defn calculate-horiz-offset [alignh ^Rectangle2D bounds]
  (case alignh
    :right (- (.getWidth bounds))
    :center (- (/ (.getWidth bounds) 2))
    0))

(defn calculate-vert-offset [alignv ^Rectangle2D bounds]
  (case alignv
    :bottom (- (.getHeight bounds))
    :center (- (/ (.getHeight bounds) 2))
    0))

(defn text [txt x y & [{:keys [align]}]]
  (let [txt (str txt)
        [alignh alignv] align
        render-ctx (.getFontRenderContext g2d)
        metrics (.getFontMetrics g2d)
        font (.getFont g2d)
        bounds (.getStringBounds font txt render-ctx)
        offset-x (calculate-horiz-offset alignh bounds)
        offset-y (calculate-vert-offset alignv bounds)]
    #_(.drawRect g2d (+ offset-x x)
      (+
        offset-y
        #_(.getDescent metrics)
        (+ y #_(.getAscent metrics))) (.getWidth bounds) (.getHeight bounds))

    (.drawString g2d ^String txt
      (int (+ x offset-x))
      (int (+ y offset-y (.getAscent metrics))))))

(defn image [^Image img x y w h]
  (let [scaled (.getScaledInstance img w h Image/SCALE_SMOOTH)]
    (.drawImage g2d scaled (int x) (int y) nil)))

(defn focused []
  (.isFocused
    (SwingUtilities/getWindowAncestor panel)))

(defn mouse-position []
  (.getMousePosition panel))

(defn mouse-x
  ([]
   (let [position (.getMousePosition panel)]
     (if position
       (.getX position)
       0)))
  ([^MouseEvent e]
   (.getX e)))

(defn mouse-y
  ([]
   (let [position (.getMousePosition panel)]
     (if position
       (.getY position)
       0)))
  ([^MouseEvent e]
   (.getY e)))

(defn key-code [^KeyEvent e]
  (.getKeyCode e))

(defn shadow-text
  ([^String s x y]
   (shadow-text s x y :left :bottom))
  ([^String s x y align-horiz]
   (shadow-text s x y align-horiz :bottom))
  ([^String s x y align-horiz align-vert]
   (color 0 196)
   (text-size 20)
   (text s (inc x) (inc y)
     {:align [(or align-horiz :left) (or align-vert :bottom)]})

   (color 255)
   (text-size 20)
   (text s x y
     {:align [(or align-horiz :left) (or align-vert :bottom)]})))

(def fonts
  {"helvetica-20pt" (Font. "Helvetica" Font/PLAIN 20)})

(defn text-font [font-name]
  (let [font (get fonts font-name (Font/decode font-name))]
    (.setFont g2d font)))

