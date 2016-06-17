(ns igoki.training
  (:require [clojure.java.io :as io]
            [igoki.ui :as ui]
            [igoki.util :as util]
            [clojure.edn :as edn])
  (:import (java.io File FileInputStream)
           (org.opencv.highgui Highgui)
           (org.opencv.core MatOfByte MatOfPoint2f Mat Size Rect)
           (org.opencv.calib3d Calib3d)
           (org.opencv.imgproc Imgproc)))

(defn file-id [^File img]
  (let [nm (.getName img)]
    (.substring nm 0 (- (.length nm) 4))))

(defn config-file [^File img]
  (let [pn (str (file-id img) ".edn")]
    (io/file (.getParentFile img) pn)))

(defn config-exists? [^File img]
  (.exists (config-file img)))

(defn load-image [^File f]
  (let [result (byte-array (.length f))]
    (with-open [is (FileInputStream. f)]
      (.read is result))
    result))

(defn load-raw [^File img]
  (Highgui/imdecode (MatOfByte. (load-image img)) Highgui/IMREAD_UNCHANGED))

(defn load-next-sample [ctx folder]
  (let [imgfile
        (->>
          (.listFiles (io/file folder))
          (filter #(.endsWith (.toLowerCase (.getName %)) ".jpg"))
          (remove config-exists?)
          first)
        _ (println imgfile)
        raw (load-raw imgfile)
        pimg (util/mat-to-pimage raw)]
    (swap! ctx
      #(-> %
        (assoc :goban {:points [] :size 19}
               :state :goban)
        (update :camera assoc :raw raw :pimg pimg)
        (update :training assoc :file imgfile)))
    (println imgfile "loaded")))

(defn save-current-sample [ctx]
  (let [{:keys [board training view goban]} @ctx
        config
        {:file (.getName (:file training))
         :board board
         :goban goban
         :view (select-keys view [:samplesize :samplecorners :samplepoints])}]
    (spit (config-file (:file training)) (pr-str config))))

(defn target-points [block-size size]
  (let [extent (* block-size size)]
    [[block-size block-size] [extent block-size] [extent extent] [block-size extent]]))

(defn ref-size [block-size size]
  (Size. (* block-size (inc size)) (* block-size (inc size))))

(defn sample-points [corners size]
  (let [[ctl ctr cbr cbl] corners
        divide (fn [[x1 y1] [x2 y2]]
                 (let [xf (/ (- x2 x1) (dec size))
                       yf (/ (- y2 y1) (dec size))]
                   (map (fn [s] [(+ x1 (* s xf)) (+ y1 (* s yf))]) (range size))))
        leftedge (divide ctl cbl)
        rightedge (divide ctr cbr)]
    (map
      (fn [left right] (divide left right))
      leftedge rightedge)))

(defn dump-points [^File imgfile sample-size flat samplepoints board id]
  (doseq [[py rows] (map-indexed vector samplepoints)]
    (doseq [[px [x y]] (map-indexed vector rows)]
      (let [r (Rect. (- x sample-size) (- y sample-size) (* sample-size 2) (* sample-size 2))
            p (get-in board [py px])
            nm (str (.getAbsolutePath (.getParentFile imgfile)) "/samples/" (if p (name p) "e") "/" px "-" py "-" id ".png")]
        (Highgui/imwrite
          nm
          (.submat ^Mat flat r)))))
  samplepoints)

(defn generate-sample-points [settings ^File img]
  (util/with-release
    [target (MatOfPoint2f.)
     origpoints (MatOfPoint2f.)
     ref (Mat.)]
    (let [{{:keys [points size]} :goban :as config} (edn/read-string (slurp (config-file img)))
          raw (load-raw img)
          target (util/vec->mat target (target-points (:block-size settings) size))
          origpoints (util/vec->mat origpoints points)
          samplecorners (target-points (:block-size settings) size)
          samplepoints (sample-points samplecorners size)
          homography (Calib3d/findHomography origpoints target Calib3d/FM_RANSAC 3)]
      (Imgproc/warpPerspective raw ref homography (ref-size (:block-size settings) size))
      (dump-points img (:sample-size settings) ref samplepoints (:board config) (.getName img)))))

(defn generate-all-samples [settings folder]
  (let [f (io/file folder)
        imgs
        (->>
          (.listFiles f)
          (filter #(.endsWith (.toLowerCase (.getName %)) ".jpg"))
          (filter config-exists?))]
    (.mkdirs (File. f "samples/b"))
    (.mkdirs (File. f "samples/e"))
    (.mkdirs (File. f "samples/w"))

    (doseq [i imgs]
      (println "Processing : " (.getAbsolutePath i))
      (generate-sample-points settings i))))

(comment
  (ui/stop-read-loop ui/ctx)

  (load-next-sample ui/ctx "resources/samples/testing")
  (save-current-sample ui/ctx)
  (generate-all-samples {:block-size 35 :sample-size 18} "resources/samples/training"))