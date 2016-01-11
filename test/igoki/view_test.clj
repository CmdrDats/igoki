(ns igoki.view
  (:require
    [clojure.test :refer :all]
    [clojure.edn :as edn])
  (:import
    (de.schlichtherle.truezip.fs FsEntryNotFoundException)
    (org.opencv.highgui Highgui)
    (org.opencv.core MatOfByte)))

(defn load-reference [ctx]
  (assoc ctx :view (gather-reference ctx (-> ctx :view :homography))))

(defn load-current-image [{{:keys [file index]} :replay :as ctx}]
  (let [image (util/zip-read-file file (str index ".jpg"))]
    (Highgui/imdecode (MatOfByte. image) Highgui/IMREAD_UNCHANGED)))

(defn load-snap [{{:keys [file index]} :replay :as ctx}]
  (let [raw (load-current-image ctx)
        pimg (util/mat-to-pimage raw)]
    (update ctx :camera assoc :raw raw :pimg pimg)))

(defn config-step [{{:keys [file index] :as replay} :replay :as ctx}]
  (try
    (let [config (edn/read-string (slurp (util/zip-read-file file (str (or index 0) ".config.edn"))))]
      (->
        config
        (assoc :replay replay)
        update-homography
        load-snap
        load-reference))
    (catch FsEntryNotFoundException e ctx)))

(defn expected-board [{{:keys [file index]} :replay :as ctx}]
  (last (edn/read-string (slurp (util/zip-read-file file (str (or index 0) ".edn"))))))

(defn load-capture [filename]
  (config-step {:replay {:file filename :index 0}}))

(defn step-index [ctx]
  (->
    ctx
    (update-in [:replay :index] (fnil inc 0))
    load-snap))

(deftest t [])