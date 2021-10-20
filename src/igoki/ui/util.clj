(ns igoki.ui.util
  (:require
    [clojure.java.io :as io])
  (:import
    (javax.swing SwingUtilities JFrame JFileChooser)
    (javax.swing.filechooser FileNameExtensionFilter)))


(defn save-dialog [current-file success-fn]
  (SwingUtilities/invokeLater
    #(let [frame (JFrame. "Save")
           chooser (JFileChooser.)]
       (try
         (.setAlwaysOnTop frame true)

         (doto chooser
           (.setSelectedFile (or current-file (io/file "game.sgf")))
           (.setFileFilter (FileNameExtensionFilter. "SGF Files" (into-array ["sgf"]))))

         (when
           (= JFileChooser/APPROVE_OPTION (.showSaveDialog chooser frame))
           (success-fn (.getSelectedFile chooser)))
         (finally (.dispose frame))))))

(defn load-dialog [success-fn & [start-dir]]
  (SwingUtilities/invokeLater
    #(let [frame (JFrame. "Load")
           chooser (if start-dir (JFileChooser. ^String start-dir) (JFileChooser.))]
       (try
         (.setAlwaysOnTop frame true)
         (doto chooser
           (.setFileFilter (FileNameExtensionFilter. "SGF Files" (into-array ["sgf"]))))

         (when
           (= JFileChooser/APPROVE_OPTION (.showOpenDialog chooser frame))
           (success-fn (.getSelectedFile chooser)))
         (finally (.dispose frame))))))
