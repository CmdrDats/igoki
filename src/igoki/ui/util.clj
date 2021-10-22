(ns igoki.ui.util
  (:require
    [clojure.java.io :as io]
    [seesaw.core :as s])
  (:import
    (javax.swing SwingUtilities JFrame JFileChooser)
    (javax.swing.filechooser FileNameExtensionFilter)
    (java.awt Desktop Desktop$Action)
    (java.net URI)))


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


(defn open [^String url]
  (cond
    ;; Linux support
    (not= -1
      (.read
        (.getInputStream
          (.exec (Runtime/getRuntime)
            (into-array ["which" "xdg-open"])))))
    (.exec (Runtime/getRuntime) (into-array ["xdg-open" url]))

    ;; If can browse
    (and (Desktop/isDesktopSupported)
      (.isSupported (Desktop/getDesktop) Desktop$Action/BROWSE))
    (.browse (Desktop/getDesktop) (URI. url))

    :else
    (s/alert (str "Cannot browse directly, open your browser to the URL: " url)
      :type :warning)))