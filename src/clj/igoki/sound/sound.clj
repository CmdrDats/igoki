(ns igoki.sound.sound
  (:import (javax.sound.sampled AudioSystem LineListener LineEvent LineEvent$Type)
           (java.util.concurrent CountDownLatch)))

(def get-clip
  (memoize
    (fn [file]
      (println "Loading:" file)
      (let [ais  (AudioSystem/getAudioInputStream (ClassLoader/getSystemResource file))
            clip (AudioSystem/getClip)]
        (.open clip ais)
        clip))))

(defn sound [file]
  (let [clip (get-clip file)
        latch (CountDownLatch. 1)
        listener (proxy [LineListener] []
                   (update [^LineEvent e]
                     (when (= (.getType e) LineEvent$Type/STOP)
                       (.countDown latch))))]
    (.addLineListener clip listener)
    (.setFramePosition clip 0)
    (.start clip)
    (.await latch)
    (.removeLineListener clip listener)))

(def sounds
  {:click  "sounds/click.wav"
   :undo   "sounds/back.wav"
   :submit "sounds/submit.wav"})

(defn play-sound [soundkey]
  (if-let [s (get sounds soundkey)]
    (doto (Thread. #(sound s))
      (.setDaemon true)
      (.start))))
