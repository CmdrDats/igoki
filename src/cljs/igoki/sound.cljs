(ns igoki.sound)

(def sounds (atom {}))

(defn play [sound]
  (let [snd (get (swap! sounds update sound #(or % (js/Audio. (str "/sounds/" sound ".wav")))) sound)]
    (.play snd)))

#_(play "japanese/atari")

#_(let [back (js/Audio. "/sounds/back.wav")
      click (js/Audio. "/sounds/click.wav")]
  (set! (.-onended back) #(.play click))
  (.play back)
  )