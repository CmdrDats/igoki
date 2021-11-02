(ns igoki.sound.announce
  (:require
    [clojure.string :as str]
    [igoki.sgf :as sgf]
    [clojure.java.io :as io]
    [igoki.sound.sound :as snd])
  (:import (java.util.concurrent ThreadPoolExecutor TimeUnit LinkedBlockingQueue)))

;; Japanese words generated via google translate
;; Enlglish words generated via https://www.naturalreaders.com/online/

(comment
  ;; Words..
  "
  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
  Ichi, Ni, San, Yon, Go, Roku, Nana, Hachi, Kyu, Ju,
  Ju-ichi, Ju-ni, Ju-san, Ju-yon, Ju-go, Ju-roku, Ju-nana, Ju-hachi, Ju-kyu
  Agohimo, Akisankaku, Atari, Atekomi, Boshi, Botsugi,
  Daidaigeima, Dan, Dango, Degiri, Fukure, Geta, Goken-biraki,
  Gote, Gote no sente, Gyaku komi, Hamete, Hana zuke, Hane,
  Hane-dashi, Hane-komi, Hara-zuke, Hasami, Hasami tsuke,
  Hazama tobi, Hekomi, Hiraki, Horikomi, Ikken tobi,
  Joseki, Kakari, Kaketsugi, Kannon-biraki, Kuruma no ato-oshi,
  Katatsuki, Kiri, Komi, Kosumi, Kosumi-dashi, Kosumi-tsuke,
  Kyu, Moku, Mokai komoku, Kenka Komoku, Nageru, Hanekaeshi,
  Nidan-bane, Niken biraki, Niken tobi, Nigiri, Nirensei,
  Nobi, Ogeima, Onadare, Owari, Ponnuki, Ryojimari, Ryoatari,
  Sagari, Sangen biraki, Sanrensei, Sente, Sente no gote,
  Shico, Shico-atari, Shimari, Kuro, Shiro, Shodan, Suberi,
  Susaoki, Susogakari, Tagai sen, Taisha, Takefu, Te, Teai, Tenuki, Tesuji, Tobi,
  Tetchu, Tsuke, Tsuke-koshi, Tsuki, Tsume-biraki, Warikomi,
  Wariuchi, Komoku, Hoshi, Sansan, Mokuhazushi, Takamoku, Oomokuhazushi,
  Ootakamoku, Gonogo, Tengen, Hoshishita, Hoshiwaki, NiNoIchi,
  Hidariue, migiue, hidarishita, migishita, keima kakari,
  ikken takagakari, ogeima takagakari, niken takagakari"
  )

(def sound-executor
  (ThreadPoolExecutor. 1 1 60 TimeUnit/MINUTES (LinkedBlockingQueue. 24)))

(defn announce [lang parts]
  (println "Announce: " parts)
  (.submit sound-executor
    (fn []
      (doseq [p (remove #(or (nil? %) (str/blank? %)) parts)]
        (case p
          "," (Thread/sleep 250)
          "-" (Thread/sleep 500)
          (snd/sound (str "public/sounds/" (name (or lang :en)) "/" p ".wav")))))))

(def soundmapping
  {:en
   {:players {:white "white" :black "black"}
    :coords
    {:join nil
     :x
     (->>
       (range 1 20)
       (map
         (fn [i]
           (let [c (char (+ 64 i (if (> i 8) 1 0)))]
             [i (str c)])))
       (into {}))

     :y
     (->>
       (range 1 20)
       (map
         (fn [i] [i (str "post" i)]))
       (into {}))}}
   :jp
   {:players {:white "Shiro" :black "Kuro"}
    :coords
    {:join "no"
     :x
     {1 "Ichi" 2 "Ni" 3 "San" 4 "Yon" 5 "Go" 6 "Roku" 7 "Nana" 8 "Hachi" 9 "Kyu" 10 "Ju"
      11 "Ju-ichi" 12 "Ju-ni" 13 "Ju-san" 14 "Ju-yon" 15 "Ju-go" 16 "Ju-roku"
      17 "Ju-nana" 18 "Ju-hachi" 19 "Ju-kyu"}

     :y
     {1 "Ichi" 2 "Ni" 3 "San" 4 "Yon" 5 "Go" 6 "Roku" 7 "Nana" 8 "Hachi" 9 "Kyu" 10 "Ju"
      11 "Ju-ichi" 12 "Ju-ni" 13 "Ju-san" 14 "Ju-yon" 15 "Ju-go" 16 "Ju-roku"
      17 "Ju-nana" 18 "Ju-hachi" 19 "Ju-kyu"}}}})

(def named-points
  {[3 3]   ["Hidariue" "Sansan"]
   [3 16]  ["Hidarishita" "Sansan"]
   [16 3]  ["Migiue" "Sansan"]
   [16 16] ["Migishita" "Sansan"]
   [10 10] ["Tengen"]})

(def opening-point
  {[3 3] "Sansan"
   [4 4] "Hoshi"
   [3 4] "Komoku"
   [3 5] "Mokuhazushi"
   [4 5] "Takamoku"
   [3 6] "Oomokuhazushi"
   [4 6] "Ootakamoku"
   [5 5] "Gonogo"
   [3 9] "Hoshishita"
   [3 10] "Hoshiwaki"
   [1 2] "NiNoIchi"})

(defn normalize [[x y]]
  (sort
    [(- 10 (Math/abs (int (- x 10))))
     (- 10 (Math/abs (int (- y 10))))]))

(defn board-area [[x y]]
  (if (or (= x 10) (= y 10))
    nil
    (if (< x 10)
      (if (< y 10)
        "Hidariue"
        "Hidarishita")
      (if (< y 10)
        "Migiue"
        "Migishita"))))

(defn lookup-sound [language path]
  (get-in soundmapping (concat [language] path)))

(defn comment-move [ctx node board]
  (let [{:keys [player language] :or {language :en}} (:announce @ctx)
        {:keys [black white]} node
        moves (or black white)
        position (first moves)
        [x y :as p] (map inc (sgf/convert-sgf-coord position))
        #_#_named (named-points p)
        #_#_opening [(board-area p) (opening-point (normalize p))]]

    (when
      (and
        ;; There shouldn't be black _and_ white moves to announce, else we'll just bombard
        (not (and black white))

        ;; There should also only be one move to announce, else, again, we'll bombard.
        (= 1 (count moves))

        ;; And the user should have requested which players to announce, specifically.
        (or
          (and white (:white player))
          (and black (:black player))))

      (announce
        language
        (concat
          (when (> (count player) 1)
            [(lookup-sound language [:players (if black :black :white)])
             ","])
          [(lookup-sound language [:coords :x x])
           (lookup-sound language [:coords :join])
           (lookup-sound language [:coords :y y])] #_(or named opening))))))

(defn set-announce-player [ctx player]
  (swap! ctx assoc-in [:announce :player]
    (case player
      :black #{:black}
      :white #{:white}
      :both #{:black :white}
      #{})))

(defn set-announce-language [ctx langkey]
  (swap! ctx assoc-in [:announce :language] langkey))

