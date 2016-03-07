(ns igoki.sound.announce
  (:require [clojure.string :as str]
            [igoki.sgf :as sgf]
            [clojure.java.io :as io]
            [igoki.sound.sound :as snd]))

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
  Ootakamoku, Gonogo, Tengen, Hoshishita, Hoshiwaki, NiNolchi,
  Hidariue, migiue, hidarishita, migishita, keima kakari,
  ikken takagakari, ogeima takagakari, niken takagakari"
  )

(defn announce [parts]
  (println "Announce: " parts)
  (doto (Thread.
          (fn []
            (doseq [p (remove #(or (nil? %) (str/blank? %)) parts)]
              (case p
                "," (Thread/sleep 250)
                "-" (Thread/sleep 500)
                (snd/sound (str "public/sounds/japanese/" p ".wav"))))))
    (.setDaemon true)
    (.start)))

(def soundmapping
  {1 "Ichi" 2 "Ni" 3 "San" 4 "Yon" 5 "Go" 6 "Roku" 7 "Nana" 8 "Hachi" 9 "Kyu" 10 "Ju"
   11 "Ju-ichi" 12 "Ju-ni" 13 "Ju-san" 14 "Ju-yon" 15 "Ju-go" 16 "Ju-roku"
   17 "Ju-nana" 18 "Ju-hachi" 19 "Ju-kyu"})

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

(defn comment-move [node board]
  (let [{:keys [black white]} node
        position (first (or black white))
        [x y :as p] (map inc (sgf/convert-sgf-coord position))
        named (named-points p)
        opening [(board-area p) (opening-point (normalize p))]]
    (announce (concat [(if black "Kuro" "Shiro") "," (soundmapping x) "no" (soundmapping y) ","] (or named opening)))))

