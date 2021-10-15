(defproject igoki "0.1.1"
  :description "Igoki, physical Go board/OGS interface"
  :url "http://github.com/CmdrDats/igoki"
  :license
  {:name "Eclipse Public License"
   :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies
  [[org.clojure/clojure "1.8.0"]
   [com.google.guava/guava "20.0"]

   [org.openpnp/opencv "2.4.11-1"]
   [cheshire "5.10.1"]
   [de.schlichtherle.truezip/truezip-file "7.7.10"]
   [de.schlichtherle.truezip/truezip-driver-zip "7.7.10"]

   [io.socket/socket.io-client "0.9.0"]

   [org.clojure/tools.logging "1.1.0"]

   [log4j "1.2.17"]
   [org.slf4j/slf4j-api "1.7.32"]
   [org.slf4j/jul-to-slf4j "1.7.32"]
   [org.slf4j/slf4j-log4j12 "1.7.32"]

   [org.nd4j/nd4j "0.4-rc3.10" :extension "pom"]
   [org.nd4j/nd4j-native "0.4-rc3.10"]
   [org.deeplearning4j/deeplearning4j-core "0.4-rc3.10"]
   [org.nd4j/canova-api "0.0.0.16"]]

  :main igoki.core

  :repl-options
  {:welcome "Welcome to igoki"
   :init-ns igoki.core
   :init (-main)}

  #_#_:min-lein-version "2.5.0"

  :uberjar-name "igoki.jar")
