(defproject igoki "0.1.0-SNAPSHOT"
  :description "Igoki, physical Go board/OGS interface"
  :url "http://github.com/CmdrDats/igoki"
  :license
  {:name "Eclipse Public License"
   :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies
  [[org.clojure/clojure "1.10.3"]
   [com.google.guava/guava "20.0"]

   [org.openpnp/opencv "2.4.11-1"]
   [quil "3.1.0"]
   [cheshire "5.10.1"]
   [de.schlichtherle.truezip/truezip-file "7.7.9"]
   [de.schlichtherle.truezip/truezip-driver-zip "7.7.9"]

   [kuusisto/tinysound "1.1.1"]
   [io.socket/socket.io-client "0.9.0"]

   [org.clojure/tools.logging "0.3.1"]

   [log4j "1.2.16"]
   [org.slf4j/slf4j-api "1.6.2"]
   [org.slf4j/jul-to-slf4j "1.6.2"]
   [org.slf4j/slf4j-log4j12 "1.6.2"]

   [org.nd4j/nd4j "0.4-rc3.10" :extension "pom"]
   [org.nd4j/nd4j-native "0.4-rc3.10"]
   [org.deeplearning4j/deeplearning4j-core "0.4-rc3.10"]
   [org.nd4j/canova-api "0.0.0.16"]]

  ;:repositories {"yadarts-maven" "https://raw.github.com/yadarts/maven/master"}
  :jvm-opts
  ["-Djavax.net.ssl.trustStore=resources/ogs.truststore"
   "-Xms1024M" "-Xmx2048M" "-XX:NewSize=528M" "-XX:+UseParNewGC" "-XX:+UseConcMarkSweepGC"
   "-XX:+CMSParallelRemarkEnabled" "-server" "-XX:-OmitStackTraceInFastThrow"]
  :main igoki.core

  :repl-options
  {:welcome "Welcome to igoki"
   :init-ns igoki.core
   :init (-main)}

  :min-lein-version "2.5.0"

  :uberjar-name "igoki.jar"

  :resource-paths ["resources"])
