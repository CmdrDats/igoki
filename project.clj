(defproject igoki "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies
  [[org.clojure/clojure "1.7.0"]
   [org.openpnp/opencv "2.4.11-1"]
   [quil "2.2.6"]
   [clj-http "2.0.0"]
   [cheshire "5.5.0"]
   [de.schlichtherle.truezip/truezip-file "7.7.9"]
   [de.schlichtherle.truezip/truezip-driver-zip "7.7.9"]
   [kuusisto/tinysound "1.1.1"]
   [io.socket/socket.io-client "0.6.2"]
   [org.clojure/tools.logging "0.3.1"]
   [log4j "1.2.16"]
   [org.slf4j/slf4j-api "1.6.2"]
   [org.slf4j/jul-to-slf4j "1.6.2"]
   [org.slf4j/slf4j-log4j12 "1.6.2"]]
  :repositories {"yadarts-maven" "https://raw.github.com/yadarts/maven/master"}
  :jvm-opts ["-Djavax.net.ssl.trustStore=resources/ogs.truststore"]
  :main igoki.core)
