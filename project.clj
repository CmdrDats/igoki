(defproject igoki "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies
  [[org.clojure/clojure "1.8.0"]
   [org.clojure/clojurescript "1.9.671"]
   [com.google.guava/guava "20.0"]

   [org.openpnp/opencv "2.4.11-1"]
   [quil "2.2.6"]
   [clj-http "2.0.0"]
   [cheshire "5.5.0"]
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
   [org.nd4j/canova-api "0.0.0.16"]

   [cljs-react-material-ui "0.2.47"]

   [rum "0.10.8" :exclusions [cljsjs/react cljsjs/react-dom]]
   [com.taoensso/sente "1.8.0"]

   [http-kit "2.1.18"]
   [ring "1.4.0"]
   [ring/ring-defaults "0.1.5"]
   [compojure "1.4.0"]
   [hiccup "1.0.5"]
   [environ "1.0.2"]
   [com.cemerick/piggieback "0.2.1"]]

  :repositories {"yadarts-maven" "https://raw.github.com/yadarts/maven/master"}
  :jvm-opts ["-Djavax.net.ssl.trustStore=resources/ogs.truststore"
             "-Xms1024M" "-Xmx2048M" "-XX:NewSize=528M" "-XX:+UseParNewGC" "-XX:+UseConcMarkSweepGC"
             "-XX:+CMSParallelRemarkEnabled" "-server" "-XX:-OmitStackTraceInFastThrow"]
  :main igoki.core

  :repl-options
  {:welcome "Welcome to igoki"
   :init-ns igoki.core
   :init (-main)}

  :plugins [[lein-environ "1.0.2"]
            [lein-cljsbuild "1.1.6" :exclusions [org.clojure/clojure]]
            [lein-figwheel "0.5.11" :exclusions [org.clojure/clojure]]]

  :min-lein-version "2.5.0"

  :uberjar-name "igoki.jar"

  :clean-targets
  ^{:protect false}
  [:target-path
   [:cljsbuild :builds :app :compiler :output-dir]
   [:cljsbuild :builds :app :compiler :output-to]]
  :figwheel
  {:css-dirs ["resources/public/css"]}
  :source-paths ["src/clj" "src/cljc"]
  :resource-paths ["resources" "target/cljsbuild"]
  :cljsbuild
  {:builds
   {:app
    {:source-paths ["src/cljs" "src/cljc"]
     :figwheel {:on-jsload "igoki.core/mount-root"}
     :compiler
     {:output-to "resources/public/js/app.js"
      :output-dir "resources/public/js/out"
      :asset-path   "js/out"
      :optimizations :none
      :pretty-print  true
      :main "igoki.core"}}}})
