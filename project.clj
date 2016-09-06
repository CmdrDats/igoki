(defproject igoki "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies
  [[org.clojure/clojure "1.8.0"]

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
   [org.slf4j/slf4j-log4j12 "1.6.2"]

   [com.google.guava/guava "18.0"]
   [org.nd4j/nd4j "0.4-rc3.10" :extension "pom"]
   [org.nd4j/nd4j-native "0.4-rc3.10"]
   [org.deeplearning4j/deeplearning4j-core "0.4-rc3.10"]
   [org.nd4j/canova-api "0.0.0.16"]



   [re-frame "0.7.0-alpha-3"]
   [re-com "0.8.0"]
   [prismatic/schema "1.0.5"]
   [com.taoensso/sente "1.8.0"]
   [http-kit "2.1.18"]
   [reagent "0.6.0-alpha"
    :exclusions [org.clojure/tools.reader]]
   [reagent-forms "0.5.20"]
   [reagent-utils "0.1.7"]
   [ring "1.4.0"]
   [ring/ring-defaults "0.1.5"]
   [compojure "1.4.0"]
   [hiccup "1.0.5"]
   [environ "1.0.2"]
   [org.clojure/clojurescript "1.7.228"
    :scope "provided"]
   [com.andrewmcveigh/cljs-time "0.3.14"]
   [secretary "1.2.3"]
   [venantius/accountant "0.1.6"
    :exclusions [org.clojure/tools.reader]]]
  :npm
  {:root :target-path
   :dependencies
   [[magnifier "0.3.0"]]}
  :repositories {"yadarts-maven" "https://raw.github.com/yadarts/maven/master"}
  :jvm-opts ["-Djavax.net.ssl.trustStore=resources/ogs.truststore"
             "-Xms1024M" "-Xmx2048M" "-XX:NewSize=528M" "-XX:+UseParNewGC" "-XX:+UseConcMarkSweepGC"
             "-XX:+CMSParallelRemarkEnabled" "-server" "-XX:-OmitStackTraceInFastThrow"]
  :main igoki.core

  :plugins [[lein-environ "1.0.2"]
            [lein-cljsbuild "1.1.1"]
            [lein-npm "0.6.2"]
            [lein-asset-minifier "0.2.7"
             :exclusions [org.clojure/clojure]]]

  :min-lein-version "2.5.0"

  :uberjar-name "igoki.jar"

  :clean-targets ^{:protect false} [:target-path
                                    [:cljsbuild :builds :app :compiler :output-dir]
                                    [:cljsbuild :builds :app :compiler :output-to]]

  :source-paths ["src/clj" "src/cljc"]
  :resource-paths ["resources" "target/cljsbuild"]

  :minify-assets
  {:assets
   {"resources/public/css/site.min.css" "resources/public/css/site.css"}}

  :cljsbuild {:builds {:app {:source-paths ["src/cljs" "src/cljc"]
                             :compiler {:output-to "target/cljsbuild/public/js/app.js"
                                        :output-dir "target/cljsbuild/public/js/out"
                                        :asset-path   "js/out"
                                        :optimizations :none
                                        :pretty-print  true}}}}


  :profiles {:dev {:repl-options {:init-ns igoki.repl}

                   :dependencies [[ring/ring-mock "0.3.0"]
                                  [ring/ring-devel "1.4.0"]
                                  [prone "1.0.2"]
                                  [lein-figwheel "0.5.0-6"
                                   :exclusions [org.clojure/core.memoize
                                                ring/ring-core
                                                org.clojure/clojure
                                                org.ow2.asm/asm-all
                                                org.clojure/data.priority-map
                                                org.clojure/tools.reader
                                                org.clojure/clojurescript
                                                org.clojure/core.async
                                                org.clojure/tools.analyzer.jvm]]
                                  [org.clojure/tools.nrepl "0.2.12"]
                                  [com.cemerick/piggieback "0.2.1"]
                                  [pjstadig/humane-test-output "0.7.1"]
                                  ]

                   :source-paths ["env/dev/clj"]
                   :plugins [[lein-figwheel "0.5.0-6"
                              :exclusions [org.clojure/core.memoize
                                           ring/ring-core
                                           org.clojure/clojure
                                           org.ow2.asm/asm-all
                                           org.clojure/data.priority-map
                                           org.clojure/tools.reader
                                           org.clojure/clojurescript
                                           org.clojure/core.async
                                           org.clojure/tools.analyzer.jvm]]
                             ]

                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]

                   :figwheel {:http-server-root "public"
                              :server-port 3449
                              :nrepl-port 7002
                              :nrepl-middleware ["cemerick.piggieback/wrap-cljs-repl"
                                                 ]
                              :css-dirs ["resources/public/css"]
                              :ring-handler igoki.web.handler/app}

                   :env {:dev true}

                   :cljsbuild {:builds {:app {:source-paths ["env/dev/cljs"]
                                              :compiler {:main "igoki.dev"
                                                         :source-map true}}



                                        }
                               }}

             :uberjar {:hooks [minify-assets.plugin/hooks]
                       :source-paths ["env/prod/clj"]
                       :prep-tasks ["compile" ["cljsbuild" "once"]]
                       :env {:production true}
                       :aot :all
                       :omit-source true
                       :cljsbuild {:jar true
                                   :builds {:app
                                            {:source-paths ["env/prod/cljs"]
                                             :compiler
                                                           {:optimizations :advanced
                                                            :pretty-print false}}}}}}
  )
