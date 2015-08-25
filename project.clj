(defproject igoki "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies
  [[org.clojure/clojure "1.7.0"]
   [nu.pattern/opencv "2.4.9-7"]
   [clj-kdtree "1.2.0" :exclusions [org.clojure/clojure]]]
  :main badukpro.core
  )
