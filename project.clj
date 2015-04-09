(defproject ttt "0.1.0-TIC-TAC-TOE"
  :description "Have fun!"
  :url "http://fellipebrito.github.io/ttt"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2850"]
                 [figwheel "0.2.5-SNAPSHOT"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [jayq "2.5.4"]
                 [sablono "0.3.4"]
                 [org.omcljs/om "0.8.8"]]

  :plugins [[lein-cljsbuild "1.0.4"]
            [lein-figwheel "0.2.5-SNAPSHOT"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src" "dev_src"]
              :compiler {:output-to "resources/public/js/compiled/ttt.js"
                         :output-dir "resources/public/js/compiled/out"
                         :optimizations :none
                         :main ttt.dev
                         :asset-path "js/compiled/out"
                         :source-map true
                         :source-map-timestamp true
                         :cache-analysis true }}
             {:id "min"
              :source-paths ["src"]
              :compiler {:output-to "resources/public/js/compiled/ttt.js"
                         :main ttt.core
                         :optimizations :advanced
                         :pretty-print false}}]}

  :figwheel {
             :http-server-root "public" ;; default and assumes "resources"
             :server-port 3449 ;; default
             :css-dirs ["resources/public/css"] }) ;; watch and update CSS
