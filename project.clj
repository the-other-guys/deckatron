(defproject deckatron "0.1.0-SNAPSHOT"
  :dependencies [
    [org.clojure/clojure        "1.7.0"]
    [org.clojure/clojurescript  "1.7.189"]
    [org.clojure/core.async     "0.2.374"]
    [rum                        "0.6.0"]
    [http-kit                   "2.1.19"]
    [compojure                  "1.4.0" :exclusions [commos-codec]]
    [com.cognitect/transit-clj  "0.8.285"]
    [com.cognitect/transit-cljs "0.8.232"]
    [com.lucasbradstreet/instaparse-cljs "1.4.1.0"]
  ]

  :plugins [
    [lein-cljsbuild       "1.1.1"]
    [tonsky/lein-figwheel "0.5.0-4"]
  ]

  :aliases      { "package"   ["do"
                               "cljsbuild" "once" "advanced,"
                               "uberjar"] }
  :aot          [ deckatron.server ]
  :uberjar-name "deckatron.jar"
  :uberjar-exclusions [#"public/js/out"]
  :auto-clean   false

  :clean-targets ^{:protect false} ["resources/public/js" "target"]

  :main         deckatron.server
  :figwheel     { :ring-handler  "deckatron.server/app"
                  :css-dirs     ["resources/public"]
                  :server-port   8080
                  :repl          false }

  :cljsbuild {
    :builds [
      { :id           "none"
        :source-paths ["src"]
        :figwheel     { :on-jsload      "deckatron.app/refresh" }
        :compiler     { :optimizations  :none
                        :main           deckatron.app
                        :asset-path     "/js/out"
                        :output-to      "resources/public/js/main.js"
                        :output-dir     "resources/public/js/out"
                        :source-map     true
                        :compiler-stats true } }

      { :id           "advanced"
        :source-paths ["src"]
        :compiler     { :optimizations  :advanced
                        :main           deckatron.app
                        :output-to      "resources/public/js/main.js"
                        :compiler-stats true
                        :pretty-print   false
                        :pseudo-names   false } }
  ]}
)
