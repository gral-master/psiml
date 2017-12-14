(defproject psiml "0.1.0-SNAPSHOT"
  :description "Polymorphic, Subtyped and type Infered ML"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/clojurescript "1.9.946"
                  :exclusions [org.apache.ant/ant]]]
  :plugins [[lein-cljsbuild "1.1.7"
             :exclusions [[org.clojure/clojure]]]]
  :cljsbuild {:builds [{:source-paths ["src"]
                        :compiler {:main psiml.core
                                   :output-to "web/main.js"
                                   :output-dir "web/"
                                   :source-map "web/main.js.map"
                                   :optimizations :whitespace
                                   :pretty-print true}}]}
  :main ^:skip-aot psiml.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
