(defproject psiml "0.1.0-SNAPSHOT"
  :description "Polymorphic, Subtyped and type Infered ML"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.521"
                  :exclusions [org.apache.ant/ant]]]
  :plugins [[lein-cljsbuild "1.1.7"]]
  :cljsbuild { :builds [{:source-paths ["src"]
                         :compiler {:main psiml.core
                                    :output-to "web/main.js"
                                    :output-dir "web/"
                                    :optimizations :whitespace
                                    :pretty-print true}}]}
  :main ^:skip-aot psiml.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
