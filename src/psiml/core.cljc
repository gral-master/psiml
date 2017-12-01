(ns psiml.core)

#?(:cljs (enable-console-print!))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [e :expr
        abs [:abs "name" e]
        var [:var "name"]
        app [:app e e]
        rec [:rec {"label" e}]
        get [:get "label" e]
        let [:let "name" e]]
        ;; let-var ? can be inferred
        ;; let-rec ? can be inferred
        ;; then types with type variables and recursive types
    (do (println "Hello, expressions!")
        (println abs)
        (println var)
        (println app)
        (println rec)
        (println get)
        (println let))))

#?(:cljs (-main))
