(ns psiml.core
  (:require [psiml.util
             #?(:clj :refer
                :cljs :refer-macros) [match-first]]))

#?(:cljs (enable-console-print!))

(defn eval-lambda
  "Evaluates an extended lambda calculi expression"
  ([expr] (eval-lambda expr {}))
  ([expr env]
   ((match-first
     [:abs n e] [:abs n (eval-lambda e env)]
     [:var n] (or (env n) [:var n])
     [:app e1 e2] (let [e2' (eval-lambda e2 env)]
                    (or ((match-first [:abs n e1'] (eval-lambda e1' (conj env [n e2']))) e1)
                        [:app (eval-lambda e1 env) e2']))
     [:rec m] [:rec (reduce (fn [m' [l e]] (assoc m' l (eval-lambda e env))) {} m)]
     [:get l e] (let [e' (eval-lambda e env)]
                  (or ((match-first [:rec m] (m l)) e')
                      [:get l e'])))
    expr)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello!"))

#?(:cljs (-main))
