(ns psiml.core
  (:require [psiml.type :as t]
            [psiml.parse :as p]
            [clojure.core.match :refer [match]]))

#?(:cljs (enable-console-print!))

(defn eval-expr
  "Evaluates an expression"
  ([expr] (eval-expr expr {}))
  ([expr env]
   (match expr
     [:abs n e] [:abs n (eval-expr e env)]
     [:rec n e] [:rec n (eval-expr e (conj env [n e]))]
     [:lit c] [:cst c]
     [:var n] (or (env n) [:var n])
     [:app e1 e2] (let [e2' (eval-expr e2 env)]
                    (or (match e1 [:abs n e1'] (eval-expr e1' (conj env [n e2'])))
                        [:app (eval-expr e1 env) e2']))
     [:struct m] [:struct (reduce (fn [m' [l e]] (assoc m' l (eval-expr e env))) {} m)]
     [:get l e] (let [e' (eval-expr e env)]
                  (or (match e' [:struct m] (m l))
                      [:get l e']))
     expr)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [e (p/string "(fn [n1 n2] true)")]
    (println e)))

#?(:cljs (-main))
