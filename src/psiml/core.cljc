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
     [:rec n e] [:rec n (eval-lambda e (conj env [n e]))]
     [:cst c] [:cst c]
     [:var n] (or (env n) [:var n])
     [:app e1 e2] (let [e2' (eval-lambda e2 env)]
                    (or ((match-first [:abs n e1'] (eval-lambda e1' (conj env [n e2']))) e1)
                        [:app (eval-lambda e1 env) e2']))
     [:struct m] [:struct (reduce (fn [m' [l e]] (assoc m' l (eval-lambda e env))) {} m)]
     [:get l e] (let [e' (eval-lambda e env)]
                  (or ((match-first [:struct m] (m l)) e')
                      [:get l e'])))
    expr)))

;; [:t :int]
;; [:t :bool]
;; [:t-abs {ty -> ty}]
;; [:t-rec]
;; [:t-struct {field -> ty}]
;; [:t-var n]
;; [:t-top]
;; [:t-bot]
;; [:t-meet t t]
;; [:t-join t t]

;; 1 --> [:t :int]
;; true --> [:t :bool]
;; (fn [n1 n2] e) --> [:abs n1 [:abs n2 r(e)]]
;; {l1 e1 l2 e2} --> [:struct {l1 r(e1) l2 r(e2)}]
;; (:l e) --> [:get l r(e)]
;; (let [n e1] e2) --> [:app [:abs n r(e2)] [:rec n r(e1)]]
;; (e1 e2) --> [:app r(e1) r(e2)]

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello!"))

#?(:cljs (-main))
