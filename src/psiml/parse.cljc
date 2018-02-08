(ns psiml.parse
  "Parses concrete syntax, producing abstract syntax
  e := 1 --> [:lit 1]
    | true --> [:lit true]
    | (fn [a b] e) --> [:abs :a [:abs :b e']]
    | (e1 e2 e3) --> [:app [:app e1' e2'] e3']
    | {:l1 e1 :l2 e2} --> [:struct {:l1 e1' :l2 e2'}]
    | (:l s) --> [:get :l s']
    | (let [n e1] e2) --> [:app [:abs n e2'] e1']"
  (:refer-clojure :exclude [read-string])
  (:require [clojure.core.match :refer [match]]
            [#?(:clj clojure.edn
                :cljs cljs.reader) :refer [read-string]]))

(defn data
  "Parses concrete syntax from data"
  [d]
  (cond
    (seq? d)
    (match d
      (['fn [a] b] :seq)
      (if (symbol? a) [:abs (keyword a) (data b)])
      (['fn [a & args] b] :seq)
      (data (list 'fn [a] (list 'fn args b)))
      (['let [n e] b] :seq)
      (if (symbol? n) [:app [:abs (keyword n) (data b)] (data e)])
      (['let [n e & r] b] :seq)
      (data (list 'let [n e] (list 'let r b)))
      ([a b] :seq)
      (if (keyword? a) [:get a (data b)]
          [:app (data a) (data b)])
      ([a b & r] :seq)
      (data (cons (list a b) r)))
    (symbol? d)
    [:var (keyword d)]
    (or (integer? d) (true? d) (false? d))
    [:lit d]
    (map? d)
    [:struct (reduce (fn [m [l v]] (assoc m l (data v))) {} d)]))

(defn string
  "Parses concrete syntax from a string"
  [s]
  (data (read-string s)))
