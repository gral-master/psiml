(ns psiml.parse
  "Parses concrete syntax, producing abstract syntax"
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
      ;; abs
      (['fn [a] b] :seq) 
      (if (symbol? a) [:abs (keyword a) (data b)])
      ;; get
      ([a b] :seq)
      (if (keyword? a) [:get (name a) (data b)]))
    (or (integer? d) (true? d) (false? d)) 
      [:cst d]
    ;; struct
    (map? d)
      [:struct (reduce #(conj %1 {(first %2) (data(second %2))}) {} d)]))

(defn string
  "Parses concrete syntax from a string"
  [s]
  (data (read-string s)))

(conj {} {(first [:a 1]) (second [:a 1])})