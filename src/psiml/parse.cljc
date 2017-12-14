(ns psiml.parse
  "Parses concrete syntax, producing abstract syntax"
  (:refer-clojure :exclude [read-string])
  (:require [clojure.core.match :refer [match]]
            [#?(:clj clojure.edn
                :cljs cljs.reader) :refer [read-string]]))

(defn data
  "Parses concrete syntax from data"
  [d]
  (if (seq? d)
    (match d
      (['fn [a] b] :seq)
      (if (symbol? a) [:abs (keyword a) (data b)]))
    (if (or (integer? d) (true? d) (false? d)) [:cst d])))

(defn string
  "Parses concrete syntax from a string"
  [s]
  (data (read-string s)))
