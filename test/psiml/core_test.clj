(ns psiml.core-test
  (:require [clojure.test :refer :all]
            [psiml.core :refer :all]))

(deftest eval-expr-simple
  (testing "Evaluate a simple expression"
    (let [e [:app [:abs :x [:get :a [:var :x]]] [:struct {:a [:var :y]}]]
          r [:var :y]]
      (is (= (eval-expr e) r)))))
