(ns psiml.core-test
  (:require [clojure.test :refer :all]
            [psiml.core :refer :all]))

(deftest eval-lambda-simple
  (testing "Evaluate a simple expression"
    (let [e [:app [:abs "x" [:get "a" [:var "x"]]] [:rec {"a" [:var "y"]}]]
          r [:var "y"]]
      (is (= (eval-lambda e) r)))))
