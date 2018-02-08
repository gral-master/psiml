(ns psiml.parse-test
  (:require [clojure.test :refer :all]
            [psiml.core :refer :all]
            [psiml.parse :refer :all]))

(deftest parse-expr-simple
  (testing "Parse an int"
    (is (= (string "7") [:lit 7])))
  (testing "Parse a function with a struct"
    (let [e "(fn [label] {:label true})"
          r [:abs :label [:struct {:label [:lit true]}]]]
      (is (= (string e) r))))
  (testing "Parse a function with a nested struct"
    (let [e "(fn [label] {:label {:hello false}})"
          r [:abs :label [:struct {:label [:struct {:hello [:lit false]}]}]]]
      (is (= (string e) r))))
  (testing "Function with multiple parameters"
    (let [e "(fn [a b c] a)"
          r [:abs :a [:abs :b [:abs :c [:var :a]]]]]
      (is (= (string e) r))))
  (testing "Application with multiple arguments"
    (let [e "(f a b 3)"
          r [:app [:app [:app [:var :f] [:var :a]]
                   [:var :b]]
             [:lit 3]]]
      (is (= (string e) r))))
  (testing "Get a label"
    (is (= (string "(:l s)") [:get :l [:var :s]])))
  (testing "Let some stuff"
    (let [e "(let [a 1 b (fn [x] x)] b)"
          r [:app [:abs :a
                   [:app [:abs :b [:var :b]]
                    [:abs :x [:var :x]]]]
             [:lit 1]]]
      (is (= (string e) r)))))
