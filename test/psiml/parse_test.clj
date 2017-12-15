(ns psiml.parse-test
  (:require [clojure.test :refer :all]
            [psiml.core :refer :all]
            [psiml.parse :refer :all]))

(deftest parse-expr-simple
	(testing "Parse an int"
		(let [e "7"
			  r [:lit 7]]
			  (is (= (string e) r))))
	(testing "Parse a function with a struct"
		(let [e "(fn [label] {:label true})"
			  r [:abs :label [:struct {:label [:lit true]}]]]
			(is (= (string e) r))))
	(testing "Parse a function with a nested struct"
		(let [e "(fn [label] {:label {:hello false}})"
			  r [:abs :label [:struct {:label [:struct {:hello [:lit false]}]}]]]
			(is (= (string e) r)))))



