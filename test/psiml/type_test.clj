(ns psiml.type-test
  (:require [clojure.test :refer :all]
            [psiml.type :refer :all]))

(def t-int [:t :int])
(def t-bool [:t :bool])

(deftest unify-basetypes
  (is (= (unify t-int t-bool {}) [nil {}]))
  (is (= (unify t-int t-int {}) [t-int {}])))

(deftest unify-struct
  (is (= (unify [:struct {:i t-int :b t-bool}]
                [:struct {:b t-bool :i t-int}] {})
         [[:struct {:i t-int :b t-bool}] {}]))
  (is (= (unify [:struct {:i t-int :b t-bool}]
                [:struct {:b t-int :i t-bool}] {})
         [nil {}]))
  (is (= (unify [:struct {:i t-int :b t-bool}]
                [:struct {:a t-bool :b t-bool :i t-int}] {})
         [nil {}])))

(deftest expr-basetypes
  (is (= (expr [:cst 1]) [t-int {}]))
  (is (= (expr [:cst true]) [t-bool {}])))

(def id [:abs :x [:var :x]])

(deftest expr-id
  (is (= (expr id) [[:abs [:t-var] [:t-var]] {}])))

;; (deftest expr-id-app
;;   (is (= (expr [:app id [:var :y]] {:y [:t-var]})
;;          [[:t-var] {:y [:t-var]}])))
