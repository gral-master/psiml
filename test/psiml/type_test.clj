(ns psiml.type-test
  (:require [clojure.test :refer :all]
            [psiml.type :refer :all]))

(def t-int [:t :int])
(def t-bool [:t :bool])

(defn unifications?
  [t1 t2 t-input t-output t-bi]
  (or (= (unify-input t1 t2 {}) [t-input {}])
      (= (unify-output t1 t2 {}) [t-output {}])
      (= (bi-unify t1 t2 {}) [t-bi {}])))

(deftest bi-unify-basetypes
  (is (unifications? t-int t-bool
                     [:meet t-int t-bool]
                     [:join t-int t-bool]
                     nil))
  (is (apply unifications? (repeat 5 t-int))))

(deftest bi-unify-struct
  (is (apply unifications?
             (repeat 5 [:struct {:i t-int :b t-bool}])))
  (is (unifications? [:struct {:i t-int :b t-bool}]
                     [:struct {:b t-int :i t-bool}]
                     [:struct {:i [:meet t-int t-bool]
                               :b [:meet t-bool t-int]}]
                     [:struct {:i [:join t-int t-bool]
                               :b [:join t-bool t-int]}]
                     nil))
  (is (unifications? [:struct {:i t-int :b t-bool}]
                     [:struct {:a t-bool :b t-bool :i t-int}]
                     [:struct {:a t-bool :i t-int :b t-bool}]
                     [:struct {:i t-int :b t-bool}]
                     [:struct {:i t-int :b t-bool}])))

(defn type?
  [e t]
  (= (expr e) [t {}]))

(deftest expr-basetypes
  (is (type? [:lit 1] t-int))
  (is (type? [:lit true] t-bool)))

(deftest expr-struct
  (let [s [:struct {:a [:lit 1] :b [:lit true]}]]
    (is (type? s [:struct {:a t-int :b t-bool}]))
    (is (type? [:get :a s] t-int))))

(def id [:abs :x [:var :x]])

(deftest expr-id
  (is (= (expr id) [[:abs [:top] [:top]] {}])))
