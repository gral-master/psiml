(ns psiml.type-test
  (:require [clojure.test :refer :all]
            [psiml.type :refer :all]))

(def t-int [:t :int])
(def t-bool [:t :bool])

(deftest test-free-vars
  (is (= (free-vars [:var :x] #{}) #{:x}))
  (is (= (free-vars [:var :x] #{:x}) #{}))
  (is (= (free-vars [:t-abs :x [:var :x]] #{}) #{}))
  (is (= (free-vars
           [:t-abs :x [:abs [:var :x] [:var :y]]] #{})
         #{:y})))

(defn unifications?
  [t1 t2 t-input t-output t-bi]
  (or (= (meet t1 t2 {}) [t-input {}])
      (= (join t1 t2 {}) [t-output {}])
      (= (biunify t1 t2 {}) [t-bi {}])))

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

(deftest test-eq?
  (is (eq? [:t-abs :a [:var :a]]
           [:t-abs :b [:var :b]])))

(defn is-typed?
  ([e t] (is-typed? e t {}))
  ([e t vars]
   (let [[t' env] (expr e {:vars vars :bound-t-vars #{}})]
     (is (eq? (first (abstract-types t' env)) t)))))

(deftest expr-basetypes
  (is-typed? [:lit 1] t-int)
  (is-typed? [:lit true] t-bool))

(deftest expr-struct
  (let [s [:struct {:a [:lit 1] :b [:lit true]}]]
    (is-typed? s [:struct {:a t-int :b t-bool}])
    (is-typed? [:get :a s] t-int)))

(def id [:abs :x [:var :x]])

(deftest expr-id
  (is-typed? id [:t-abs :a [:abs [:var :a] [:var :a]]]))

(deftest select-int-bool
  (is-typed? [:abs :p
              [:abs :v
               [:abs :d
                [:if [:app [:var :p] [:var :v]]
                 [:app [:var :int-id] [:var :v]]
                 [:app [:var :bool-id] [:var :d]]]]]]
             [:abs [:abs t-int t-bool]
              [:abs t-int
               [:abs t-bool
                [:join t-int t-bool]]]]
             {:int-id [:abs t-int t-int]
              :bool-id [:abs t-bool t-bool]}))

(deftest select
  (is-typed? [:abs :p
              [:abs :v
               [:abs :d
                [:if [:app [:var :p] [:var :v]]
                 [:var :v]
                 [:var :d]]]]]
             [:t-abs :v
              [:t-abs :d
               [:abs [:abs [:var :v] t-bool]
                [:abs [:var :v]
                 [:abs [:var :d]
                  [:join [:var :v] [:var :d]]]]]]]))

;; NEXT TARGET:
;; (deftest meet-and
;;   (is-typed? [:abs :p
;;               [:abs :a
;;                [:abs :b
;;                 [:if [:app [:var :p] [:var :a]]
;;                  [:app [:var :p] [:var :b]]
;;                  [:lit false]]]]]
;;              [:t-abs :a
;;               [:t-abs :b
;;                [:abs [:abs [:meet [:var :a] [:var :b]] t-bool]
;;                 [:abs [:var :a]
;;                  [:abs [:var :b]
;;                   t-bool]]]]]))
