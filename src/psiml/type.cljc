(ns psiml.type
  "Types abstract syntax, producing typed abstract syntax"
  #?(:cljs (:require-macros psiml.type))
  (:require [clojure.core.match :refer [match]]))

;; typed ast {:node n :type t]
;; {:node [:abs :x {:node [:var :x] :type [:t-var]}]
;;  :type [:abs [:t-var] [:t-var]]}

;; [:t :int]
;; [:t :bool]
;; [:abs t t]
;; [:rec t]
;; {:struct {field -> e}

;; [:top]
;; [:bot]
;; [:meet t t]
;; [:join t t]

(defn map-env
  "Returns (f t env) if t is not nil, otherwise [t env]"
  [[t env] f]
  (if (nil? t) [t env] (f t env)))

(defmacro with-env
  "Passes an environment through a series of operations,
  binding the results and returning a final value.

  The operations are given the environment as an additional
  argument and should return a value of the form [result new-env].
  The binding itself is done using map-env, thus aborting the
  series of operations after an error."
  ([env r] `[~r ~env])
  ([env b f & binds]
   (let [senv (if (symbol? env) env (gensym))]
     `(map-env (->> ~env ~f)
               (fn [~b ~senv] (with-env ~senv ~@binds))))))

(defn env-id
  "Returns `v` without affecting the environment"
  [v env]
  [v env])

(declare unify-input)
(declare unify-output)

(defn unify-input
  [t1 t2 env]
  (match [t1 t2]
    [[:top] t] [t env]
    [t [:top]] [t env]
    [[:t b1] [:t b2]]
    [(if (= b1 b2) [:t b1] [:meet [:t b1] [:t b2]]) env]
    [[:struct tm1] [:struct tm2]]
    (with-env env
      t-m (#(reduce
              (partial reduce (fn [[t-m env] [l t]]
                                (if (contains? t-m l)
                                  (with-env env
                                    t (unify-input (t-m l) t)
                                    (assoc t-m l t))
                                  [(assoc t-m l t) env])))
              [{} %]
              [tm1 tm2]))
      [:struct t-m])
    [[:abs t1-out t1-in] [:abs t2-out t2-in]]
    (with-env env
      t-in (unify-input t1-in t2-in)
      t-out (unify-output t1-out t2-out)
      [:abs t-out t-in])
    [[:rec t1] [:rec t2]]
    (with-env env
      t (unify-input t1 t2)
      [:rec t])
    :else [nil env]))

(defn unify-output
  [t1 t2 env]
  (match [t1 t2]
    [[:bot] t] [t env]
    [t [:bot]] [t env]
    [[:t b1] [:t b2]]
    [(if (= b1 b2) [:t b1] [:join [:t b1] [:t b2]]) env]
    [[:struct tm1] [:struct tm2]]
    (with-env env
      t-m (#(reduce (fn [[t-m env] [l t1]]
                      (if-let [t2 (tm2 l)]
                        (with-env env
                          t (unify-output t1 t2)
                          (assoc t-m l t))
                        ([t-m env])))
                    [{} %] tm1))
      [:struct t-m])
    [[:abs t1-in t1-out] [:abs t2-in t2-out]]
    (with-env env
      t-in (unify-input t1-in t2-in)
      t-out (unify-output t1-out t2-out)
      [:abs t-in t-out])
    [[:rec t1] [:rec t2]]
    (with-env env
      t (unify-output t1 t2)
      [:rec t])
    :else [nil env]))

(defn bi-unify
  [t-in t-out env]
  (match [t-in t-out]
    [[:top] t] [t env]
    [_ [:bot]] [[:bot] env]
    [[:t a] [:t b]]
    [(if (= a b) [:t a]) env]
    [[:struct tm-in] [:struct tm-out]]
    (with-env env
      t-m (#(reduce (fn [[t-m env] [l t-in]]
                      (if-let [t-out (tm-out l)]
                        (with-env env
                          t (bi-unify t-in t-out)
                          (assoc t-m l t))
                        [nil env]))
                    [{} %]
                    tm-in))
      [:struct t-m])
    [[:abs t-in-out t-in-in] [:abs t-out-in t-out-out]]
    (with-env env
      t-a (bi-unify t-out-in t-in-out)
      t-b (bi-unify t-in-in t-out-out)
      [:abs t-a t-b])
    [[:rec t-in] [:rec t-out]]
    [nil env] ; TODO
    [[:meet t1-in t2-in] _]
    (with-env env
      t1 (bi-unify t1-in t-out)
      t2 (bi-unify t2-in t-out)
      t (unify-output t1 t2)
      t)
    [_ [:join t1-out t2-out]]
    (with-env env
      t1 (bi-unify t-in t1-out)
      t2 (bi-unify t-in t2-out)
      t (unify-output t1 t2)
      t)
    :else [nil env]))

(defn with-var
  [n f env]
  (let [shadow (env n)
        [t-f env] (f (assoc env n [:top]))
        t-n (env n)]
    [[t-n t-f] (if shadow (assoc env n shadow) (dissoc env n))]))

(defn expr
  "Types an expression"
  ([e] (expr e {}))
  ([exp env]
   (match exp
     [:abs n e]
     (with-env env
       [t-n t-e] (with-var n #(expr e %))
       [:abs t-n t-e])
     [:rec n e]
     (with-env env
       [t-n t-e] (with-var n #(expr e %))
       t (unify-output t-n t-e)
       [:rec n t])
     [:lit c] [(cond (integer? c) [:t :int]
                     (or (true? c) (false? c)) [:t :bool])
               env]
     [:var n] [(env n) env]
     [:app e1 e2]
     (with-env env
       t1 (expr e1)
       t2 (expr e2)
       [t-in t-out] (env-id (match t1
                              [:abs a b] [a b]
                              :else nil))
       _ (bi-unify t-in t2)
       t-out)
     [:struct m]
     (with-env env
       t-m (#(reduce (fn [[t-m env] [l e]]
                       (with-env env
                         t (expr e)
                         (assoc t-m l t))) [{} %] m))
       [:struct t-m])
     [:get l e]
     (with-env env
       t (expr e)
       (match t [:struct t-m] (t-m l) :else nil)))))
