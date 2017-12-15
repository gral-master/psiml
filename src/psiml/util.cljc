(ns psiml.util
  #?(:cljs (:require-macros psiml.util)))

(defn by-two
  ([s] (by-two s nil))
  ([s e] (if-let [s (seq s)]
           (if e
             (lazy-seq (cons [e (first s)] (by-two (rest s) nil)))
             (by-two (rest s) (first s)))
           s)))

(defmacro match-first
  [& binds]
  (let [e (gensym)
        tests (map (fn [[p _]] `(= ~(first p) (first ~e)))
                   (by-two binds))
        handlers (map (fn [[p b]] `(fn [~@(rest p)] ~b))
                      (by-two binds))
        handler `(cond ~@(interleave tests handlers))]
    `(fn [~e] (apply ~handler (rest ~e)))))
