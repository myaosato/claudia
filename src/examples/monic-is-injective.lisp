(defpackage :claudia/examples/monic-is-injective
  (:use :cl
        :claudia/theorem)
  (:export :app
           :==
           :->
           :injective->monic
           :monic->injective))
(in-package :claudia/examples/monic-is-injective)

(def-func app *)
(def-predicate == 2)
(def-const ->) ;; (app (app -> x) _) => x


(def-theorem injective->monic
    (→ (∀ x (∀ y (→ (== (app f x) (app f y)) (== x y))))
       (∀ g (∀ h (→ (∀ z (== (app f (app g z)) (app f (app h z))))
                    (∀ z (== (app g z) (app h z)))))))
    (:vars (x y f z g h))
  (to-r)
  (forall-r)
  (forall-r)
  (to-r)
  (forall-r)
  (forall-l z)
  (forall-l (app g z) 0 1)
  (forall-l (app h z) 0 1)
  (to-l 0 1)
  (id)
  (id))

(def-theorem monic->injective
    (→ (∀ g (∀ h (→ (∀ z (== (app f (app g z)) (app f (app h z))))
                    (∀ z (== (app g z) (app h z))))))
       (∀ x (∀ y (→ (== (app f x) (app f y)) (== x y)))))
    (:vars (x y f g h z))
  ;; WIP) just memo, it is not proof 
  (to-r)
  (forall-r)
  (forall-r)
  (forall-l (app -> x))
  (forall-l (app -> y))
  (to-r)
  (to-l 0 1)
  (forall-r)
  (wr 0 1)
  (forall-l z 1 1)
  (wl 1 0))
