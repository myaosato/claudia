(defpackage :claudia/examples/monic-is-injective
  (:use :cl
        :claudia/theorem)
  (:export :app
           :==
           :->
           :injective->monic
           :monic->injective))
(in-package :claudia/examples/monic-is-injective)

(def-predicate == 2)
(def-const ->) ;; ((-> x) _) => x


(def-theorem injective->monic
    (→ (∀ x (∀ y (→ (== (func f x) (func f y)) (== x y))))
       (∀ g (∀ h (→ (∀ z (== (func f (func g z)) (func f (func h z))))
                    (∀ z (== (func g z) (func h z)))))))
    (:vars (x y f z g h))
  (to-r)
  (forall-r)
  (forall-r)
  (to-r)
  (forall-r)
  (forall-l z)
  (forall-l (func g z) 0 1)
  (forall-l (func h z) 0 1)
  (to-l 0 1)
  (id)
  (id))

(def-theorem monic->injective
    (→ (∀ g (∀ h (→ (∀ z (== (func f (func g z)) (func f (func h z))))
                    (∀ z (== (func g z) (func h z))))))
       (∀ x (∀ y (→ (== (func f x) (func f y)) (== x y)))))
    (:vars (x y f g h z))
  ;; WIP) just memo, it is not proof 
  (to-r)
  (forall-r)
  (forall-r)
  (forall-l (func -> x))
  (forall-l (func -> y))
  (to-r)
  (to-l 0 1)
  (forall-r)
  (wr 0 1)
  (forall-l z 1 1)
  (wl 1 0))
