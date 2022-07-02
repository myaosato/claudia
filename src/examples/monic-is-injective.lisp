(defpackage :claudia/examples/monic-is-injective
  (:use :cl
        :claudia/api/theorem)
  (:export :app
           :==
           :->
           :injective->monic
           :monic->injective))
(in-package :claudia/examples/monic-is-injective)

(def-const ==)
(def-const ->) ;; ((-> x) _) => x


(def-theorem injective->monic
    (→ (∀ x (∀ y (→ (== (f x) (f y)) (== x y))))
       (∀ g (∀ h (→ (∀ z (== (f (g z)) (f (h z))))
                    (∀ z (== (g z) (h z)))))))
    (:vars (x y f z g h))
  (to-r)
  (forall-r)
  (forall-r)
  (to-r)
  (forall-r)
  (forall-l z)
  (forall-l (terms (g z)) 0 1)
  (forall-l (terms (h z)) 0 1)
  (to-l 0 1)
  (id)
  (id))

(def-theorem monic->injective
    (→ (∀ g (∀ h (→ (∀ z (== (f (g z)) (f (h z))))
                    (∀ z (== (g z) (h z))))))
       (∀ x (∀ y (→ (== (f x) (f y)) (== x y)))))
    (:vars (x y f g h z))
  ;; WIP) just memo, it is not proof 
  (to-r)
  (forall-r)
  (forall-r)
  (forall-l (terms (-> x)))
  (forall-l (terms (-> y)))
  (to-r)
  (to-l 0 1)
  (forall-r)
  (wr 0 1)
  (forall-l z 1 1)
  (wl 1 0))
