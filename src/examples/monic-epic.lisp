(defpackage :claudia/examples/monic-epic
  (:use :cl
        :claudia/api/theorem
        :claudia/examples/equivalence)
  (:export :app
           :==
           :constant
           :rule-constant
           :injective->monic
           :monic->injective
           :surjective->epic))
(in-package :claudia/examples/monic-epic)

(def-const constant)
(defvar rule-constant (rule (x y) ((constant x) y) x))
(def-const pointwise-==)
(defvar rule-pointwise-== (rule (g h x) ((pointwise-== g h) x) (== (g x) (h x))))


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
  (to-r)
  (forall-r)
  (forall-r)
  (forall-l (terms (constant x)))
  (forall-l (terms (constant y)))
  (to-r)
  (to-l 0 1)
  (forall-r)
  (wr 0 1)
  (forall-l z 1 1)
  (wl 1 0)
  (rewrite-r rule-constant 0)
  (id)
  (rewrite-l rule-constant 0)
  (id))

(def-theorem surjective->epic
    (→ (∀ y (∃ x (== (f x) y)))
       (∀ g (∀ h (→ (∀ x (== (g (f x)) (h (f x))))
                    (∀ y (== (g y) (h y)))))))
    (:vars (x y f g h p))
  (to-r)
  (forall-r)
  (forall-r)
  (to-r)
  (forall-r)
  (cut (formulas (∀ x (∀ y (→ (== x y) (∀ p (→ (p x) (p y))))))))
  (forall-r)
  (wr 0 1)
  (forall-r)
  (app-a substitutivity)
  (forall-l y 0 2)
  (exists-l 0 2)
  (forall-l (terms (f x)))
  (forall-l y)
  (to-l)
  (id)
  (forall-l (terms (pointwise-== g h)))
  (rewrite-l rule-pointwise-==)
  (forall-l x 0 1)
  (to-l)
  (id)
  (id))

