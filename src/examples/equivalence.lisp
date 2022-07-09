(defpackage :claudia/examples/equivalence
  (:use :cl
        :claudia/api/theorem)
  (:export :==
           :curried-==-1
           :curried-==-2
           :reflexivity
           :substitutivity
           :symmetry))
(in-package :claudia/examples/equivalence)

(def-const ==)
(def-axiom reflexivity (x) (∀ x (== x x)))
(def-axiom substitutivity (x y p) (∀ x (∀ y (→ (== x y) (∀ p (→ (p x) (p y)))))))
(defvar curried-==-1 (rule (x y) ((== x) y) (== x y)))
(defvar curried-==-2 (rule (x y) ((== x) y) (== y x)))

(def-theorem symmetry
    (∀ x (∀ y (→ (== x y) (== y x))))
    (:vars (x y))
  (forall-r)
  (forall-r)
  (to-r)
  (cut reflexivity)
  (app-a reflexivity)
  (forall-l x)
  (cut substitutivity)
  (app-a substitutivity)
  (forall-l x)
  (forall-l y)
  (to-l)
  (id)
  (forall-l (terms (== x)))
  (rewrite-l curried-==-2)
  (to-l)
  (id)
  (id))
