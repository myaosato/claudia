(defpackage :claudia/examples/equivalence
  (:use :cl
        :claudia/api/theorem)
  (:export :==
           :curried-==-1
           :curried-==-2
           :reflexivity
           :substitutivity
           :symmetry
           :transitivity))
(in-package :claudia/examples/equivalence)

(def-const ==)
(def-axiom reflexivity (x) (== x x))
(def-axiom substitutivity (x y p) (→ (== x y) (∀ p (→ (p x) (p y)))))
(defvar curried-==-1 (rule (x y) ((== x) y) (== x y)))
(defvar curried-==-2 (rule (x y) ((== x) y) (== y x)))

(def-theorem symmetry
    (→ (== x y) (== y x))
    (:vars (x y))
  (to-r)
  (cut reflexivity)
  (app-a reflexivity)
  (cut substitutivity)
  (app-a substitutivity)
  (to-l)
  (id)
  (forall-l (terms (== x)))
  (rewrite-l curried-==-2)
  (to-l)
  (id)
  (id))

(def-theorem transitivity
    (→ (∧ (== x y) (== y z)) (== x z))
    (:vars (x y z p))
  (to-r)
  (and-l)
  (cut (formulas (→ (== y x) (∀ p (→ (p y) (p x))))))
  (app-a substitutivity)
  (to-l)
  (cut symmetry)
  (app-a symmetry)
  (to-l)
  (id)
  (id)
  (forall-l (terms (== z)))
  (rewrite-l curried-==-2)
  (to-l)
  (id)
  (id))
