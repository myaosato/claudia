(defpackage :claudia/examples/first-order-logic
  (:nicknames :claudia/examples/fol)
  (:use :cl
        :claudia/theorem
        :claudia/term
        :claudia/formula
        :claudia/command)
  (:export :specialization))
(in-package :claudia/examples/first-order-logic)

(def-predicate p 1)

(def-theorem specialization (→ (∀ x (p x)) (∃ x (p x))) goal
    nil (x y)
  (to-r goal 0)
  (forall-l goal 0 y)
  (exists-r goal 0 y)
  (id goal 0))
