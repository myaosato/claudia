(defpackage :claudia/examples/first-order-logic
  (:nicknames :claudia/examples/fol)
  (:use :cl
        :claudia/theorem)
  (:export :specialization))
(in-package :claudia/examples/first-order-logic)

(def-predicate p 1)

(def-theorem specialization (→ (∀ x (p x)) (∃ x (p x)))
  (:vars (x y))
  (to-r 0)
  (forall-l 0 y)
  (exists-r 0 y)
  (id 0))
