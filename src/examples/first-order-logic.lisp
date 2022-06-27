(defpackage :claudia/examples/first-order-logic
  (:nicknames :claudia/examples/fol)
  (:use :cl
        :claudia/api/theorem)
  (:export :specialization))
(in-package :claudia/examples/first-order-logic)

(def-const p)

(def-theorem specialization (→ (∀ x (predicate p x)) (∃ x (predicate p x)))
  (:vars (x))
  (to-r)
  (forall-l x)
  (exists-r x)
  (id))
