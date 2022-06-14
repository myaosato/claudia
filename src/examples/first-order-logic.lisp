(defpackage :claudia/examples/first-order-logic
  (:nicknames :claudia/example/fol)
  (:use :cl
        :claudia/theorem
        :claudia/term
        :claudia/formula
        :claudia/lk)
  (:export :specialization))
(in-package :claudia/examples/first-order-logic)

(def-predicate p 1)

(let* ((x (var "x"))
       (a (const "a")))
  (def-theorem specialization nil (list (→ (∀ x (p x)) (∃ x (p x))))
    (0 to-r)
    (0 forall-l a)
    (0 exists-r a)
    (0 id)))
