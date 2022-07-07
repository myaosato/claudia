(defpackage :claudia-test/all
  (:use :cl)
  (:import-from :claudia/examples/classical-propositional-logic)
  (:import-from :claudia/examples/first-order-logic)
  (:import-from :claudia/examples/monic-is-injective))
(in-package :claudia-test/all)

(defun test ()
  (claudia/examples/classical-propositional-logic:law-of-exclded-middle)
  (claudia/examples/classical-propositional-logic:material-implication)
  (claudia/examples/classical-propositional-logic:|Perice's law|)
  (claudia/examples/first-order-logic:specialization)
  (claudia/examples/monic-is-injective:injective->monic)
  (claudia/examples/monic-is-injective:monic->injective))
