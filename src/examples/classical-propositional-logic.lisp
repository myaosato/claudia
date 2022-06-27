(defpackage :claudia/examples/classical-propositional-logic
  (:nicknames :claudia/examples/cpl)
  (:use :cl
        :claudia/theorem
        :claudia/command)
  (:export :law-of-exclded-middle
           :material-implication
           :|Perice's law|))
(in-package :claudia/examples/classical-propositional-logic)

;; ****************************************************************
;; ex. law of exclded middle
;; ⊢ A∨¬A
;; ****************************************************************
(def-theorem law-of-exclded-middle (∨ a (¬ a))
  (:props (a))
  (or-r)
  (not-r 0 1)
  (id))

;; ****************************************************************
;; ex. material implication
;; A → B ⊢ ¬A ∨ B
;; ****************************************************************
(def-theorem material-implication (∧ (→ (→ A B) (∨ (¬ a) b)) (→ (∨ (¬ a) b) (→ A B)))
  (:props (a b))
  (and-r)
  (to-r)
  (or-r)
  (to-l)
  (not-r 0 1)
  (id)
  (id)
  (to-r)
  (to-r)
  (or-l 0 1)
  (not-l 0 1)
  (id)
  (id))

;; ****************************************************************
;; ex. Peirce's law
;; ⊢ ((A → B) → A) → A
;; ****************************************************************
(def-theorem |Perice's law| (→ (→ (→ a b) a) a)
  (:props (a b))
  (to-r)
  (to-l)
  (id 1)
  (to-r)
  (id))
