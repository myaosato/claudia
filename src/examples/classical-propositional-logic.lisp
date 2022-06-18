(defpackage :claudia/examples/classical-propositional-logic
  (:nicknames :claudia/examples/cpl)
  (:use :cl
        :claudia/theorem
        :claudia/command)
  (:export :law-of-exclded-middle
           :material-implication-1
           :material-implication-2
           :|Perice's law|))
(in-package :claudia/examples/classical-propositional-logic)

;; ****************************************************************
;; ex. law of exclded middle
;; ⊢ A∨¬A
;; ****************************************************************
(def-theorem law-of-exclded-middle (∨ a (¬ a)) goal
    (a) nil
  (cr 0)
  (or-r1 0)
  (or-r2 0 1)
  (not-r 0 1)
  (id 0))

;; ****************************************************************
;; ex. material implication 1
;; A → B ⊢ ¬A ∨ B
;; ****************************************************************
(def-theorem material-implication-1 (→ (→ A B) (∨ (¬ a) b)) goal
    (a b) nil
  (to-r 0)
  (to-l 0)
  (or-r1 0 1)
  (or-r2 1)
  (not-r 0 1)
  (id 0)
  (id 0))

;; ****************************************************************
;; ex. material implication 2
;; ⊢ (¬A ∨ B) → (A → B)
;; ****************************************************************
(def-theorem material-implication-2 (→ (∨ (¬ a) b) (→ A B)) goal
    (a b) nil
  (to-r 0)
  (to-r 0)
  (or-l 0 1)
  (not-l 0 1)
  (id 0)
  (id 0))

;; ****************************************************************
;; ex. Peirce's law
;; ⊢ ((A → B) → A) → A
;; ****************************************************************
(def-theorem |Perice's law| (→ (→ (→ a b) a) a) goal
    (a b) nil
  (to-r 0)
  (to-l 0)
  (id 1)
  (to-r 0)
  (id 0))
