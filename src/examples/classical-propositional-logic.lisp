(defpackage :claudia/examples/classical-propositional-logic
  (:nicknames :claudia/examples/cpl)
  (:use :cl
        :claudia/theorem
        :claudia/formula
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
  (cr goal 0)
  (or-r1 goal 0)
  (or-r2 goal 0 1)
  (not-r goal 0 1)
  (id goal 0))

;; ****************************************************************
;; ex. material implication 1
;; A → B ⊢ ¬A ∨ B
;; ****************************************************************
(def-theorem material-implication-1 (→ (→ A B) (∨ (¬ a) b)) goal
    (a b) nil
  (to-r goal 0)
  (to-l goal 0)
  (or-r1 goal 0 1)
  (or-r2 goal 1)
  (not-r goal 0 1)
  (id goal 0)
  (id goal 0))

;; ****************************************************************
;; ex. material implication 2
;; ⊢ (¬A ∨ B) → (A → B)
;; ****************************************************************
(def-theorem material-implication-2 (→ (∨ (¬ a) b) (→ A B)) goal
    (a b) nil
  (to-r goal 0)
  (to-r goal 0)
  (or-l goal 0 1)
  (not-l goal 0 1)
  (id goal 0)
  (id goal 0))

;; ****************************************************************
;; ex. Peirce's law
;; ⊢ ((A → B) → A) → A
;; ****************************************************************
(def-theorem |Perice's law| (→ (→ (→ a b) a) a) goal
    (a b) nil
  (to-r goal 0)
  (to-l goal 0)
  (id goal 1)
  (to-r goal 0)
  (id goal 0))
