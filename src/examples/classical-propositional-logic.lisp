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
(def-theorem law-of-exclded-middle (∨ a (¬ a))
  (:props (a))
  (cr)
  (or-r1)
  (or-r2 0 1)
  (not-r 0 1)
  (id))

;; ****************************************************************
;; ex. material implication 1
;; A → B ⊢ ¬A ∨ B
;; ****************************************************************
(def-theorem material-implication-1 (→ (→ A B) (∨ (¬ a) b))
  (:props (a b))
  (to-r)
  (to-l)
  (or-r1 0 1)
  (or-r2 1)
  (not-r 0 1)
  (id)
  (id))

;; ****************************************************************
;; ex. material implication 2
;; ⊢ (¬A ∨ B) → (A → B)
;; ****************************************************************
(def-theorem material-implication-2 (→ (∨ (¬ a) b) (→ A B))
  (:props (a b))
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
