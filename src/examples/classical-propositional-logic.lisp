(defpackage :claudia/examples/classical-propositional-logic
  (:nicknames :claudia/examples/cpl)
  (:use :cl
        :claudia/theorem
        :claudia/lk)
  (:export :law-of-exclded-middle
           :material-implication-1
           :material-implication-2
           :|Perice's law|))
(in-package :claudia/examples/classical-propositional-logic)

;; ****************************************************************
;; ex. law of exclded middle
;; ⊢ A∨¬A
;; ****************************************************************
(let ((a (prop "A")))
  (def-theorem law-of-exclded-middle (∨ a (¬ a))
    (0 cr)
    (0 or-r1)
    (0 pr 0 1)
    (0 or-r2)
    (0 not-r)
    (0 id)))

;; ****************************************************************
;; ex. material implication 1
;; A → B ⊢ ¬A ∨ B
;; ****************************************************************
(let ((a (prop "A"))
      (b (prop "B")))
  (def-theorem material-implication-1 (→ (→ A B) (∨ (¬ a) b))
    (0 to-r)
    (0 cr)
    (0 or-r1)
    (0 pr 0 1)
    (0 or-r2)
    (0 pr 0 1)
    (0 to-l)
    (0 pr 0 1)
    (0 not-r)
    (0 wr 1)
    (0 id)
    (0 wr 0)
    (0 id)))

;; ****************************************************************
;; ex. material implication 2
;; ⊢ (¬A ∨ B) → (A → B)
;; ****************************************************************
(let ((a (prop "A"))
      (b (prop "B")))
  (def-theorem material-implication-2 (→ (∨ (¬ a) b) (→ A B))
    (0 to-r)
    (0 to-r)
    (0 pl 0 1)
    (0 or-l)
    (1 wl 1)
    (1 id)
    (0 not-l)
    (0 wr 1)
    (0 id)))

;; ****************************************************************
;; ex. Peirce's law
;; ⊢ ((A → B) → A) → A
;; ****************************************************************
(let ((a (prop "A"))
      (b (prop "B")))
  (def-theorem |Perice's law| (→ (→ (→ a b) a) a)
    (0 to-r)
    (0 to-l)
    (1 id)
    (0 to-r)
    (0 wr 0)
    (0 id)))
