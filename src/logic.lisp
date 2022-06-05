(defpackage :logic/logic
  (:use :cl
        :logic/formula
        :logic/sequent
        :logic/goal
        :logic/axiom
        :logic/lk))
(in-package :logic/logic)

; Proof: https://gist.github.com/myaosato/fd198c9c211f541d6349f8df7ad899a7


;; ****************************************************************
;; theorem (printer)
;;
;; TODO
;;   - structure of theorem
;;   - separate definition(structure) and printer
;; ****************************************************************
(defmacro def-theorem (name antecedent succedent &body proof)
  (let ((current-goal (gensym "GOAL-"))
        (step (gensym "STEP-"))
        (n (gensym "N-"))
        (rule (gensym "RULE-"))
        (args (gensym "ARGS-"))
        (steps (mapcar (lambda (x) `(list ,(nth 0 x) ',(nth 1 x) ,@(nthcdr 2 x)))
                       proof)))
    `(defun ,name ()
       (let ((,current-goal (make-goal (make-sequent ,antecedent ,succedent))))
         (format t "↓↓↓ GOAL ↓↓↓ ~%")
         (print-goal ,current-goal)
         (loop :for ,step :in (list ,@steps)
               :for ,n := (car ,step)
               :for ,rule := (cadr ,step)
               :for ,args := (cddr ,step)
               :do (setf ,current-goal (do-step ,current-goal ,n ,rule ,args))
               :do (format t "↓↓↓ ~A ↓↓↓ ~%" ,rule)
               :do (print-goal ,current-goal))
         ,current-goal))))


;; ****************************************************************
;; ex. law of exclded middle
;; ⊢ A∨¬A
;; ****************************************************************
(let ((a (prop "A")))
  (def-theorem law-of-exclded-middle nil (list (∨ a (¬ a)))
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
  (def-theorem material-implication-1 (list (→ A B)) (list (∨ (¬ a) b))
    (0 cr)
    (0 or-r1)
    (0 pr 0 1)
    (0 or-r2)
    (0 pr 0 1)
    (0 to-l 1 1)
    (1 id)
    (0 pr 0 1)
    (0 not-r)
    (0 id)))


;; ****************************************************************
;; ex. material implication 2
;; ⊢ (¬A ∨ B) → (A → B)
;; ****************************************************************
(let ((a (prop "A"))
      (b (prop "B")))
  (def-theorem material-implication-2 nil (list (→ (∨ (¬ a) b) (→ A B)))
    (0 to-r)
    (0 to-r)
    (0 pl 0 1)
    (0 or-l 2 0)
    (1 id)
    (0 not-l)
    (0 id)))


;; ****************************************************************
;; ex. Peirce's law
;; ⊢ ((A → B) → A) → A
;; ****************************************************************
(let ((a (prop "A"))
      (b (prop "B")))
  (def-theorem |Perice's law| nil (list (→ (→ (→ a b) a) a))
    (0 to-r)
    (0 cr)
    (0 to-l 1 1)
    (1 id)
    (0 to-r)
    (0 wr)
    (0 id)))
