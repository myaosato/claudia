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
        (args (gensym "ARGS-")))
    `(defun ,name ()
       (let ((,current-goal (make-goal (make-sequent ,antecedent ,succedent))))
         (format t "↓↓↓ GOAL ↓↓↓ ~%")
         (print-goal ,current-goal)
         (loop :for ,step :in ',proof
               :for ,n := (car ,step)
               :for ,rule := (cadr ,step)
               :for ,args := (cddr ,step)
               :do (setf ,current-goal (do-step ,current-goal ,n ,rule ,args))
               :do (format t "↓↓↓ ~A ↓↓↓ ~%" ,rule)
               :do (print-goal ,current-goal))
         ,current-goal))))


;; ****************************************************************
;; ex. law-of-exclded-middle
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
