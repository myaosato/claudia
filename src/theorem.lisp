(defpackage :claudia/theorem
  (:use :cl
        :claudia/sequent
        :claudia/goal
        :claudia/pprint)
  (:export :def-theorem))
(in-package :claudia/theorem)

;; ****************************************************************
;; theorem (printer)
;;
;; TODO
;;   - structure of theorem
;;   - separate definition(structure) and printer
;; ****************************************************************
(defmacro def-theorem (name theorem &body proof)
  (let ((current-goal (gensym "GOAL-"))
        (step (gensym "STEP-"))
        (n (gensym "N-"))
        (rule (gensym "RULE-"))
        (args (gensym "ARGS-"))
        (steps (mapcar (lambda (x) `(list ,(nth 0 x) ',(nth 1 x) ,@(nthcdr 2 x)))
                       proof)))
    `(defun ,name ()
       (let ((,current-goal (goal (sequent nil (list ,theorem)))))
         (format t "~16,,,'-A [GOAL]~%" "")
         (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
           (format t "~W~%" ,current-goal))
         (loop :for ,step :in (list ,@steps)
               :for ,n := (car ,step)
               :for ,rule := (cadr ,step)
               :for ,args := (cddr ,step)
               :do (setf ,current-goal (do-step ,current-goal ,n ,rule ,args))
               :do (format t "~16,,,'-A [~A]~%" "" ,rule)
               :do (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
                     (format t "~W~%" ,current-goal)))
         ,current-goal))))
