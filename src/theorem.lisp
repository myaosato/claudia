(defpackage :claudia/theorem
  (:use :cl
        :claudia/environment
        :claudia/command)
  (:import-from :claudia/term
                :var :def-func)
  (:import-from :claudia/formula
                :∧ :∨ :¬ :→ :∀ :∃ :prop :def-predicate)
  (:import-from :claudia/sequent
                :sequent)
  (:import-from :claudia/goal
                :goal)
  (:import-from :claudia/pprint
                :print-claudia-print-dispatch)
  (:export :def-theorem
           ;; command
           :id :cut
           :and-l1 :and-l2 :and-r
           :or-l :or-r1 :or-r2
           :not-l :not-r
           :to-l :to-r
           :forall-l :forall-r
           :exists-l :exists-r
           :wl :wr
           :cl :cr
           :pl :pr
           ;; formula
           :∧ :∨ :¬ :→ :∀ :∃ :prop :def-predicate
           ;; 
           :def-func))
(in-package :claudia/theorem)

;; ****************************************************************
;; theorem (printer)
;;
;; TODO
;;   - structure of theorem
;;   - separate definition(structure) and printer
;; ****************************************************************

(defmacro def-theorem (name theorem current-goal props vars &body proof)
  `(defun ,name ()
     (let (,@(mapcar (lambda (sym) (list sym `(prop ',sym))) props)
           ,@(mapcar (lambda (sym) (list sym `(var ',sym))) vars))
       (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
         (with-current-goal (goal (sequent nil (list ,theorem)))
           (format t "~16,,,'-A [GOAL]~%" "")
           (format t "~W~%" current-goal)
           ,@(mapcar (lambda (command)
                       `(progn 
                          (setf current-goal ,command)
                          (format t "~16,,,'-A [~A]~%" "" ',(car command))
                          (format t "~W~%" current-goal)))
                     proof))))))
