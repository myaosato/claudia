(defpackage :claudia/theorem
  (:use :cl
        :claudia/command)
  (:import-from :claudia/term
                :var)
  (:import-from :claudia/formula
                :prop)
  (:import-from :claudia/sequent
                :sequent)
  (:import-from :claudia/goal
                :goal)
  (:import-from :claudia/pprint
                :print-claudia-print-dispatch)
  (:export :def-theorem))
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
       (let ((,current-goal (goal (sequent nil (list ,theorem))))
             (*print-pprint-dispatch* print-claudia-print-dispatch))
         (format t "~16,,,'-A [GOAL]~%" "")
         (format t "~W~%" ,current-goal)
         ,@(mapcar (lambda (command)
                     `(progn 
                        (setf ,current-goal ,command)
                        (format t "~16,,,'-A [~A]~%" "" ',(car command))
                        (format t "~W~%" ,current-goal)))
                   proof)))))