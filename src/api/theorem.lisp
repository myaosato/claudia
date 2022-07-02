(defpackage :claudia/api/theorem
  (:use :cl
        :claudia/environment
        :claudia/command)
  (:import-from :claudia/meta-data/interface
                :var :const :func
                :∧ :∨ :¬ :→ :∀ :∃ :prop :predicate)
  (:import-from :claudia/sequent
                :sequent)
  (:import-from :claudia/goal
                :goal)
  (:import-from :claudia/print/pprint
                :print-claudia-print-dispatch)
  (:export :def-theorem
           :def-const
           ;; command
           :id :cut
           :and-l :and-r
           :or-l :or-r
           :not-l :not-r
           :to-l :to-r
           :forall-l :forall-r
           :exists-l :exists-r
           :wl :wr
           :cl :cr
           :pl :pr
           ;; formula
           :∧ :∨ :¬ :→ :∀ :∃ :predicate
           ;; term
           :func))
(in-package :claudia/api/theorem)

;; ****************************************************************
;; theorem (printer)
;;
;; TODO
;;   - structure of theorem
;;   - separate definition(structure) and printer
;; ****************************************************************

(defmacro def-theorem (name theorem (&key (props nil) (vars nil)) &body proof)
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

(defmacro def-const (sym)
  (declare (type symbol sym))
  `(defvar ,sym (const ',sym)))
