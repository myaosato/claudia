(defpackage :claudia/api/theorem
  (:use :cl
        :claudia/environment
        :claudia/command)
  (:import-from :claudia/meta-data/interface
                :var :const :func
                :∧ :∨ :¬ :→ :∀ :∃ :prop :predicate
                :formulas :terms)
  (:import-from :claudia/pattern/interface
                :rule)
  (:import-from :claudia/sequent
                :sequent)
  (:import-from :claudia/goal
                :goal)
  (:import-from :claudia/print/interface
                :print-claudia-print-dispatch)
  (:export :def-theorem
           :def-const
           :def-axiom
           ;; command
           :app-a
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
           :rewrite-l :rewrite-r
           ;; pattern
           :rule
           ;; meta-data
           :∧ :∨ :¬ :→ :∀ :∃ :predicate
           :func
           :formulas :terms))
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
     (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
       (let (,@(mapcar (lambda (sym) (list sym `(prop ',sym))) props)
             ,@(mapcar (lambda (sym) (list sym `(var ',sym))) vars))
         (with-current-goal (goal (sequent nil (list (formulas ,theorem))))
           (format t "~16,,,'-A [GOAL]~%" "")
           (format t "~W~%" current-goal)
           ,@proof)))))

(defmacro def-const (sym)
  `(defvar ,sym (const ',sym)))

(defmacro def-axiom (name vars axiom)
  `(defvar ,name (let ,(mapcar (lambda (s) `(,s (var ',s))) vars)
                   (formulas ,axiom))))
