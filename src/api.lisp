(defpackage :claudia/api
  (:use :cl)
  (:import-from :claudia/environment
                :history
                :current-goal
                :reset-claudia-environment)
  (:import-from :claudia/command)
  (:import-from :claudia/term
                :def-func :const)
  (:import-from :claudia/formula
                :∧ :∨ :¬ :→ :∀ :∃ :def-predicate)
  (:import-from :claudia/sequent
                :sequent)
  (:import-from :claudia/goal
                :goal)
  (:import-from :claudia/pprint
                :print-claudia-print-dispatch)
  (:export :start-proof :proof-hist
           :prop :var
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
           :props :vars
           ;; formula
           :∧ :∨ :¬ :→ :∀ :∃ :def-predicate
           ;; term
           :def-func :const
           ;; environment
           :reset-claudia-environment))
(in-package :claudia/api)

(defmacro prop (sym)
  `(progn
     (defvar ,sym (claudia/formula:prop ',sym))
     (push (cons ',sym ,sym) claudia/environment:props)
     (format t "PROP: ~A: ~A~%" ',sym ,sym)
     ,sym))

(defmacro var (sym)
  `(progn
     (defvar ,sym (claudia/term:var ',sym))
     (push (cons ',sym ,sym) claudia/environment:vars)
     (format t "VAR: ~A: ~A~%" ',sym ,sym)
     ,sym))

(defmacro start-proof (theorem &key (props nil) (vars nil))
  `(progn
     (reset-claudia-environment)
     ,@(mapcar (lambda (sym) (list 'prop sym)) props)
     ,@(mapcar (lambda (sym) (list 'var sym)) vars)
     (setf current-goal (goal (sequent nil (list ,theorem))))
     (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
       (format t "~16,,,'-A [GOAL]~%" "")
       (format t "~W~%" current-goal)
       (push (cons nil current-goal) history))
     current-goal))

(defmacro with-environment (command)
  ;; TODO error-handling
  `(let ((new-goal ,command))
     (setf current-goal new-goal)
     (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
       (format t "~16,,,'-A [~A]~%" "" ',(car command))
       (format t "~W~%" current-goal)
       (push (cons ',command current-goal) history))
     current-goal))

;; api comannd
(defun id (n)
  (with-environment (claudia/command:id n)))

(defun cut (n formula)
  (with-environment (claudia/command:cut n formula)))

(defun and-l1 (n &optional (m 0))
  (with-environment (claudia/command:and-l1 n m)))

(defun and-l2 (n &optional (m 0))
  (with-environment (claudia/command:and-l2 n m)))

(defun and-r (n &optional (m 0))
  (with-environment (claudia/command:and-r n m)))

(defun or-l (n &optional (m 0))
  (with-environment (claudia/command:or-l n m)))

(defun or-r1 (n &optional (m 0))
  (with-environment (claudia/command:or-r1 n m)))

(defun or-r2 (n &optional (m 0))
  (with-environment (claudia/command:or-r2 n m)))

(defun not-l (n &optional (m 0))
  (with-environment (claudia/command:not-l n m)))

(defun not-r (n &optional (m 0))
  (with-environment (claudia/command:not-r n m)))

(defun to-l (n &optional (m 0))
  (with-environment (claudia/command:to-l n m)))

(defun to-r (n &optional (m 0))
  (with-environment (claudia/command:to-r n m)))

(defun forall-l (n term &optional (m 0))
  (with-environment (claudia/command:forall-l n term m)))

(defun forall-r (n &optional (m 0))
  (with-environment (claudia/command:forall-r n m)))

(defun exists-l (n &optional (m 0))
  (with-environment (claudia/command:exists-l n m)))

(defun exists-r (n term &optional (m 0))
  (with-environment (claudia/command:exists-r n term m)))

(defun wl (n &optional (m 0))
  (with-environment (claudia/command:wl n m)))

(defun wr (n &optional (m 0))
  (with-environment (claudia/command:wr n m)))

(defun cl (n &optional (m 0))
  (with-environment (claudia/command:cl n m)))

(defun cr (n &optional (m 0))
  (with-environment (claudia/command:cr n m)))

(defun pl (n m l)
  (with-environment (claudia/command:pl n m l)))

(defun pr (n m l)
  (with-environment (claudia/command:pr n m l)))

;; api helper
(defun props ()
  (loop :for prop :in claudia/environment:props
        :do (format t "PROP: ~A: ~A~%" (car prop) (cdr prop))))

(defun vars ()
  (loop :for var :in claudia/environment:vars
        :do (format t "var: ~A: ~A~%" (car var) (cdr var))))

(defun proof-hist ()
  (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
    (loop :for step :in (reverse claudia/environment:history)
          :do (if (null (car step))
                  (format t "~16,,,'-A [GOAL]~%" "")
                  (format t "~16,,,'-A [~A]~%" "" (caar step)))
          :do (format t "~W~%" (cdr step)))))