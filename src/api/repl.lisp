(defpackage :claudia/api/repl
  (:use :cl)
  (:import-from :claudia/api/theorem)
  (:import-from :claudia/environment
                :history
                :current-goal
                :current-theorem
                :reset-claudia-environment)
  (:import-from :claudia/command)
  (:import-from :claudia/pattern/interface
                :rule)
  (:import-from :claudia/meta-data/interface
                :var :const :func
                :∧ :∨ :¬ :→ :∀ :∃ :predicate :prop
                :terms :formulas)
  (:import-from :claudia/sequent
                :sequent)
  (:import-from :claudia/goal
                :goal)
  (:import-from :claudia/print/interface
                :print-claudia-print-dispatch)
  (:export :start-proof :proof-hist :undo :export-proof
           :def-prop :def-var
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
           :props :vars
           ;; meta-data
           :∧ :∨ :¬ :→ :∀ :∃ :predicate
           :func :const
           :terms :formulas
           ;; pattern
           :rule
           ;; environment
           :reset-claudia-environment))
(in-package :claudia/api/repl)

(defmacro def-prop (sym)
  `(progn
     (defvar ,sym (prop ',sym))
     (push (cons ',sym ,sym) claudia/environment:props)
     (format t "PROP: ~A: ~A~%" ',sym ,sym)
     ,sym))

(defmacro def-var (sym)
  `(progn
     (defvar ,sym (var ',sym))
     (push (cons ',sym ,sym) claudia/environment:vars)
     (format t "VAR: ~A: ~A~%" ',sym ,sym)
     ,sym))

(defmacro start-proof (theorem &key (props nil) (vars nil))
  `(progn
     (reset-claudia-environment)
     ,@(mapcar (lambda (sym) (list 'def-prop sym)) props)
     ,@(mapcar (lambda (sym) (list 'def-var sym)) vars)
     (setf current-theorem ',theorem)
     (setf current-goal (goal (sequent nil (list (formulas ,theorem)))))
     (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
       (format t "~16,,,'-A [GOAL]~%" "")
       (format t "~W~%" current-goal)
       (push (cons nil current-goal) history))
     t))

(defmacro with-environment (command &rest args)
  ;; TODO error-handling
  `(let* ((arg-list (list ,@args)))
     (apply #',command arg-list)
     (push (cons (cons ',command arg-list) current-goal) history)
     t))

;; api comannd
(defun app-a (axiom &optional (n 0))
  (with-environment claudia/command:app-a axiom n))

(defun id (&optional (n 0))
  (with-environment claudia/command:id n))

(defun cut (formula &optional (n 0))
  (with-environment claudia/command:cut formula n))

(defun and-l (&optional (n 0) (m 0))
  (with-environment claudia/command:and-l n m))

(defun and-r (&optional (n 0) (m 0))
  (with-environment claudia/command:and-r n m))

(defun or-l (&optional (n 0) (m 0))
  (with-environment claudia/command:or-l n m))

(defun or-r (&optional (n 0) (m 0))
  (with-environment claudia/command:or-r n m))

(defun not-l (&optional (n 0) (m 0))
  (with-environment claudia/command:not-l n m))

(defun not-r (&optional (n 0) (m 0))
  (with-environment claudia/command:not-r n m))

(defun to-l (&optional (n 0) (m 0))
  (with-environment claudia/command:to-l n m))

(defun to-r (&optional (n 0) (m 0))
  (with-environment claudia/command:to-r n m))

(defun forall-l (term &optional (n 0) (m 0))
  (with-environment claudia/command:forall-l term n m))

(defun forall-r (var &optional (n 0) (m 0))
  (with-environment claudia/command:forall-r var n m))

(defun exists-l (var &optional (n 0) (m 0))
  (with-environment claudia/command:exists-l var n m))

(defun exists-r (term &optional (n 0) (m 0))
  (with-environment claudia/command:exists-r term n m))

(defun wl (&optional (n 0) (m 0))
  (with-environment claudia/command:wl n m))

(defun wr (&optional (n 0) (m 0))
  (with-environment claudia/command:wr n m))

(defun cl (&optional (n 0) (m 0))
  (with-environment claudia/command:cl n m))

(defun cr (&optional (n 0) (m 0))
  (with-environment claudia/command:cr n m))

(defun pl (&optional (n 0) (m 0) (l 1))
  (with-environment claudia/command:pl n m l))

(defun pr (&optional (n 0) (m 0) (l 1))
  (with-environment claudia/command:pr n m l))

(defun rewrite-l (rule &optional (n 0) (m 0))
  (with-environment claudia/command:rewrite-l rule n m))

(defun rewrite-r (rule &optional (n 0) (m 0))
  (with-environment claudia/command:rewrite-r rule n m))


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

(defun undo ()
  (cond ((cdr history)
         (setf history (cdr history))
         (setf current-goal (cdar history))
         (proof-hist))
        (t
         (format t "no history~%"))))

(defmacro export-proof (&optional (name (gensym "RANDOM-NAME-")) (package-name 'claudia/make-theorem))
  (let ((current-package-name (package-name *package*)))
    (unless (find-package package-name)
      (make-package package-name :use (list :cl :claudia/api/theorem)))
    `(progn
       (in-package ,package-name)
       (import ',(mapcar #'car claudia/environment:props))
       (import ',(mapcar #'car claudia/environment:vars))
       (format t "~S~%" `(claudia/api/theorem:def-theorem ,',name ,claudia/environment:current-theorem
                             (:props ,(mapcar #'car claudia/environment:props)
                              :vars ,(mapcar #'car claudia/environment:vars))
                           ,@(mapcar #'car (cdr (reverse claudia/environment:history)))))
       (in-package ,current-package-name)
       (ignore-errors (delete-package 'claudia/make-theorem)))))
