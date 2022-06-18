(defpackage :claudia/api
  (:use :cl)
  (:import-from :claudia/theorem)
  (:import-from :claudia/environment
                :history
                :current-goal
                :current-theorem
                :reset-claudia-environment)
  (:import-from :claudia/command)
  (:import-from :claudia/term
                :var :def-func :const)
  (:import-from :claudia/formula
                :∧ :∨ :¬ :→ :∀ :∃ :def-predicate
                :prop)
  (:import-from :claudia/sequent
                :sequent)
  (:import-from :claudia/goal
                :goal)
  (:import-from :claudia/pprint
                :print-claudia-print-dispatch)
  (:export :start-proof :proof-hist :undo :export-proof
           :def-prop :def-var
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
     (setf current-goal (goal (sequent nil (list ,theorem))))
     (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
       (format t "~16,,,'-A [GOAL]~%" "")
       (format t "~W~%" current-goal)
       (push (cons nil current-goal) history))
     t))

(defmacro with-environment (command &rest args)
  ;; TODO error-handling
  `(let* ((arg-list (list ,@args))
          (new-goal (apply #',command arg-list)))
     (setf current-goal new-goal)
     (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
       (format t "~16,,,'-A [~A]~%" "" ',command)
       (format t "~W~%" current-goal)
       (push (cons (cons ',command arg-list) current-goal) history))
     t))

;; api comannd
(defun id (n)
  (with-environment claudia/command:id n))

(defun cut (n formula)
  (with-environment claudia/command:cut n formula))

(defun and-l1 (n &optional (m 0))
  (with-environment claudia/command:and-l1 n m))

(defun and-l2 (n &optional (m 0))
  (with-environment claudia/command:and-l2 n m))

(defun and-r (n &optional (m 0))
  (with-environment claudia/command:and-r n m))

(defun or-l (n &optional (m 0))
  (with-environment claudia/command:or-l n m))

(defun or-r1 (n &optional (m 0))
  (with-environment claudia/command:or-r1 n m))

(defun or-r2 (n &optional (m 0))
  (with-environment claudia/command:or-r2 n m))

(defun not-l (n &optional (m 0))
  (with-environment claudia/command:not-l n m))

(defun not-r (n &optional (m 0))
  (with-environment claudia/command:not-r n m))

(defun to-l (n &optional (m 0))
  (with-environment claudia/command:to-l n m))

(defun to-r (n &optional (m 0))
  (with-environment claudia/command:to-r n m))

(defun forall-l (n term &optional (m 0))
  (with-environment claudia/command:forall-l n term m))

(defun forall-r (n &optional (m 0))
  (with-environment claudia/command:forall-r n m))

(defun exists-l (n &optional (m 0))
  (with-environment claudia/command:exists-l n m))

(defun exists-r (n term &optional (m 0))
  (with-environment claudia/command:exists-r n term m))

(defun wl (n &optional (m 0))
  (with-environment claudia/command:wl n m))

(defun wr (n &optional (m 0))
  (with-environment claudia/command:wr n m))

(defun cl (n &optional (m 0))
  (with-environment claudia/command:cl n m))

(defun cr (n &optional (m 0))
  (with-environment claudia/command:cr n m))

(defun pl (n m l)
  (with-environment claudia/command:pl n m l))

(defun pr (n m l)
  (with-environment claudia/command:pr n m l))

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
      (make-package package-name :use (list :cl :claudia/theorem)))
    `(progn
       (in-package ,package-name)
       (import ',(mapcar #'car claudia/environment:props))
       (import ',(mapcar #'car claudia/environment:vars))
       (format t "~S~%" `(claudia/theorem:def-theorem ,',name ,claudia/environment:current-theorem
                             (:props ,(mapcar #'car claudia/environment:props)
                              :vars ,(mapcar #'car claudia/environment:vars))
                           ,@(mapcar #'car (cdr (reverse claudia/environment:history)))))
       (in-package ,current-package-name)
       (ignore-errors (delete-package 'claudia/make-theorem)))))
