(defpackage :claudia/environment
  (:use :cl
        :claudia/goal
        :claudia/sequent)
  (:export :reset-claudia-environment
           :with-current-goal
           :current-goal
           :history
           :props
           :vars))
(in-package :claudia/environment)

(defparameter void (goal (sequent nil nil)))
(defvar current-goal void)
(defvar history nil)
(defvar props nil)
(defvar vars nil)

(defun reset-claudia-environment ()
  (setf history nil)
  (setf props nil)
  (setf vars nil)
  (setf current-goal void))

(defmacro with-current-goal (goal &body body)
  `(let ((current-goal ,goal))
     ,@body))
