(defpackage :claudia/environment
  (:use :cl
        :claudia/goal
        :claudia/sequent)
  (:export :reset
           :with-current-goal
           :current-goal
           :history))
(in-package :claudia/environment)

(defparameter void (goal (sequent nil nil)))
(defvar current-goal void)
(defvar history nil)

(defun reset ()
  (setf history nil)
  (setf current-goal void))

(defmacro with-current-goal (goal &body body)
  `(let ((current-goal ,goal))
     ,@body))
