(defpackage :claudia/environment
  (:use :cl
        :claudia/goal
        :claudia/sequent)
  (:export :start
           :with-current-goal
           :current-goal))
(in-package :claudia/environment)

(defparameter void (goal (sequent nil nil)))
(defvar current-goal void)

(defun start ()
  (setf current-goal void))

(defmacro with-current-goal (goal &body body)
  `(let ((current-goal ,goal))
     ,@body))
