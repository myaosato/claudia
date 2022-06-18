(defpackage :claudia/api
  (:use :cl
        :claudia/environment))
(in-package :claudia/api)

(defmacro with-environment (command)
  `(let ((new-goal ,command))
     (setf current-goal new-goal)
     (push ',command history)))

(defmacro start-proof ())