(defpackage :claudia/meta-data/meta-data
  (:use :cl)
  (:export :meta-data
           :%free-vars :free-vars :free-p
           :<- :<-able
           :==))
(in-package :claudia/meta-data/meta-data)

(defclass meta-data nil
  ((free-vars :initarg :free-vars :initform nil :accessor %free-vars :reader free-vars)))
(defmethod <- ((place meta-data) var term)
  (error "<- method for type ~A is not defined" (type-of place)))
(defmethod <-able ((place meta-data) var term)
  (error "<-able method for type ~A is not defined" (type-of place)))
(defmethod == ((a meta-data) (b meta-data))
  (error "meta-data-= method for type ~A is not defined" (type-of a)))

(defun free-p (var &optional x)
  (if (typep x 'meta-data)
      (find var (free-vars x) :test #'==)
      (lambda (f) (free-p var f))))
