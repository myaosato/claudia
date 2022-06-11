(defpackage :logic/term
  (:use :cl)
  (:export :format-term
           :term :var :const
           :func :def-func))
(in-package :logic/term)

;; ****************************************************************
;; meta data type
;;
;; const := const-val | func const*
;; term := const | var | func term*
;;
;; ****************************************************************

(defclass term nil nil)
(defmethod format-term ((term term))
  (error "format-term method for type ~A is not defined" (type-of term)))

(defclass var (term)
  ((name :initarg :name :reader name)))
(defun var (name)
  (make-instance 'var :name name))
(defmethod format-term ((term var))
  (format nil "~A" (name term)))

(defclass const-val (term)
  ((name :initarg :name :reader const-name)))
(defun const (name)
  (make-instance 'const-val :name name))
(defmethod format-term ((term  const-val))
  (format nil "~A" (name term)))

(defclass func (term)
  ((name :initarg :name :reader name)
   (terms :initarg :terms :reader terms :type (vector term *))))
(defun func (name arity &rest terms)
  (make-instance 'func :name name :terms (coerce terms `(vector term ,arity))))
(defmacro def-func (name arity)
  `(defun ,name (&rest terms)
     (apply #'func ',name ,arity terms)))
(defmethod format-term ((term func))
  (format nil "~A(~{~A~^, ~})" (name term) (mapcar #'format-term (coerce (terms term) 'list))))
(defun func-const*-p (thing)
  (and (typep thing 'func)
       (typep (terms thing) '(simple-array const (*)))))

(deftype const ()
  `(or const-val
       (satisfies func-const*-p)))
       

        
