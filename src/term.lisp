(defpackage :logic/term
  (:use :cl)
  (:export :term :var :const
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
(defmethod print-object ((term term) stream)
  (declare (ignore stream))
  (error "print-object method for type ~A is not defined" (type-of term)))
(defmacro def-print-term ((term class) control-string &rest format-arguments)
  (let ((stream (gensym "STREAM")))
    `(defmethod print-object ((,term ,class) ,stream)
       (format ,stream ,control-string ,@format-arguments))))

(defclass var (term)
  ((name :initarg :name :reader name)))
(defun var (name)
  (make-instance 'var :name name))
(def-print-term (term var) "#<Var: ~A>" (name term))

(defclass const-val (term)
  ((name :initarg :name :reader name)))
(defun const (name)
  (make-instance 'const-val :name name))
(def-print-term (term  const-val) "#<Const: ~A>" (name term))

(defclass func (term)
  ((name :initarg :name :reader name)
   (terms :initarg :terms :reader terms :type (vector term *))))
(defun func (name arity &rest terms)
  (declare (type symbol name))
  (make-instance 'func :name name :terms (coerce terms `(vector term ,arity))))
(defmacro def-func (name arity)
  `(defun ,name (&rest terms)
     (apply #'func ',name ,arity terms)))
(def-print-term (term func) "(~A ~{~A~^ ~})" (name term) (coerce (terms term) 'list))
(defun func-const*-p (thing)
  (and (typep thing 'func)
       (typep (terms thing) '(simple-array const (*)))))

(deftype const ()
  `(or const-val
       (satisfies func-const*-p)))
