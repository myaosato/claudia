(defpackage :claudia/term
  (:use :cl
        :claudia/pprint)
  (:shadow :substitute)
  (:export :term :free-vars :term-=
           :var :const :func           
           :term-list))
(in-package :claudia/term)

;; ****************************************************************
;; meta data type
;;
;; func := name terms*
;; term := const | var | func
;;
;; ****************************************************************
(defclass term nil
  ((free-vars :initarg :free-vars :initform nil :accessor %free-vars :reader free-vars)))
(defmethod print-object ((term term) stream)
  (declare (ignore stream))
  (error "print-object method for type ~A is not defined" (type-of term)))
(defmethod pprint-term ((term term) stream)
  (declare (ignore stream))
  (error "pprint-term method for type ~A is not defined" (type-of term)))
(def-claudia-print (term) (term stream)
  (pprint-term term stream))
(defmethod substitute ((place term) var term)
  (error "substitute method for type ~A is not defined" (type-of place)))
(defmethod substitutable ((place term) var term)
  (error "substitutable method for type ~A is not defined" (type-of place)))
(defmethod term-= ((a term) (b term))
  (error "term-= method for type ~A is not defined" (type-of a)))
(defun term-list-p (thing)
  (and (listp thing)
       (every (lambda (x) (typep x 'term)) thing)))
(deftype term-list ()
  `(satisfies term-list-p))

;; var
(defclass var (term)
  ((name :initarg :name :reader name)))
(defmethod initialize-instance :after ((term var) &key)
  (setf (%free-vars term) (list term)))
(defun var (name)
  (make-instance 'var :name name))
(defmethod print-object ((term var) stream)
  (format stream "~A" (name term)))
(defmethod pprint-term ((term var) stream)
  (format stream "~A" (name term)))
(defmethod substitute ((place var) (var var) (term term))
  (if (eq place var)
      term
      place))
(defmethod substitutable ((place term) (var var) (term term))
  t)
(defmethod term-= ((a var) (b term))
  (and (typep b 'var)
       (eq a b)))

;; const
(defclass const (term)
  ((name :initarg :name :reader name)))
(defun const (name)
  (make-instance 'const :name name))
(defmethod print-object ((term  const) stream)
    (format stream "~A" (name term)))
(defmethod pprint-term ((term const) stream)
  (format stream "~A" (name term)))
(defmethod substitute ((place const) (var var) (term term))
  place)
(defmethod term-= ((a const) (b term))
  (and (typep b 'const)
       (eq a b)))

;; func
(defclass func (term)
  ((name :initarg :name :reader name)
   (terms :initarg :terms :reader terms :type term-list)))
(defun func (name &rest terms)
  (make-instance 'func :name name :terms terms))
(defmethod initialize-instance :after ((term func) &key)
  (setf (%free-vars term) (reduce #'union (mapcar #'free-vars (terms term))
                                  :initial-value (free-vars (name term)))))
(defmethod print-object ((term func) stream)
  (format stream "(~A ~A ~{~A~^ ~})" 'func (name term) (terms term)))
(defmethod pprint-term ((term func) stream)
  (format stream "~W(~{~:W~^, ~})" (name term) (terms term)))
(defmethod substitute ((place func) (var var) (term term))
  (apply #'func
         (substitute (name place) var term)
         (mapcar (lambda (x) (substitute x var term)) (terms place))))
(defmethod term-= ((a func) (b term))
  (and (typep b 'func)
       (term-= (name a) (name b))
       (every #'term-= (terms a) (terms b))))
