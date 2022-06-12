(defpackage :claudia/term
  (:use :cl
        :claudia/pprint)
  (:shadow :substitute)
  (:export :term :free-vars :var :const :constructor
           :func :def-func
           :substitute :substitutable))
(in-package :claudia/term)

;; ****************************************************************
;; meta data type
;;
;; const := const-val | func const*
;; term := const | var | func term*
;;
;; ****************************************************************
(defclass term nil
  ((free-vars :initarg :free-vars :initform nil :accessor %free-vars :reader free-vars)))
(defmethod print-object ((term term) stream)
  (declare (ignore stream))
  (error "print-object method for type ~A is not defined" (type-of term)))
(defmacro def-print-term ((term class) control-string &rest format-arguments)
  (let ((stream (gensym "STREAM")))
    `(defmethod print-object ((,term ,class) ,stream)
       (format ,stream ,control-string ,@format-arguments))))
(defmethod pprint-term ((term term) stream)
  (declare (ignore stream))
  (error "pprint-term method for type ~A is not defined" (type-of term)))
(def-claudia-print (term) (term stream)
  (pprint-term term stream))
(defmethod substitute ((place term) var term)
  (error "substitute method for type ~A is not defined" (type-of place)))
(defmethod substitutable ((place term) var term)
  (error "substitutable method for type ~A is not defined" (type-of place)))

;; var
(defclass var (term)
  ((name :initarg :name :reader name)))
(defmethod initialize-instance :after ((term var) &key)
  (setf (%free-vars term) (list term)))
(defun var (name)
  (make-instance 'var :name name))
(def-print-term (term var) "#<Var: ~A>" (name term))
(defmethod pprint-term ((term var) stream)
  (format stream "~A" (name term)))
(defmethod substitute ((place var) (var var) (term term))
  (if (eq place var)
      term
      place))

(defmethod substitutable ((place term) (var var) (term term))
  t)

;; const-val
(defclass const-val (term)
  ((name :initarg :name :reader name)))
(defun const (name)
  (make-instance 'const-val :name name))
(def-print-term (term  const-val) "#<Const: ~A>" (name term))
(defmethod pprint-term ((term const-val) stream)
  (format stream "~A" (name term)))
(defmethod substitute ((place const-val) (var var) (term term))
  place)

;; func
(defclass func (term)
  ((name :initarg :name :reader name)
   (terms :initarg :terms :reader terms :type (vector term *))
   (constructor :initarg :constructor :reader constructor)))
(defmethod initialize-instance :after ((term func) &key)
  (setf (%free-vars term) (reduce #'union (mapcar #'free-vars (coerce (terms term) 'list)))))
(defmacro def-func (name arity)
  (declare (type symbol name))
  `(labels ((%c (name constructor &rest terms)
              (make-instance 'func
                             :name name
                             :terms (coerce terms '(vector term ,arity))
                             :constructor constructor))
            (c (&rest terms) (apply #'%c ',name #'c terms)))
     (defun ,name (&rest terms) (apply #'%c ',name #'c terms))))
(def-print-term (term func) "(~A ~{~A~^ ~})" (name term) (coerce (terms term) 'list))
(defmethod pprint-term ((term func) stream)
  (if (= (length (terms term)) 2)
      (format stream "(~:W ~A ~:W)"
              (aref (terms term) 0) (name term) (aref (terms term) 1))
      (format stream "~A(~{~:W~^, ~})" (name term) (coerce (terms term) 'list))))
(defmethod substitute ((place func) (var var) (term term))
  (apply (constructor place)
         (mapcar (lambda (x) (substitute x var term)) (coerce (terms place) 'list))))

;; const (meta type)
(defun func-const*-p (thing)
  (and (typep thing 'func)
       (typep (terms thing) '(simple-array const (*)))))
(deftype const ()
  `(or const-val
       (satisfies func-const*-p)))

(defmacro with-terms (term-spec-list &body body)
  `(let (,@(mapcar (lambda (term-spec)
                     (list (car term-spec)
                           (cond ((eq (cadr term-spec) :var)
                                  (var (car term-spec)))
                                 ((eq (cadr term-spec) :const)
                                  (const (car term-spec)))
                                 (t (error "invalid term-spec, term-spec := (list symbol (or :const :var))")))))
                   term-spec-list))
     ,@body))
  