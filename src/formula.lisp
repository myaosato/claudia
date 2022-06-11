(defpackage :logic/formula
  (:use :cl
        :logic/term
        :logic/pprint)
  (:export :∧ :is-∧ :∧-1 :∧-2
           :∨ :is-∨ :∨-1 :∨-2
           :¬ :is-¬ :¬-1
           :→ :is-→ :→-1 :→-2
           :prop :is-prop
           :predicate :def-predicate))
(in-package :logic/formula)

;; ****************************************************************
;; meta data type
;;
;; const := const-val | func const*
;; term := const | var | func term*
;; atomic := prop | predicate term*
;; formula := formula ∧ formula | formula ∨ formula | ¬ formula | atomic
;;
;; ****************************************************************

(defgeneric format-formula (formula))

(defclass formula nil nil)
(defmethod print-object ((formula formula) stream)
  (declare (ignore stream))
  (error "print-object method for type ~A is not defined" (type-of formula)))
(defmacro def-print-formula ((formula class) control-string &rest format-arguments)
  (let ((stream (gensym "STREAM")))
    `(defmethod print-object ((,formula ,class) ,stream)
       (format ,stream ,control-string ,@format-arguments))))
(defmethod pprint-formula ((formula formula) stream)
  (declare (ignore stream))
  (error "pprint-formula method for type ~A is not defined" (type-of formula)))
(def-logic-print ('formula stream formula)
  (pprint-formula formula stream))

(defclass ∧ (formula)
  ((∧-1 :initarg :∧-1 :reader ∧-1 :type formula)
   (∧-2 :initarg :∧-2 :reader ∧-2 :type formula)))
(defun ∧ (f1 f2)
  (make-instance '∧ :∧-1 f1 :∧-2 f2))
(defun is-∧ (formula)
  (eq (type-of formula) '∧))
(def-print-formula (formula ∧) "(~A ~A ~A)" '∧ (∧-1 formula) (∧-2 formula))
(defmethod pprint-formula ((formula ∧) stream)
  (format stream "(~A ∧ ~A)" (pprint-formula (∧-1 formula) nil) (pprint-formula (∧-2 formula) nil)))

(defclass ∨ (formula)
  ((∨-1 :initarg :∨-1 :reader ∨-1 :type formula)
   (∨-2 :initarg :∨-2 :reader ∨-2 :type formula)))
(defun ∨ (f1 f2)
  (make-instance '∨ :∨-1 f1 :∨-2 f2))
(defun is-∨ (formula)
  (eq (type-of formula) '∨))
(def-print-formula (formula ∨) "(~A ~A ~A)" '∨ (∨-1 formula) (∨-2 formula))
(defmethod pprint-formula ((formula ∨) stream)
  (format stream "(~A ∨ ~A)" (pprint-formula (∨-1 formula) nil) (pprint-formula (∨-2 formula) nil)))

(defclass ¬ (formula)
  ((¬-1 :initarg :¬-1 :reader ¬-1 :type formula)))
(defun ¬ (f)
  (make-instance '¬ :¬-1 f))
(defun is-¬ (formula)
  (eq (type-of formula) '¬))
(def-print-formula (formula ¬) "(~A ~A)" '¬ (¬-1 formula))
(defmethod pprint-formula ((formula ¬) stream)
  (format stream "¬~A" (pprint-formula (¬-1 formula) nil)))

(defclass → (formula)
  ((→-1 :initarg :→-1 :reader →-1 :type formula)
   (→-2 :initarg :→-2 :reader →-2 :type formula)))
(defun → (f1 f2)
  (make-instance '→ :→-1 f1 :→-2 f2))
(defun is-→ (formula)
  (eq (type-of formula) '→))
(def-print-formula (formula →) "(~A ~A ~A)" '→ (→-1 formula) (→-2 formula))
(defmethod pprint-formula ((formula →) stream)
  (format stream "(~A → ~A)" (pprint-formula (→-1 formula) nil) (pprint-formula (→-2 formula) nil)))

(defclass atomic (formula) nil)

(defclass prop (atomic)
  ((name :initarg :name :reader name)))
(defun prop (name)
  (make-instance 'prop :name name))
(defun is-prop (formula)
  (eq (type-of formula) 'prop))
(def-print-formula (formula prop) "#<Prop: ~A>" (name formula))
(defmethod pprint-formula ((formula prop) stream)
  (format stream "~A" (name formula)))

(defclass predicate (atomic)
  ((name :initarg :name :reader name)
   (terms :initarg :terms :reader terms :type (vector term *))))
(defun predicate (name arity &rest terms)
  (declare (type symbol name))
  (make-instance 'predicate :name name :terms (coerce terms `(vector term ,arity))))
(defmacro def-predicate (name arity)
  `(defun ,name (&rest terms)
     (apply #'predicate ',name ,arity terms)))
(def-print-formula (formula predicate) "(~A ~{~A~^ ~})" (name formula) (coerce (terms formula) 'list))
(defmethod pprint-formula ((formula predicate) stream)
  (format stream "~A(~{~:W~^ ~})" (name formula) (coerce (terms formula) 'list)))
