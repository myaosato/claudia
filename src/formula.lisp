(defpackage :logic/formula
  (:use :cl
        :logic/term)
  (:export :format-formula
           :∧ :is-∧ :∧-1 :∧-2
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
(defmethod format-formula ((formula formula))
  (error "format-formula method for type ~A is not defined" (type-of formula)))

(defclass ∧ (formula)
  ((∧-1 :initarg :∧-1 :reader ∧-1 :type formula)
   (∧-2 :initarg :∧-2 :reader ∧-2 :type formula)))
(defun ∧ (f1 f2)
  (make-instance '∧ :∧-1 f1 :∧-2 f2))
(defun is-∧ (formula)
  (eq (type-of formula) '∧))
(defmethod format-formula ((formula ∧))
  (format nil "(~A ∧ ~A)" (format-formula (∧-1 formula)) (format-formula (∧-2 formula))))

(defclass ∨ (formula)
  ((∨-1 :initarg :∨-1 :reader ∨-1 :type formula)
   (∨-2 :initarg :∨-2 :reader ∨-2 :type formula)))
(defun ∨ (f1 f2)
  (make-instance '∨ :∨-1 f1 :∨-2 f2))
(defun is-∨ (formula)
  (eq (type-of formula) '∨))
(defmethod format-formula ((formula ∨))
  (format nil "(~A ∨ ~A)" (format-formula (∨-1 formula)) (format-formula (∨-2 formula))))

(defclass ¬ (formula)
  ((¬-1 :initarg :¬-1 :reader ¬-1 :type formula)))
(defun ¬ (f)
  (make-instance '¬ :¬-1 f))
(defun is-¬ (formula)
  (eq (type-of formula) '¬))
(defmethod format-formula ((formula ¬))
  (format nil "¬~A" (format-formula (¬-1 formula))))

(defclass → (formula)
  ((→-1 :initarg :→-1 :reader →-1 :type formula)
   (→-2 :initarg :→-2 :reader →-2 :type formula)))
(defun → (f1 f2)
  (make-instance '→ :→-1 f1 :→-2 f2))
(defun is-→ (formula)
  (eq (type-of formula) '→))
(defmethod format-formula ((formula →))
  (format nil "(~A → ~A)" (format-formula (→-1 formula)) (format-formula (→-2 formula))))

(defclass atomic (formula) nil)

(defclass prop (atomic)
  ((name :initarg :name :reader name)))
(defun prop (name)
  (make-instance 'prop :name name))
(defun is-prop (formula)
  (eq (type-of formula) 'prop))
(defmethod format-formula ((formula prop))
  (format nil "~A" (name formula)))

(defclass predicate (atomic)
  ((name :initarg :name :reader name)
   (terms :initarg :terms :reader terms :type (vector term *))))
(defun predicate (name arity &rest terms)
  (make-instance 'predicate :name name :terms (coerce terms `(vector term ,arity))))
(defmacro def-predicate (name arity)
  `(defun ,name (&rest terms)
     (apply #'predicate ',name ,arity terms)))
(defmethod format-formula ((formula predicate))
  (format nil "~A(~{~A~^, ~})" (name formula) (mapcar #'format-term (coerce (terms formula) 'list))))
