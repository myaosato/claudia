(defpackage :logic/formula
  (:use :cl)
  (:export :∧ :is-∧ :∧-1 :∧-2
           :∨ :is-∨ :∨-1 :∨-2
           :¬ :is-¬ :¬-1
           :→ :is-→ :→-1 :→-2
           :prop :is-prop :prop-name
           :format-formula))
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

(defclass atomic nil nil)

(defclass prop (atomic)
  ((name :initarg :name :reader prop-name)))
(defun prop (name)
  (make-instance 'prop :name name))
(defun is-prop (formula)
  (eq (type-of formula) 'prop))
(defmethod format-formula ((formula prop))
  (format nil "~A" (prop-name formula)))
