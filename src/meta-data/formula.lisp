(defpackage :claudia/meta-data/formula
  (:use :cl
        :claudia/meta-data/meta-data
        :claudia/meta-data/term)
  (:export :formula :formula-list
           :∧ :∧-0 :∧-1
           :∨ :∨-0 :∨-1
           :¬ :¬-0
           :→ :→-0 :→-1
           :∀ :∀-var :∀-formula
           :∃ :∃-var :∃-formula
           :prop :prop-name
           :predicate :predicate-terms))
(in-package :claudia/meta-data/formula)

;; ****************************************************************
;; meta data type
;;
;; const := const-val | func const*
;; term := const | var | func term*
;; atomic := prop | predicate term*
;; formula := formula ∧ formula | formula ∨ formula | ¬ formula | atomic
;;
;; ****************************************************************
(defclass formula (meta-data)
  nil)
(defun formula-list-p (thing)
  (and (listp thing)
       (every (lambda (x) (typep x 'formula)) thing)))
(deftype formula-list ()
  `(satisfies formula-list-p))

;; ∧
(defclass ∧ (formula)
  ((∧-0 :initarg :∧-0 :reader ∧-0 :type formula)
   (∧-1 :initarg :∧-1 :reader ∧-1 :type formula)))
(defun ∧ (f1 f2)
  (make-instance '∧ :∧-0 f1 :∧-1 f2))
(defmethod initialize-instance :after ((formula ∧) &key)
  (setf (%free-vars formula) (union (free-vars (∧-0 formula)) (free-vars (∧-1 formula)))))
(defmethod <- ((place ∧) (var var) (term term))
  (∧ (<- (∧-0 place) var term) (<- (∧-1 place) var term)))
(defmethod <-able ((place ∧) (var var) (term term))
  (and (<-able (∧-0 place) var term) (<-able (∧-1 place) var term)))
(defmethod == ((a ∧) (b meta-data))
  (and (typep b '∧)
       (== (∧-0 a) (∧-0 b))
       (== (∧-1 a) (∧-1 b))))

;; ∨
(defclass ∨ (formula)
  ((∨-0 :initarg :∨-0 :reader ∨-0 :type formula)
   (∨-1 :initarg :∨-1 :reader ∨-1 :type formula)))
(defun ∨ (f1 f2)
  (make-instance '∨ :∨-0 f1 :∨-1 f2))
(defmethod initialize-instance :after ((formula ∨) &key)
  (setf (%free-vars formula) (union (free-vars (∨-0 formula)) (free-vars (∨-1 formula)))))
(defmethod <- ((place ∨) (var var) (term term))
  (∨ (<- (∨-0 place) var term) (<- (∨-1 place) var term)))
(defmethod <-able ((place ∨) (var var) (term term))
  (and (<-able (∨-0 place) var term) (<-able (∨-1 place) var term)))
(defmethod == ((a ∨) (b meta-data))
  (and (typep b '∨)
       (== (∨-0 a) (∨-0 b))
       (== (∨-1 a) (∨-1 b))))

;; ¬
(defclass ¬ (formula)
  ((¬-0 :initarg :¬-0 :reader ¬-0 :type formula)))
(defun ¬ (f)
  (make-instance '¬ :¬-0 f))
(defmethod initialize-instance :after ((formula ¬) &key)
  (setf (%free-vars formula) (free-vars (¬-0 formula))))
(defmethod <- ((place ¬) (var var) (term term))
  (¬ (<- (¬-0 place) var term)))
(defmethod <-able ((place ¬) (var var) (term term))
  (<-able (¬-0 place) var term))
(defmethod == ((a ¬) (b meta-data))
  (and (typep b '¬)
       (== (¬-0 a) (¬-0 b))))

;; →
(defclass → (formula)
  ((→-0 :initarg :→-0 :reader →-0 :type formula)
   (→-1 :initarg :→-1 :reader →-1 :type formula)))
(defun → (f1 f2)
  (make-instance '→ :→-0 f1 :→-1 f2))
(defmethod initialize-instance :after ((formula →) &key)
  (setf (%free-vars formula) (union (free-vars (→-0 formula)) (free-vars (→-1 formula)))))
(defmethod <- ((place →) (var var) (term term))
  (→ (<- (→-0 place) var term) (<- (→-1 place) var term)))
(defmethod <-able ((place →) (var var) (term term))
  (and (<-able (→-0 place) var term) (<-able (→-1 place) var term)))
(defmethod == ((a →) (b meta-data))
  (and (typep b '→)
       (== (→-0 a) (→-0 b))
       (== (→-1 a) (→-1 b))))

;; ∀
(defclass ∀ (formula)
  ((∀-var :initarg :var :reader ∀-var :type var)
   (∀-formula :initarg :formula :reader ∀-formula :type formula)))
(defun ∀ (var formula)
  (make-instance '∀ :var var :formula formula))
(defmethod initialize-instance :after ((formula ∀) &key)
  (setf (%free-vars formula) (remove (∀-var formula) (free-vars (∀-formula formula)) :test #'eq)))
(defmethod <- ((place ∀) (var var) (term term))
  (if (eq (∀-var place) var)
      place
      (∀ (∀-var place) (<- (∀-formula place) var term))))
(defmethod <-able ((place ∀) (var var) (term term))
  (or (not (== (∀-var place) var))
      (not (free-p var (∀-formula place)))
      (and (<-able (∀-formula place) var term)
           (not (free-p (∀-var place) term)))))
(defmethod == ((a ∀) (b meta-data))
  (and (typep b '∀)
       (== (∀-var a) (∀-var b))
       (== (∀-formula a) (∀-formula b))))

;; ∃
(defclass ∃ (formula)
  ((∃-var :initarg :var :reader ∃-var :type var)
   (∃-formula :initarg :formula :reader ∃-formula :type formula)))
(defun ∃ (var formula)
  (make-instance '∃ :var var :formula formula))
(defmethod initialize-instance :after ((formula ∃) &key)
  (setf (%free-vars formula) (remove (∃-var formula) (free-vars (∃-formula formula)) :test #'eq)))
(defmethod <- ((place ∃) (var var) (term term))
  (if (eq (∃-var place) var)
      place
      (∃ (∃-var place) (<- (∃-formula place) var term))))
(defmethod <-able ((place ∃) (var var) (term term))
  (or (not (== (∃-var place) var))
      (not (free-p var (∃-formula place)))
      (and (<-able (∃-formula place) var term)
           (not (free-p (∃-var place) term)))))
(defmethod == ((a ∃) (b meta-data))
  (and (typep b '∃)
       (== (∃-var a) (∃-var b))
       (== (∃-formula a) (∃-formula b))))

;;;; atomic
(defclass atomic (formula) nil)
(defmethod <-able ((place atomic) (var var) (term term))
  t)

;; prop
(defclass prop (atomic)
  ((name :initarg :name :reader prop-name)))
(defun prop (name)
  (make-instance 'prop :name name))
(defmethod <- ((place prop) (var var) (term term))
  place)
(defmethod == ((a prop) (b meta-data))
  (and (typep b 'prop)
       (eq (prop-name a) (prop-name b))))

;; predicate
(defclass predicate (atomic)
  ((terms :initarg :terms :reader predicate-terms :type term-list)))
(defmethod initialize-instance :after ((formula predicate) &key)
  (setf (%free-vars formula) (reduce #'union
                                     (mapcar #'free-vars (predicate-terms formula)))))
(defun predicate (&rest terms)
  (make-instance 'predicate :terms terms))
(defmethod <- ((place predicate) (var var) (term term))
  (apply #'predicate
         (mapcar (lambda (x) (<- x var term)) (predicate-terms place))))
(defmethod == ((a predicate) (b meta-data))
  (and (typep b 'predicate)
       (every #'== (predicate-terms a) (predicate-terms b))))
