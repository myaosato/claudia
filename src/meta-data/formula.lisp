(defpackage :claudia/meta-data/formula
  (:use :cl
        :claudia/meta-data/meta-data
        :claudia/meta-data/term
        :claudia/pprint)
  (:export :formula :formula-list
           :∧ :∧-1 :∧-2
           :∨ :∨-1 :∨-2
           :¬ :¬-1
           :→ :→-1 :→-2
           :∀ :∀-var :∀-formula
           :∃ :∃-var :∃-formula
           :prop :predicate))
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
  ((∧-1 :initarg :∧-1 :reader ∧-1 :type formula)
   (∧-2 :initarg :∧-2 :reader ∧-2 :type formula)))
(defun ∧ (f1 f2)
  (make-instance '∧ :∧-1 f1 :∧-2 f2))
(defmethod initialize-instance :after ((formula ∧) &key)
  (setf (%free-vars formula) (union (free-vars (∧-1 formula)) (free-vars (∧-2 formula)))))
(defmethod <- ((place ∧) (var var) (term term))
  (∧ (<- (∧-1 place) var term) (<- (∧-2 place) var term)))
(defmethod <-able ((place ∧) (var var) (term term))
  (and (<-able (∧-1 place) var term) (<-able (∧-2 place) var term)))
(defmethod == ((a ∧) (b meta-data))
  (and (typep b '∧)
       (== (∧-1 a) (∧-1 b))
       (== (∧-2 a) (∧-2 b))))

;; ∨
(defclass ∨ (formula)
  ((∨-1 :initarg :∨-1 :reader ∨-1 :type formula)
   (∨-2 :initarg :∨-2 :reader ∨-2 :type formula)))
(defun ∨ (f1 f2)
  (make-instance '∨ :∨-1 f1 :∨-2 f2))
(defmethod initialize-instance :after ((formula ∨) &key)
  (setf (%free-vars formula) (union (free-vars (∨-1 formula)) (free-vars (∨-2 formula)))))
(defmethod <- ((place ∨) (var var) (term term))
  (∨ (<- (∨-1 place) var term) (<- (∨-2 place) var term)))
(defmethod <-able ((place ∨) (var var) (term term))
  (and (<-able (∨-1 place) var term) (<-able (∨-2 place) var term)))
(defmethod == ((a ∨) (b meta-data))
  (and (typep b '∨)
       (== (∨-1 a) (∨-1 b))
       (== (∨-2 a) (∨-2 b))))

;; ¬
(defclass ¬ (formula)
  ((¬-1 :initarg :¬-1 :reader ¬-1 :type formula)))
(defun ¬ (f)
  (make-instance '¬ :¬-1 f))
(defmethod initialize-instance :after ((formula ¬) &key)
  (setf (%free-vars formula) (free-vars (¬-1 formula))))
(defmethod <- ((place ¬) (var var) (term term))
  (¬ (<- (¬-1 place) var term)))
(defmethod <-able ((place ¬) (var var) (term term))
  (<-able (¬-1 place) var term))
(defmethod == ((a ¬) (b meta-data))
  (and (typep b '¬)
       (== (¬-1 a) (¬-1 b))))

;; →
(defclass → (formula)
  ((→-1 :initarg :→-1 :reader →-1 :type formula)
   (→-2 :initarg :→-2 :reader →-2 :type formula)))
(defun → (f1 f2)
  (make-instance '→ :→-1 f1 :→-2 f2))
(defmethod initialize-instance :after ((formula →) &key)
  (setf (%free-vars formula) (union (free-vars (→-1 formula)) (free-vars (→-2 formula)))))
(defmethod <- ((place →) (var var) (term term))
  (→ (<- (→-1 place) var term) (<- (→-2 place) var term)))
(defmethod <-able ((place →) (var var) (term term))
  (and (<-able (→-1 place) var term) (<-able (→-2 place) var term)))
(defmethod == ((a →) (b meta-data))
  (and (typep b '→)
       (== (→-1 a) (→-1 b))
       (== (→-2 a) (→-2 b))))

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
  ((name :initarg :name :reader name)))
(defun prop (name)
  (make-instance 'prop :name name))
(defmethod <- ((place prop) (var var) (term term))
  place)
(defmethod == ((a prop) (b meta-data))
  (and (typep b 'prop)
       (eq a b)))

;; predicate
(defclass predicate (atomic)
  ((name :initarg :name :reader name)
   (terms :initarg :terms :reader terms :type term-list)))
(defmethod initialize-instance :after ((formula predicate) &key)
  (setf (%free-vars formula) (reduce #'union
                                     (mapcar #'free-vars (terms formula))
                                     :initial-value (free-vars (name formula)))))
(defun predicate (name &rest terms)
  (make-instance 'predicate :name name :terms terms))
(defmethod <- ((place predicate) (var var) (term term))
  (apply #'predicate
         (<- (name place) var term)
         (mapcar (lambda (x) (<- x var term)) (terms place))))
(defmethod == ((a predicate) (b meta-data))
  (and (typep b 'predicate)
       (== (name a) (name b))
       (every #'== (terms a) (terms b))))



;; ------------------------
(defmethod print-object ((formula formula) stream)
  (declare (ignore stream))
  (error "print-object method for type ~A is not defined" (type-of formula)))
(defmethod pprint-formula ((formula formula) stream)
  (declare (ignore stream))
  (error "pprint-formula method for type ~A is not defined" (type-of formula)))
(def-claudia-print (formula) (formula stream)
  (pprint-formula formula stream))

(defmethod print-object ((formula ∧) stream)
  (format stream "(~A ~A ~A)" '∧ (∧-1 formula) (∧-2 formula)))
(defmethod pprint-formula ((formula ∧) stream)
  (format stream "(~:W ∧ ~:W)" (∧-1 formula) (∧-2 formula)))

(defmethod print-object ((formula ∨) stream)
  (format stream "(~A ~A ~A)" '∨ (∨-1 formula) (∨-2 formula)))
(defmethod pprint-formula ((formula ∨) stream)
  (format stream "(~:W ∨ ~:W)" (∨-1 formula) (∨-2 formula)))

(defmethod print-object ((formula ¬) stream)
  (format stream "(~A ~A)" '¬ (¬-1 formula)))
(defmethod pprint-formula ((formula ¬) stream)
  (format stream "¬~:W" (¬-1 formula)))

(defmethod print-object ((formula →) stream)
  (format stream "(~A ~A ~A)" '→ (→-1 formula) (→-2 formula)))
(defmethod pprint-formula ((formula →) stream)
  (format stream "(~:W → ~:W)" (→-1 formula) (→-2 formula)))

(defmethod print-object ((formula ∀) stream)
  (format stream "(~A ~A ~A)" '∀ (∀-var formula) (∀-formula formula)))
(defmethod pprint-formula ((formula ∀) stream)
  (format stream "∀~:W(~:W)" (∀-var formula) (∀-formula formula)))

(defmethod print-object ((formula ∃) stream)
  (format stream "(~A ~A ~A)" '∃ (∃-var formula) (∃-formula formula)))
(defmethod pprint-formula ((formula ∃) stream)
  (format stream "∃~:W(~:W)" (∃-var formula) (∃-formula formula)))

(defmethod print-object ((formula prop) stream)
  (format stream "#<Prop: ~A>" (name formula)))
(defmethod pprint-formula ((formula prop) stream)
  (format stream "~A" (name formula)))

(defmethod print-object ((formula predicate) stream)
  (format stream "(~A ~A ~{~A~^ ~})" 'predicate (name formula) (terms formula)))
(defmethod pprint-formula ((formula predicate) stream)
  (if (= (length (terms formula)) 2) ;; ?
      (format stream "(~:W ~:W ~:W)" (nth 0 (terms formula)) (name formula) (nth 1 (terms formula)))
      (format stream "~:W(~{~:W~^ ~})" (name formula) (terms formula))))
