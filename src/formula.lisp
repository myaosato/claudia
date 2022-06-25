(defpackage :claudia/formula
  (:use :cl
        :claudia/term
        :claudia/pprint)
  (:shadowing-import-from :claudia/term
                          :substitute
                          :substitutable)
  (:export :formula :formula-list :free-p :formula-=
           :∧ :∧-1 :∧-2
           :∨ :∨-1 :∨-2
           :¬ :¬-1
           :→ :→-1 :→-2
           :∀ :∀-var :∀-formula
           :∃ :∃-var :∃-formula
           :prop :predicate))
(in-package :claudia/formula)

;; ****************************************************************
;; meta data type
;;
;; const := const-val | func const*
;; term := const | var | func term*
;; atomic := prop | predicate term*
;; formula := formula ∧ formula | formula ∨ formula | ¬ formula | atomic
;;
;; ****************************************************************
(defclass formula nil
  ((free-vars :initarg :free-vars :initform nil :accessor %free-vars :reader free-vars)))
(defmethod print-object ((formula formula) stream)
  (declare (ignore stream))
  (error "print-object method for type ~A is not defined" (type-of formula)))
(defmethod pprint-formula ((formula formula) stream)
  (declare (ignore stream))
  (error "pprint-formula method for type ~A is not defined" (type-of formula)))
(def-claudia-print (formula) (formula stream)
  (pprint-formula formula stream))
(defun formula-list-p (thing)
  (and (listp thing)
       (every (lambda (x) (typep x 'formula)) thing)))
(deftype formula-list ()
  `(satisfies formula-list-p))
(defmethod substitute ((place formula) var term)
  (error "substitute method for type ~A is not defined" (type-of place)))
(defmethod substitutable ((place formula) var term)
  (error "substitutable method for type ~A is not defined" (type-of place)))
(defmethod formula-= ((a formula) (b formula))
  (error "formula-= method for type ~A is not defined" (type-of a)))

(defun free-p (var &optional x)
  (if (or (typep x 'formula)
          (typep x 'term))
      (find var (free-vars x) :test #'eq)
      (lambda (f) (free-p var f))))

;; ∧
(defclass ∧ (formula)
  ((∧-1 :initarg :∧-1 :reader ∧-1 :type formula)
   (∧-2 :initarg :∧-2 :reader ∧-2 :type formula)))
(defun ∧ (f1 f2)
  (make-instance '∧ :∧-1 f1 :∧-2 f2))
(defmethod print-object ((formula ∧) stream)
  (format stream "(~A ~A ~A)" '∧ (∧-1 formula) (∧-2 formula)))
(defmethod pprint-formula ((formula ∧) stream)
  (format stream "(~:W ∧ ~:W)" (∧-1 formula) (∧-2 formula)))
(defmethod initialize-instance :after ((formula ∧) &key)
  (setf (%free-vars formula) (union (free-vars (∧-1 formula)) (free-vars (∧-2 formula)))))
(defmethod substitute ((place ∧) (var var) (term term))
  (∧ (substitute (∧-1 place) var term) (substitute (∧-2 place) var term)))
(defmethod substitutable ((place ∧) (var var) (term term))
  (and (substitutable (∧-1 place) var term) (substitutable (∧-2 place) var term)))
(defmethod formula-= ((a ∧) (b formula))
  (and (typep b '∧)
       (formula-= (∧-1 a) (∧-1 b))
       (formula-= (∧-2 a) (∧-2 b))))

;; ∨
(defclass ∨ (formula)
  ((∨-1 :initarg :∨-1 :reader ∨-1 :type formula)
   (∨-2 :initarg :∨-2 :reader ∨-2 :type formula)))
(defun ∨ (f1 f2)
  (make-instance '∨ :∨-1 f1 :∨-2 f2))
(defmethod print-object ((formula ∨) stream)
  (format stream "(~A ~A ~A)" '∨ (∨-1 formula) (∨-2 formula)))
(defmethod pprint-formula ((formula ∨) stream)
  (format stream "(~:W ∨ ~:W)" (∨-1 formula) (∨-2 formula)))
(defmethod initialize-instance :after ((formula ∨) &key)
  (setf (%free-vars formula) (union (free-vars (∨-1 formula)) (free-vars (∨-2 formula)))))
(defmethod substitute ((place ∨) (var var) (term term))
  (∨ (substitute (∨-1 place) var term) (substitute (∨-2 place) var term)))
(defmethod substitutable ((place ∨) (var var) (term term))
  (and (substitutable (∨-1 place) var term) (substitutable (∨-2 place) var term)))
(defmethod formula-= ((a ∨) (b formula))
  (and (typep b '∨)
       (formula-= (∨-1 a) (∨-1 b))
       (formula-= (∨-2 a) (∨-2 b))))

;; ¬
(defclass ¬ (formula)
  ((¬-1 :initarg :¬-1 :reader ¬-1 :type formula)))
(defun ¬ (f)
  (make-instance '¬ :¬-1 f))
(defmethod print-object ((formula ¬) stream)
  (format stream "(~A ~A)" '¬ (¬-1 formula)))
(defmethod pprint-formula ((formula ¬) stream)
  (format stream "¬~:W" (pprint-formula (¬-1 formula) nil)))
(defmethod initialize-instance :after ((formula ¬) &key)
  (setf (%free-vars formula) (free-vars (¬-1 formula))))
(defmethod substitute ((place ¬) (var var) (term term))
  (¬ (substitute (¬-1 place) var term)))
(defmethod substitutable ((place ¬) (var var) (term term))
  (substitutable (¬-1 place) var term))
(defmethod formula-= ((a ¬) (b formula))
  (and (typep b '¬)
       (formula-= (¬-1 a) (¬-1 b))))

;; →
(defclass → (formula)
  ((→-1 :initarg :→-1 :reader →-1 :type formula)
   (→-2 :initarg :→-2 :reader →-2 :type formula)))
(defun → (f1 f2)
  (make-instance '→ :→-1 f1 :→-2 f2))
(defmethod print-object ((formula →) stream)
  (format stream "(~A ~A ~A)" '→ (→-1 formula) (→-2 formula)))
(defmethod pprint-formula ((formula →) stream)
  (format stream "(~:W → ~:W)" (→-1 formula) (→-2 formula)))
(defmethod initialize-instance :after ((formula →) &key)
  (setf (%free-vars formula) (union (free-vars (→-1 formula)) (free-vars (→-2 formula)))))
(defmethod substitute ((place →) (var var) (term term))
  (→ (substitute (→-1 place) var term) (substitute (→-2 place) var term)))
(defmethod substitutable ((place →) (var var) (term term))
  (and (substitutable (→-1 place) var term) (substitutable (→-2 place) var term)))
(defmethod formula-= ((a →) (b formula))
  (and (typep b '→)
       (formula-= (→-1 a) (→-1 b))
       (formula-= (→-2 a) (→-2 b))))

;; ∀
(defclass ∀ (formula)
  ((∀-var :initarg :var :reader ∀-var :type var)
   (∀-formula :initarg :formula :reader ∀-formula :type formula)))
(defun ∀ (var formula)
  (make-instance '∀ :var var :formula formula))
(defmethod print-object ((formula ∀) stream)
  (format stream "(~A ~A ~A)" '∀ (∀-var formula) (∀-formula formula)))
(defmethod pprint-formula ((formula ∀) stream)
  (format stream "∀~:W(~:W)" (∀-var formula) (∀-formula formula)))
(defmethod initialize-instance :after ((formula ∀) &key)
  (setf (%free-vars formula) (remove (∀-var formula) (free-vars (∀-formula formula)) :test #'eq)))
(defmethod substitute ((place ∀) (var var) (term term))
  (if (eq (∀-var place) var)
      place
      (∀ (∀-var place) (substitute (∀-formula place) var term))))
(defmethod substitutable ((place ∀) (var var) (term term))
  (or (not (term-= (∀-var place) var))
      (not (free-p var (∀-formula place)))
      (and (substitutable (∀-formula place) var term)
           (not (free-p (∀-var place) term)))))
(defmethod formula-= ((a ∀) (b formula))
  (and (typep b '∀)
       (term-= (∀-var a) (∀-var b))
       (formula-= (∀-formula a) (∀-formula b))))

;; ∃
(defclass ∃ (formula)
  ((∃-var :initarg :var :reader ∃-var :type var)
   (∃-formula :initarg :formula :reader ∃-formula :type formula)))
(defun ∃ (var formula)
  (make-instance '∃ :var var :formula formula))
(defmethod print-object ((formula ∃) stream)
  (format stream "(~A ~A ~A)" '∃ (∃-var formula) (∃-formula formula)))
(defmethod pprint-formula ((formula ∃) stream)
  (format stream "∃~:W(~:W)" (∃-var formula) (∃-formula formula)))
(defmethod initialize-instance :after ((formula ∃) &key)
  (setf (%free-vars formula) (remove (∃-var formula) (free-vars (∃-formula formula)) :test #'eq)))
(defmethod substitute ((place ∃) (var var) (term term))
  (if (eq (∃-var place) var)
      place
      (∃ (∃-var place) (substitute (∃-formula place) var term))))
(defmethod substitutable ((place ∃) (var var) (term term))
  (or (not (term-= (∃-var place) var))
      (not (free-p var (∃-formula place)))
      (and (substitutable (∃-formula place) var term)
           (not (free-p (∃-var place) term)))))
(defmethod formula-= ((a ∃) (b formula))
  (and (typep b '∃)
       (term-= (∃-var a) (∃-var b))
       (formula-= (∃-formula a) (∃-formula b))))

;;;; atomic
(defclass atomic (formula) nil)
(defmethod substitutable ((place atomic) (var var) (term term))
  t)

;; prop
(defclass prop (atomic)
  ((name :initarg :name :reader name)))
(defun prop (name)
  (make-instance 'prop :name name))
(defmethod print-object ((formula prop) stream)
  (format stream "#<Prop: ~A>" (name formula)))
(defmethod pprint-formula ((formula prop) stream)
  (format stream "~A" (name formula)))
(defmethod substitute ((place prop) (var var) (term term))
  place)
(defmethod formula-= ((a prop) (b formula))
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
(defmethod print-object ((formula predicate) stream)
  (format stream "(~A ~A ~{~A~^ ~})" 'predicate (name formula) (terms formula)))
(defmethod pprint-formula ((formula predicate) stream)
  (if (= (length (terms formula)) 2) ;; ?
      (format stream "(~:W ~:W ~:W)" (nth 0 (terms formula)) (name formula) (nth 1 (terms formula)))
      (format stream "~:W(~{~:W~^ ~})" (name formula) (terms formula))))
(defmethod substitute ((place predicate) (var var) (term term))
  (apply #'predicate
         (substitute (name place) var term)
         (mapcar (lambda (x) (substitute x var term)) (terms place))))
(defmethod formula-= ((a predicate) (b formula))
  (and (typep b 'predicate)
       (term-= (name a) (name b))
       (every #'term-= (terms a) (terms b))))
