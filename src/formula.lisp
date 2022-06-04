(defpackage :logic/formula
  (:use :cl)
  (:export :∧ :is-∧ :∧-1 :∧-2
           :∨ :is-∨ :∨-1 :∨-2
           :¬ :is-¬ :¬-1
           :⇒ :is-⇒ :⇒-1 :⇒-2
           :prop :is-prop :prop-name
           :format-formula))
(in-package :logic/formula)

;; ****************************************************************
;; meta data type
;;
;; const := const-val | func const*
;; term := const | var
;; atomic := prop | predicate term*
;; formula := formula ∧ formula | formula ∨ formula | ¬ formula | atomic
;;
;; ****************************************************************

(defun ∧ (f1 f2)
  (list :and f1 f2))
(defun is-∧ (formula)
  (eq (car formula) :and))
(defun ∧-1 (and-formula)
  (nth 1 and-formula))
(defun ∧-2 (and-formula)
  (nth 2 and-formula))

(defun ∨ (f1 f2)
  (list :or f1 f2))
(defun is-∨ (formula)
  (eq (car formula) :or))
(defun ∨-1 (and-formula)
  (nth 1 and-formula))
(defun ∨-2 (and-formula)
  (nth 2 and-formula))

(defun ¬ (f)
  (list :not f))
(defun is-¬ (formula)
  (eq (car formula) :not))
(defun ¬-1 (not-formula)
  (nth 1 not-formula))

(defun → (f1 f2)
  (list :to f1 f2))
(defun is-→ (formula)
  (eq (car formula) :to))
(defun →-1 (to-formula)
  (nth 1 to-formula))
(defun →-2 (to-formula)
  (nth 2 to-formula))

(defun prop (name)
  (list :prop name))
(defun is-prop (formula)
  (eq (car formula) :prop))
(defun prop-name (formula)
  (nth 1 formula))

(defun format-formula (formula)
  (cond ((equal formula 0)
         0)
        ((atom formula)
         (format nil "~A" formula))
        ((is-prop formula)
         (format nil "~A" (prop-name formula)))
        ((is-¬ formula)
         (format nil "¬~A" (format-formula (¬-1 formula))))
        ((is-∧ formula)
         (format nil "(~A ∧ ~A)" (format-formula (∧-1 formula)) (format-formula (∧-2 formula))))
        ((is-∨ formula)
         (format nil "(~A ∨ ~A)" (format-formula (∨-1 formula)) (format-formula (∨-2 formula))))
        ((is-→ formula)
         (format nil "(~A ∧ ~A)" (format-formula (→-1 formula)) (format-formula (→-2 formula))))
        ((eq (car formula) :nat)
         (format nil "~A is Nat" (format-formula (nth 1 formula))))
        ((eq (car formula) :succ)
         (1+ (cadr formula)))
        (t
         (error "invalid formula"))))
