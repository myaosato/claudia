(defpackage :claudia/meta-data/parser
  (:use :cl
        :claudia/meta-data/term
        :claudia/meta-data/formula)
  (:export :terms
           :formulas))
(in-package :claudia/meta-data/parser)

(defun %parse-term (x)
  (cond ((atom x)
         x)
        (t
         (list* 'func (mapcar #'%parse-term x)))))

(defun %parse-formula (x)
  (cond ((atom x)
         x)
        ((find (car x) '(∧ ∨ →))
         (list (nth 0 x) (%parse-formula (nth 1 x)) (%parse-formula (nth 2 x))))
        ((eq (car x) '¬)
         (list (nth 0 x) (%parse-formula (nth 1 x))))
        ((find (car x) '(∀ ∃))
         (list (nth 0 x) (nth 1 x) (%parse-formula (nth 2 x))))
        (t
         (list* 'predicate (mapcar #'%parse-term x)))))

(defmacro terms (x)
  (%parse-term x))

(defmacro formulas (x)
  (%parse-formula x))
