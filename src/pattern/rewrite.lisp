(defpackage :claudia/pattern/rewrite
  (:use :cl
        :claudia/pattern/pattern
        :claudia/meta-data/interface)
  (:export :rewrite))
(in-package :claudia/pattern/rewrite)

(defmethod rewrite ((term var) (rule rule))
  term)
(defmethod rewrite ((term const) (rule rule))
  term)
(defmethod rewrite ((term func) (rule rule))
  (let* ((1st-step (apply #'func (mapcar (lambda (x) (rewrite x rule)) (func-terms term))))
         (reducible (reduction rule 1st-step)))
    (if reducible
        reducible
        1st-step)))
(defmethod rewrite ((x ∧) (rule rule))
  (∧ (rewrite (∧-0 x) rule) (rewrite (∧-1 x) rule)))
(defmethod rewrite ((x ∨) (rule rule))
  (∨ (rewrite (∨-0 x) rule) (rewrite (∨-1 x) rule)))
(defmethod rewrite ((x ¬) (rule rule))
  (¬ (rewrite (¬-0 x) rule)))
(defmethod rewrite ((x →) (rule rule))
  (→ (rewrite (→-0 x) rule) (rewrite (→-1 x) rule)))
(defmethod rewrite ((x ∀) (rule rule))
  (∀ (∀-var x) (rewrite (∀-formula x) rule)))
(defmethod rewrite ((x ∃) (rule rule))
  (∃ (∃-var x) (rewrite (∃-formula x) rule)))
(defmethod rewrite ((x predicate) (rule rule))
  (apply #'predicate (mapcar (lambda (x) (rewrite x rule)) (predicate-terms x))))
(defmethod rewrite ((x prop) (rule rule))
  x)
