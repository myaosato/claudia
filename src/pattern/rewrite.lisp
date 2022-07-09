(defpackage :claudia/pattern/rewrite
  (:use :cl
        :claudia/pattern/pattern
        :claudia/meta-data/interface)
  (:export :rewrite))
(in-package :claudia/pattern/rewrite)

(defgeneric rewrite (target rule))

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

(defmethod rewrite ((formula ∧) (rule rule))
  (∧ (rewrite (∧-0 formula) rule) (rewrite (∧-1 formula) rule)))
(defmethod rewrite ((formula ∨) (rule rule))
  (∨ (rewrite (∨-0 formula) rule) (rewrite (∨-1 formula) rule)))
(defmethod rewrite ((formula ¬) (rule rule))
  (¬ (rewrite (¬-0 formula) rule)))
(defmethod rewrite ((formula →) (rule rule))
  (→ (rewrite (→-0 formula) rule) (rewrite (→-1 formula) rule)))
(defmethod rewrite ((formula ∀) (rule rule))
  (∀ (∀-var formula) (rewrite (∀-formula formula) rule)))
(defmethod rewrite ((formula ∃) (rule rule))
  (∃ (∃-var formula) (rewrite (∃-formula formula) rule)))
(defmethod rewrite ((formula predicate) (rule rule))
  (let* ((1st-step (apply #'func (mapcar (lambda (x) (rewrite x rule)) (predicate-terms formula))))
         (reducible (reduction rule 1st-step)))
    (if reducible
        (apply #'predicate (func-terms reducible))
        (apply #'predicate (func-terms 1st-step)))))
(defmethod rewrite ((formula prop) (rule rule))
  formula)
