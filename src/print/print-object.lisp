(defpackage :claudia/print/print-object
  (:use :cl
        :claudia/meta-data/interface
        :claudia/sequent
        :claudia/goal
        :claudia/print/pprint))
(in-package :claudia/print/print-object)

(defmethod print-object ((term var) stream)
  (format stream "~A" (var-name term)))
(defmethod print-object ((term  const) stream)
    (format stream "~A" (const-name term)))
(defmethod print-object ((term func) stream)
  (format stream "(~A ~{~A~^ ~})" 'func (func-terms term)))
(defmethod print-object ((formula ∧) stream)
  (format stream "(~A ~A ~A)" '∧ (∧-0 formula) (∧-1 formula)))
(defmethod print-object ((formula ∨) stream)
  (format stream "(~A ~A ~A)" '∨ (∨-0 formula) (∨-1 formula)))
(defmethod print-object ((formula ¬) stream)
  (format stream "(~A ~A)" '¬ (¬-0 formula)))
(defmethod print-object ((formula →) stream)
  (format stream "(~A ~A ~A)" '→ (→-0 formula) (→-1 formula)))
(defmethod print-object ((formula ∀) stream)
  (format stream "(~A ~A ~A)" '∀ (∀-var formula) (∀-formula formula)))
(defmethod print-object ((formula ∃) stream)
  (format stream "(~A ~A ~A)" '∃ (∃-var formula) (∃-formula formula)))
(defmethod print-object ((formula prop) stream)
  (format stream "#<Prop: ~A>" (prop-name formula)))
(defmethod print-object ((formula predicate) stream)
  (format stream "(~A ~{~A~^ ~})" 'predicate (predicate-terms formula)))
(defmethod print-object ((seq sequent) stream)
  (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
    (format stream "#<SEQUENT: ~{~:W~^, ~}  ⊢  ~{~:W~^, ~} >" (l seq) (r seq))))
(defmethod print-object ((goal goal) stream)
  (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
    (format stream "#<GOAL: ~W >" goal)))

