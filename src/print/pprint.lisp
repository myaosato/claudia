(defpackage :claudia/print/pprint
  (:use :cl
        :claudia/meta-data/interface
        :claudia/sequent
        :claudia/goal)
  (:export :print-claudia-print-dispatch))
(in-package :claudia/print/pprint)

(defparameter print-claudia-print-dispatch (copy-pprint-dispatch nil))
(defmacro def-claudia-print ((class) (obj stream) &body body)
  `(set-pprint-dispatch ',class
                        (lambda (,stream ,obj)
                          (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
                            ,@body))
                        0 print-claudia-print-dispatch))

(def-claudia-print (meta-data) (x stream)
  (pprint-meta-data x stream))

(defmethod pprint-meta-data ((x meta-data) stream)
  (declare (ignore stream))
  (error "pprint-meta-data method for type ~A is not defined" (type-of x)))
(defmethod pprint-meta-data ((term var) stream)
  (format stream "~A" (var-name term)))
(defmethod pprint-meta-data ((term const) stream)
  (format stream "~A" (const-name term)))
(defmethod pprint-meta-data ((term func) stream)
  (format stream "(~{~:W~^ ~})" (terms term)))
(defmethod pprint-meta-data ((formula ∧) stream)
  (format stream "(~:W ∧ ~:W)" (∧-1 formula) (∧-2 formula)))
(defmethod pprint-meta-data ((formula ∨) stream)
  (format stream "(~:W ∨ ~:W)" (∨-1 formula) (∨-2 formula)))
(defmethod pprint-meta-data ((formula ¬) stream)
  (format stream "¬~:W" (¬-1 formula)))
(defmethod pprint-meta-data ((formula →) stream)
  (format stream "(~:W → ~:W)" (→-1 formula) (→-2 formula)))
(defmethod pprint-meta-data ((formula ∀) stream)
  (format stream "∀~:W(~:W)" (∀-var formula) (∀-formula formula)))
(defmethod pprint-meta-data ((formula ∃) stream)
  (format stream "∃~:W(~:W)" (∃-var formula) (∃-formula formula)))
(defmethod pprint-meta-data ((formula prop) stream)
  (format stream "~A" (prop-name formula)))
(defmethod pprint-meta-data ((formula predicate) stream)
  (format stream "(~{~:W~^ ~})" (terms formula)))

(def-claudia-print (sequent) (seq stream)
  (format stream "~{~:W~^, ~}  ⊢  ~{~:W~^, ~}" (l seq) (r seq)))

(def-claudia-print (goal) (goal stream)
  (if (completed-p goal)
      (format stream "Complete !!")
      (format stream "~{[~A]: ~W~^~%~}" (loop :for seq :in (sequents goal)
                                              :for n :from 0
                                              :append (list n seq)))))

