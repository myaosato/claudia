(defpackage :claudia/goal
  (:use :cl
        :claudia/pprint)
  (:import-from :claudia/sequent
                :sequent
                :sequent-list)
  (:export :goal
           :app))
(in-package :claudia/goal)

;; ****************************************************************
;; goal
;; ****************************************************************
(defclass goal ()
  ((sequents :initarg :sequents :accessor sequents :type sequent-list)))
(defun goal (&rest sequents)
  (make-instance 'goal :sequents sequents))
(defun completed-p (goal)
  (null (sequents goal)))
(def-claudia-print (goal) (goal stream)
  (if (completed-p goal)
      (format stream "Complete !!")
      (format stream "~{[~A]: ~W~^~%~}" (loop :for seq :in (sequents goal)
                                              :for n :from 0
                                              :append (list n seq)))))
(defmethod print-object ((goal goal) stream)
  (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
    (format stream "#<GOAL: ~W >" goal)))

(defun nth-seq (n goal)
  (nth n (sequents goal)))

(defun app (goal n rule &rest args)
  (let ((result (apply rule (nth-seq n goal) args)))
    (apply #'goal (append (subseq (sequents goal) 0 n)
                          result
                          (subseq (sequents goal) (1+ n))))))
