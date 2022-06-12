(defpackage :claudia/goal
  (:use :cl
        :claudia/pprint)
  (:import-from :claudia/sequent
                :sequent
                :sequent-list)
  (:export :goal
           :do-step))
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
(defun nth-seq (n goal)
  (nth n (sequents goal)))

(defun do-step (goal n rule args)
  (let ((result (apply rule (nth-seq n goal) args)))
    (apply #'goal `(,@(subseq (sequents goal) 0 n)
                    ,@result
                    ,@(subseq (sequents goal) (1+ n))))))
