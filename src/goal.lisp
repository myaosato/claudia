(defpackage :claudia/goal
  (:use :cl)
  (:import-from :claudia/sequent
                :sequent
                :sequent-list)
  (:export :goal
           :completed-p
           :sequents
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


(defun nth-seq (n goal)
  (nth n (sequents goal)))

(defun app (goal n rule &rest args)
  (let ((result (apply rule (nth-seq n goal) args)))
    (apply #'goal (append (subseq (sequents goal) 0 n)
                          result
                          (subseq (sequents goal) (1+ n))))))
