(defpackage :claudia/goal
  (:use :cl
        :claudia/pprint)
  (:import-from :claudia/sequent)
  (:export :make-goal
           :do-step
           :print-goal))
(in-package :claudia/goal)

;; ****************************************************************
;; goal
;; ****************************************************************

(defun make-goal (sequent &rest sequents)
  (list* sequent sequents))

(defun do-step (current-goal n rule args)
  (let ((result (apply rule (nth n current-goal) args)))
    `(,@(subseq  current-goal 0 n)
      ,@(cond ((equal result (list t))
               nil)
              ((null result)
               (error ""))
              (t result))
      ,@(subseq  current-goal (1+ n)))))

(defun print-goal (goal)
  (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
    (if (null goal)
        (format t "Complete !!~%")
        (loop :for seq :in goal
              :for n :from 0
              :do (format t "[~A]: ~W~%" n seq)))))
