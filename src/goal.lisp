(defpackage :logic/goal
  (:use :cl)
  (:import-from :logic/sequent
                :print-seq)
  (:export :make-goal
           :do-step
           :print-goal))
(in-package :logic/goal)

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
  (if (null goal)
      (format t "Complete !!~%")
      (loop :for seq :in goal
            :for n :from 0
            :do (print-seq seq n))))
