(defpackage :logic/sequent
  (:use :cl)
  (:import-from :logic/formula
                :format-formula)
  (:export :make-sequent
           :l
           :r
           :length-l
           :length-r
           :nth-l
           :nth-r
           :empty-l
           :empty-r
           :format-seq
           :print-seq
           :with-splited-sequent))
(in-package :logic/sequent)

;; ****************************************************************
;; sequent
;; ****************************************************************

(defun make-sequent (antecedent succedent)
  (cons antecedent succedent))

(defun l (seq)
  (car seq))

(defun r (seq)
  (cdr seq))

(defun length-l (seq)
  (length (l seq)))

(defun length-r (seq)
  (length (r seq)))

(defun nth-l (n seq)
  (nth n (l seq)))

(defun nth-r (n seq)
  (nth n (r seq)))

(defun empty-l (seq)
  (null (l seq)))

(defun empty-r (seq)
  (null (r seq)))

(defun format-seq (seq n)
  (format nil "H~A: ~{~A~^, ~}~%C~A: ~{~A~^, ~}~%"
          n (mapcar #'format-formula (l seq))
          n (mapcar #'format-formula (r seq))))

(defun print-seq (seq n)
  (format t "~A" (format-seq seq n))
  seq)

;; ll0, ll1,... lln, lr0, lr1,... |- rl0, rl1,... rlm, rr0, rr1,...
(defmacro with-splited-sequent (sequent (n m ll lr rl rr) &body body)
  (let ((seq (gensym "SEQUENT-")))
    `(let ((,seq ,sequent))
       (cond ((< (length-l ,seq) ,n) (error ""))
             ((< (length-r ,seq) ,m) (error ""))
             (t                   
              (let ((,ll (subseq (l ,seq) 0 ,n))
                    (,lr (subseq (l ,seq) ,n))
                    (,rl (subseq (r ,seq) 0 ,m))
                    (,rr (subseq (r ,seq) ,m)))
                ,@body))))))
