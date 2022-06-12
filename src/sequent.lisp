(defpackage :claudia/sequent
  (:use :cl
        :claudia/pprint)
  (:import-from :claudia/formula)
  (:export :make-sequent
           :l :r
           :length-l :length-r
           :nth-l :nth-r
           :empty-l :empty-r
           :rest-l :rest-r
           :print-seq
           :with-splited-sequent))
(in-package :claudia/sequent)

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

(defun rest-l (seq)
  (rest (l seq)))

(defun rest-r (seq)
  (rest (r seq)))

(defun print-seq (seq stream)
  (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
    (format stream "~{~:W~^, ~}  ⊢  ~{~:W~^, ~}" (l seq) (r seq))))

;; ll0, ll1,... lln, lr0, lr1,... ⊢ rl0, rl1,... rlm, rr0, rr1,...
(defmacro with-splited-sequent (sequent (n m ll lr rl rr) &body body)
  (let ((seq (gensym "SEQUENT-"))
        (n% (gensym "N-")))
    `(let ((,seq ,sequent)
           (,n% ,n))
       (cond ((< (length-l ,seq) ,n%) (error ""))
             ((< (length-r ,seq) ,m) (error ""))
             (t                   
              (let ((,ll (subseq (l ,seq) 0 ,n%))
                    (,lr (subseq (l ,seq) ,n%))
                    (,rl (subseq (r ,seq) 0 ,m))
                    (,rr (subseq (r ,seq) ,m)))
                ,@body))))))
