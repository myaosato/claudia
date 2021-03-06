(defpackage :claudia/sequent
  (:use :cl
        :claudia/meta-data/interface
        :claudia/pattern/interface)
  (:shadow :rewrite)
  (:export :sequent :sequent-list
           :l :r
           :length-l :length-r
           :nth-l :nth-r
           :empty-l :empty-r
           :rest-l :rest-r
           :remove-nth-l :remove-nth-r
           :replace-nth-l :replace-nth-r
           :rewrite-l :rewrite-r))
(in-package :claudia/sequent)

;; ****************************************************************
;; sequent
;; ****************************************************************
(defclass sequent nil
  ((antecedent :initarg :antecedent :accessor l :type formula-list)
   (succedent :initarg :succedent :accessor r :type formula-list)))
(defun sequent (antecedent succedent)
  (make-instance 'sequent :antecedent antecedent :succedent succedent))
(defun sequent-list-p (thing)
  (and (listp thing)
       (every (lambda (x) (typep x 'sequent)) thing)))
(deftype sequent-list ()
  `(satisfies sequent-list-p))

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
(defun remove-nth-l (n seq)
  (append (subseq (l seq) 0 n) (subseq (l seq) (1+ n))))
(defun remove-nth-r (n seq)
  (append (subseq (r seq) 0 n) (subseq (r seq) (1+ n))))
(defun replace-nth-l (n formula-list seq)
  (append (subseq (l seq) 0 n) formula-list (subseq (l seq) (1+ n))))
(defun replace-nth-r (n formula-list seq)
  (append (subseq (r seq) 0 n) formula-list (subseq (r seq) (1+ n))))

; rewrite
(defun rewrite-l (seq rule n)
  (list (sequent (replace-nth-l n (list (claudia/pattern/interface:rewrite (nth-l n seq) rule)) seq)
                 (r seq))))
(defun rewrite-r (seq rule n)
  (list (sequent (l seq)
                 (replace-nth-r n (list (claudia/pattern/interface:rewrite (nth-r n seq) rule)) seq))))
