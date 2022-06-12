(defpackage :claudia/axiom
  (:use :cl)
  (:export :def-axiom))
(in-package :claudia/axiom)

;; ****************************************************************
;; Axiom
;;
;; -----
;; Γ ⊢ Δ
;;
;; ****************************************************************

(defmacro def-axiom (name lambda-list &body condition)
  `(defun ,name ,lambda-list
     (if (progn ,@condition)
         nil
         (error ""))))
