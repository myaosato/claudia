(defpackage :logic/axiom
  (:use :cl)
  (:export :def-axiom))
(in-package :logic/axiom)

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
         (list t)
         (error ""))))
