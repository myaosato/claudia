(defpackage :claudia/lk
  (:use :cl
        :claudia/formula
        :claudia/sequent)
  (:shadowing-import-from :claudia/formula
                          :substitute
                          :substitutable)
  (:export :∧ :∧-1 :∧-2
           :∨ :∨-1 :∨-2
           :¬ :¬-1
           :→ :→-1 :→-2
           :prop :def-predicate
           ;; 
           :id :cut
           :and-l1 :and-l2 :and-r
           :or-l :or-r1 :or-r2
           :not-l :not-r
           :to-l :to-r
           :forall-l :forall-r
           :exists-l :exists-r
           :wl :wr
           :cl :cr
           :pl :pr))
(in-package :claudia/lk)

;; ****************************************************************
;; LK
;;
;; ref. https://ja.wikipedia.org/wiki/シークエント計算
;;
;; WIP: implemented propositional logic part only.
;; TODO
;; - ∀
;; - ∃
;; ****************************************************************

;; axiom of LK
;; ------
;; Γ,A,Δ  ⊢ Σ,A,Π
(defun id (seq)
  (if (and (> (length-l seq) 0)
           (> (length-r seq) 0)
           (some (lambda (lf) (some (lambda (rf) (formula-= lf rf)) (r seq))) (l seq)))
      nil ;; proved
      (error "")))

;; Cut
;; Γ ⊢ A,Δ  A,Γ ⊢ Δ
;; -----------------
;; Γ ⊢ Δ
(defun cut (seq formula)
  (list (sequent (l seq) (cons formula (r seq)))
        (sequent (cons formula (l seq)) (r seq))))

;; And
;; A,Γ ⊢ Δ          B,Γ ⊢ Δ          Γ ⊢ A,Δ  Γ ⊢ B,Δ
;; ---------(∧L1)   ---------(∧L2)   ----------------(∧R)
;; A∧B,Γ ⊢ Δ        A∧B,Γ ⊢ Δ        Γ ⊢ A∧B,Δ 
(defun and-l1 (seq)
  (let ((focus (nth-l 0 seq)))
    (if (typep focus '∧)
        (list (sequent (cons (∧-1 focus) (rest-l seq)) (r seq)))
        (error ""))))

(defun and-l2 (seq)
  (let ((focus (nth-l 0 seq)))
    (if (typep focus '∧)
        (list (sequent (cons (∧-2 focus) (rest-l seq)) (r seq)))
        (error ""))))

(defun and-r (seq)
  (let ((focus (nth-r 0 seq)))
    (if (typep focus '∧)
        (list (sequent (l seq) (cons (∧-1 focus) (rest-r seq)))
              (sequent (l seq) (cons (∧-2 focus) (rest-r seq))))
        (error ""))))

;; Or
;; Γ ⊢ A,Δ          Γ ⊢ B,Δ          A,Γ ⊢ Δ  B,Γ ⊢ Δ
;; ---------(∨R1)   ---------(∨R2)   ----------------(∨L)
;; Γ ⊢ A∨B,Δ        Γ ⊢ A∨B,Δ        A∨B,Γ ⊢ Δ 
(defun or-r1 (seq)
  (let ((focus (nth-r 0 seq)))
    (if (typep focus '∨)
        (list (sequent (l seq) (cons (∨-1 focus) (rest-r seq))))
        (error ""))))

(defun or-r2 (seq)
  (let ((focus (nth-r 0 seq)))
    (if (typep focus '∨)
        (list (sequent (l seq) (cons (∨-2 focus) (rest-r seq))))
        (error ""))))

(defun or-l (seq)
  (let ((focus (nth-l 0 seq)))
    (if (typep focus '∨)
        (list (sequent (cons (∨-1 focus) (rest-l seq)) (r seq))
              (sequent (cons (∨-2 focus) (rest-l seq)) (r seq)))
        (error ""))))

;; Not
;; Γ ⊢ A,Δ       A,Γ ⊢ Δ
;; --------(¬L)  ---------(¬R)
;; ¬A,Γ ⊢ Δ      Γ ⊢ ¬A,Δ
(defun not-l (seq)
  (let ((focus (nth-l 0 seq)))
    (if (typep focus '¬)
        (list (sequent (rest-l seq) (cons (¬-1 focus) (r seq))))
        (error ""))))

(defun not-r (seq)
  (let ((focus (nth-r 0 seq)))
    (if (typep focus '¬)
        (list (sequent (cons (¬-1 focus) (l seq)) (rest-r seq)))
        (error ""))))

;; To
;; A,Γ ⊢ B,Δ      Γ ⊢ A,Δ  B,Γ ⊢ Δ
;; ---------(¬R)  ----------------(¬L)
;; Γ ⊢ A→B,Δ      A→B,Γ ⊢ Δ
(defun to-r (seq)
  (let ((focus (nth-r 0 seq)))
    (if (typep focus '→)
        (list (sequent (cons (→-1 focus) (l seq)) (cons (→-2 focus) (rest-r seq))))
        (error ""))))

(defun to-l (seq)
  (let ((focus (nth-l 0 seq)))
    (if (typep focus '→)
        (list (sequent (rest-l seq) (cons (→-1 focus) (r seq)))
              (sequent (cons (→-2 focus) (rest-l seq)) (r seq)))
        (error ""))))

;; For all
;; A[t/x],Γ ⊢ Δ      Γ ⊢ A,Δ
;; ------------(∀L)  ---------(∀R)  
;; ∀xA,Γ ⊢ Δ         Γ ⊢ ∀xA,Δ
(defun forall-l (seq term)
  (let ((focus (nth-l 0 seq)))
    (if (and (typep focus '∀)
             (substitutable focus (∀-var focus) term))
        (list (sequent (cons (substitute (∀-formula focus) (∀-var focus) term) (rest-l seq)) (r seq)))
        (error ""))))

(defun forall-r (seq)
  (let ((focus (nth-r 0 seq)))
    (if (and (typep focus '∀)
             (notany (free-p (∀-var focus)) (l seq))
             (notany (free-p (∀-var focus)) (rest-r seq)))
        (list (sequent (l seq) (cons (∀-formula focus) (rest-r seq))))
        (error ""))))

;; Exist
;; A,Γ ⊢ Δ        Γ ⊢ A[t/x],Δ
;; ---------(∃L)  ------------(∃R)  
;; ∃xA,Γ ⊢ Δ      Γ ⊢ ∃xA,Δ
(defun exists-l (seq)
  (let ((focus (nth-l 0 seq)))
    (if (and (typep focus '∃)
             (notany (free-p (∃-var focus)) (rest-l seq))
             (notany (free-p (∃-var focus)) (r seq)))
        (list (sequent (cons (∃-formula focus) (rest-l seq)) (r seq)))
        (error ""))))

(defun exists-r (seq term)
  (let ((focus (nth-r 0 seq)))
    (if (and (typep focus '∃)
             (substitutable focus (∃-var focus) term))
        (list (sequent (l seq) (cons (substitute (∃-formula focus) (∃-var focus) term) (rest-r seq))))
        (error ""))))


;; Weakening
;; Γ ⊢ Δ         Γ ⊢ Δ
;; -------(WL)  ---------(WR)
;; A,Γ ⊢ Δ       Γ ⊢ A,Δ
(defun wl (seq &optional (n 0))
  (if (> (length-l seq) n)
      (list (remove-nth-l n seq))
      (error "")))

(defun wr (seq &optional (n 0))
  (if (>= (length-r seq) 1)
      (list (remove-nth-r n seq))
      (error "")))

;; Contraction
;; A,A,Γ ⊢ Δ      Γ ⊢ A,A,Δ
;; ---------(CL)  ---------(CR)
;; A,Γ ⊢ Δ        Γ ⊢ A,Δ
(defun cl (seq)
  (if (>= (length-l seq) 1)
      (list (sequent (cons (nth-l 0 seq) (l seq)) (r seq)))
      (error "")))

(defun cr (seq)
  (if (>= (length-r seq) 1)
      (list (sequent (l seq) (cons (nth-r 0 seq) (r seq))))
      (error "")))


;; Permutation
;; Γ0,..Γm,..Γn,.. ⊢ Δ      Γ ⊢ Δ0,..Δm,..Δn,..
;; -------------------(PL)  -------------------(CR)
;; Γ0,..Γn,..Γm,.. ⊢ Δ      Γ ⊢ Δ0,..Δn,..Δm,..
(defun pl (seq n m)
  (if (> (length-l seq) (max m n))
      (let ((l (subseq (l seq) 0)))
        (rotatef (nth n l) (nth m l))
        (list (sequent l (r seq))))
      (error "")))

(defun pr (seq n m)
  (if (> (length-r seq) (max m n))
      (let ((r (subseq (r seq) 0)))
        (rotatef (nth n r) (nth m r))
        (list (sequent (l seq) r)))
      (error "")))
