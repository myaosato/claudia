(defpackage :claudia/lk
  (:use :cl
        :claudia/meta-data/interface
        :claudia/pattern/interface
        :claudia/sequent)
  (:export :app-a
           :id :cut
           :and-l :and-r
           :or-l :or-r
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

;; apply axiom
;; NOTE: admit axiom and finish proof
(defun app-a (seq axiom)
  (if (some (lambda (x) (match axiom x)) (r seq))
      nil ;; proved
      (error "")))

;; axiom of LK
;; ------
;; Γ,A,Δ  ⊢ Σ,A,Π
(defun id (seq)
  (if (and (> (length-l seq) 0)
           (> (length-r seq) 0)
           (some (lambda (lf) (some (lambda (rf) (== lf rf)) (r seq))) (l seq)))
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
;; A,B,Γ ⊢ Δ       Γ ⊢ A,Δ  Γ ⊢ B,Δ
;; ---------(∧L)   ----------------(∧R)
;; A∧B,Γ ⊢ Δ       Γ ⊢ A∧B,Δ
(defun and-l (seq n)
  (let ((focus (nth-l n seq)))
    (if (typep focus '∧)
        (list (sequent (replace-nth-l n (list (∧-0 focus) (∧-1 focus)) seq) (r seq)))
        (error "AND-L (~A) is not applicable to ~A" n seq))))

(defun and-r (seq n)
  (let ((focus (nth-r n seq)))
    (if (typep focus '∧)
        (list (sequent (l seq) (replace-nth-r n (list (∧-0 focus)) seq))
              (sequent (l seq) (replace-nth-r n (list (∧-1 focus)) seq)))
        (error "AND-R (~A) is not applicable to ~A" n seq))))

;; Or
;; Γ ⊢ A,B,Δ       A,Γ ⊢ Δ  B,Γ ⊢ Δ
;; ---------(∨R)   ----------------(∨L)
;; Γ ⊢ A∨B,Δ       A∨B,Γ ⊢ Δ 
(defun or-r (seq n)
  (let ((focus (nth-r n seq)))
    (if (typep focus '∨)
        (list (sequent (l seq) (replace-nth-r n (list (∨-0 focus) (∨-1 focus)) seq)))
        (error "OR-R (~A) is not applicable to ~A" n seq))))

(defun or-l (seq n)
  (let ((focus (nth-l n seq)))
    (if (typep focus '∨)
        (list (sequent (replace-nth-l n (list (∨-0 focus)) seq) (r seq))
              (sequent (replace-nth-l n (list (∨-1 focus)) seq) (r seq)))
        (error "OR-L (~A) is not applicable to ~A" n seq))))

;; Not
;; Γ ⊢ A,Δ       A,Γ ⊢ Δ
;; --------(¬L)  ---------(¬R)
;; ¬A,Γ ⊢ Δ      Γ ⊢ ¬A,Δ
(defun not-l (seq n)
  (let ((focus (nth-l n seq)))
    (if (typep focus '¬)
        (list (sequent (remove-nth-l n seq) (cons (¬-0 focus) (r seq))))
        (error "NOT-L (~A) is not applicable to ~A" n seq))))

(defun not-r (seq n)
  (let ((focus (nth-r n seq)))
    (if (typep focus '¬)
        (list (sequent (cons (¬-0 focus) (l seq)) (remove-nth-r n seq)))
        (error "NOT-R (~A) is not applicable to ~A" n seq))))

;; To
;; A,Γ ⊢ B,Δ      Γ ⊢ A,Δ  B,Γ ⊢ Δ
;; ---------(¬R)  ----------------(¬L)
;; Γ ⊢ A→B,Δ      A→B,Γ ⊢ Δ
(defun to-r (seq n)
  (let ((focus (nth-r n seq)))
    (if (typep focus '→)
        (list (sequent (cons (→-0 focus) (l seq)) (replace-nth-r n (list (→-1 focus)) seq)))
        (error "TO-R (~A) is not applicable to ~A" n seq))))

(defun to-l (seq n)
  (let ((focus (nth-l n seq)))
    (if (typep focus '→)
        (list (sequent (remove-nth-l n seq) (cons (→-0 focus) (r seq)))
              (sequent (replace-nth-l n (list (→-1 focus)) seq) (r seq)))
        (error "TO-L (~A) is not applicable to ~A" n seq))))

;; For all
;; A[t/x],Γ ⊢ Δ      Γ ⊢ A,Δ
;; ------------(∀L)  ---------(∀R)  
;; ∀xA,Γ ⊢ Δ         Γ ⊢ ∀xA,Δ
(defun forall-l (seq term n)
  (let ((focus (nth-l n seq)))
    (if (and (typep focus '∀)
             (<-able (∀-formula focus) (∀-var focus) term))
        (list (sequent (replace-nth-l n (list (<- (∀-formula focus) (∀-var focus) term)) seq) (r seq)))
        (error "FORALL-L (~A) is not applicable to ~A" n seq))))

(defun forall-r (seq n)
  (let ((focus (nth-r n seq)))
    (if (and (typep focus '∀)
             (notany (free-p (∀-var focus)) (l seq))
             (notany (free-p (∀-var focus)) (remove-nth-r n seq)))
        (list (sequent (l seq) (replace-nth-r n (list (∀-formula focus)) seq)))
        (error "FORALL-R (~A) is not applicable to ~A" n seq))))

;; Exist
;; A,Γ ⊢ Δ        Γ ⊢ A[t/x],Δ
;; ---------(∃L)  ------------(∃R)  
;; ∃xA,Γ ⊢ Δ      Γ ⊢ ∃xA,Δ
(defun exists-l (seq n)
  (let ((focus (nth-l n seq)))
    (if (and (typep focus '∃)
             (notany (free-p (∃-var focus)) (remove-nth-l n seq))
             (notany (free-p (∃-var focus)) (r seq)))
        (list (sequent (replace-nth-r n (list (∃-formula focus)) seq) (r seq)))
        (error "EXISTS-L (~A) is not applicable to ~A" n seq))))

(defun exists-r (seq term n)
  (let ((focus (nth-r n seq)))
    (if (and (typep focus '∃)
             (<-able (∃-formula focus) (∃-var focus) term))
        (list (sequent (l seq) (replace-nth-r n (list (<- (∃-formula focus) (∃-var focus) term)) seq)))
        (error "EXISTS-R (~A) is not applicable to ~A" n seq))))


;; Weakening
;; Γ ⊢ Δ         Γ ⊢ Δ
;; -------(WL)  ---------(WR)
;; A,Γ ⊢ Δ       Γ ⊢ A,Δ
(defun wl (seq n)
  (if (> (length-l seq) n)
      (list (sequent (remove-nth-l n seq) (r seq)))
      (error "WL (~A) is not applicable to ~A" n seq)))

(defun wr (seq n)
  (if (>= (length-r seq) 1)
      (list (sequent (l seq) (remove-nth-r n seq)))
      (error "WR (~A) is not applicable to ~A" n seq)))

;; Contraction
;; A,A,Γ ⊢ Δ      Γ ⊢ A,A,Δ
;; ---------(CL)  ---------(CR)
;; A,Γ ⊢ Δ        Γ ⊢ A,Δ
(defun cl (seq n)
  (if (>= (length-l seq) 1)
      (list (sequent (cons (nth-l n seq) (l seq)) (r seq)))
      (error "CL (~A) is not applicable to ~A" n seq)))

(defun cr (seq n)
  (if (>= (length-r seq) 1)
      (list (sequent (l seq) (cons (nth-r n seq) (r seq))))
      (error "CR (~A) is not applicable to ~A" n seq)))


;; Permutation
;; Γ0,..Γm,..Γn,.. ⊢ Δ      Γ ⊢ Δ0,..Δm,..Δn,..
;; -------------------(PL)  -------------------(CR)
;; Γ0,..Γn,..Γm,.. ⊢ Δ      Γ ⊢ Δ0,..Δn,..Δm,..
(defun pl (seq n m)
  (if (> (length-l seq) (max m n))
      (let ((l (subseq (l seq) 0)))
        (rotatef (nth n l) (nth m l))
        (list (sequent l (r seq))))
      (error "PL (~A ~A) is not applicable to ~A" n m seq)))

(defun pr (seq n m)
  (if (> (length-r seq) (max m n))
      (let ((r (subseq (r seq) 0)))
        (rotatef (nth n r) (nth m r))
        (list (sequent (l seq) r)))
      (error "PR (~A ~A) is not applicable to ~A" n m seq)))
