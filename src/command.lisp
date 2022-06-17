(defpackage :claudia/command
  (:use :cl
        :claudia/goal)
  (:import-from :claudia/lk)
  (:export :id :cut
           :and-l1 :and-l2 :and-r
           :or-l :or-r1 :or-r2
           :not-l :not-r
           :to-l :to-r
           :forall-l :forall-r
           :exists-l :exists-r
           :wl :wr
           :cl :cr
           :pl :pr))
(in-package :claudia/command)

(defun id (goal n)
  (app goal n #'claudia/lk:id))

(defun cut (goal n formula)
  (app goal n #'claudia/lk:cut formula))

(defun and-l1 (goal n &optional (m 0))
  (app goal n #'claudia/lk:and-l1 m))

(defun and-l2 (goal n &optional (m 0))
  (app goal n #'claudia/lk:and-l2 m))

(defun and-r (goal n &optional (m 0))
  (app goal n #'claudia/lk:and-r m))

(defun or-l (goal n &optional (m 0))
  (app goal n #'claudia/lk:or-l m))

(defun or-r1 (goal n &optional (m 0))
  (app goal n #'claudia/lk:or-r1 m))

(defun or-r2 (goal n &optional (m 0))
  (app goal n #'claudia/lk:or-r2 m))

(defun not-l (goal n &optional (m 0))
  (app goal n #'claudia/lk:not-l m))

(defun not-r (goal n &optional (m 0))
  (app goal n #'claudia/lk:not-r m))

(defun to-l (goal n &optional (m 0))
  (app goal n #'claudia/lk:to-l m))

(defun to-r (goal n &optional (m 0))
  (app goal n #'claudia/lk:to-r m))

(defun forall-l (goal n term &optional (m 0))
  (app goal n #'claudia/lk:forall-l term m))

(defun forall-r (goal n &optional (m 0))
  (app goal n #'claudia/lk:forall-r m))

(defun exists-l (goal n &optional (m 0))
  (app goal n #'claudia/lk:exists-l m))

(defun exists-r (goal n term &optional (m 0))
  (app goal n #'claudia/lk:exists-r term m))

(defun wl (goal n &optional (m 0))
  (app goal n #'claudia/lk:wl m))

(defun wr (goal n &optional (m 0))
  (app goal n #'claudia/lk:wr m))

(defun cl (goal n &optional (m 0))
  (app goal n #'claudia/lk:cl m))

(defun cr (goal n &optional (m 0))
  (app goal n #'claudia/lk:cr m))

(defun pl (goal n m l)
  (app goal n #'claudia/lk:pl m l))

(defun pr (goal n m l)
  (app goal n #'claudia/lk:pr m l))
