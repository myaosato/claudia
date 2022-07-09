(defpackage :claudia/command
  (:use :cl
        :claudia/environment
        :claudia/goal)
  (:import-from :claudia/sequent)
  (:import-from :claudia/lk)
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
           :pl :pr
           :rewrite-l :rewrite-r))
(in-package :claudia/command)

(defun app-a (axiom &optional (n 0))
  (app current-goal n #'claudia/lk:app-a axiom))

(defun id (&optional (n 0))
  (app current-goal n #'claudia/lk:id))

(defun cut (formula &optional (n 0))
  (app current-goal n #'claudia/lk:cut formula))

(defun and-l (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:and-l m))

(defun and-r (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:and-r m))

(defun or-l (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:or-l m))

(defun or-r (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:or-r m))

(defun not-l (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:not-l m))

(defun not-r (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:not-r m))

(defun to-l (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:to-l m))

(defun to-r (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:to-r m))

(defun forall-l (term &optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:forall-l term m))

(defun forall-r (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:forall-r m))

(defun exists-l (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:exists-l m))

(defun exists-r (term &optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:exists-r term m))

(defun wl (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:wl m))

(defun wr (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:wr m))

(defun cl (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:cl m))

(defun cr (&optional (n 0) (m 0))
  (app current-goal n #'claudia/lk:cr m))

(defun pl (&optional (n 0) (m 0) (l 1))
  (app current-goal n #'claudia/lk:pl m l))

(defun pr (&optional (n 0) (m 0) (l 1))
  (app current-goal n #'claudia/lk:pr m l))

(defun rewrite-l (rule &optional (n 0) (m 0))
  (app current-goal n #'claudia/sequent:rewrite-l m rule))

(defun rewrite-r (rule &optional (n 0) (m 0))
  (app current-goal n #'claudia/sequent:rewrite-r m rule))
