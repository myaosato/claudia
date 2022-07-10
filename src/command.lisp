(defpackage :claudia/command
  (:use :cl
        :claudia/environment
        :claudia/goal)
  (:import-from :claudia/sequent)
  (:import-from :claudia/lk)
  (:import-from :claudia/print/interface
                :print-claudia-print-dispatch)
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

(defmacro def-command (name rule &key (required nil) (optional nil))
  (let ((sequent-number (gensym)))
  `(defun ,name (,@required &optional (,sequent-number 0) ,@optional)
     (let* ((*print-pprint-dispatch* print-claudia-print-dispatch)
            (args (list ,@required ,@(mapcar #'car optional)))
            (new-goal (apply #'app current-goal ,sequent-number ,rule args)))
       (format t "~16,,,'-A [~A: ~A ~{~A~^ ~}]~%" "" ,sequent-number ',name args)
       (setf current-goal new-goal)
       (format t "~W~%" current-goal)))))

(def-command app-a #'claudia/lk:app-a :required (axiom))
(def-command id #'claudia/lk:id)
(def-command cut #'claudia/lk:cut :required (formula))
(def-command and-l #'claudia/lk:and-l :optional ((n 0)))
(def-command and-r #'claudia/lk:and-r :optional ((n 0)))
(def-command or-l #'claudia/lk:or-l :optional ((n 0)))
(def-command or-r #'claudia/lk:or-r :optional ((n 0)))
(def-command not-l #'claudia/lk:not-l :optional ((n 0)))
(def-command not-r #'claudia/lk:not-r :optional ((n 0)))
(def-command to-l #'claudia/lk:to-l :optional ((n 0)))
(def-command to-r #'claudia/lk:to-r :optional ((n 0)))
(def-command forall-l #'claudia/lk:forall-l :required (term) :optional ((n 0)))
(def-command forall-r #'claudia/lk:forall-r :optional ((n 0)))
(def-command exists-l #'claudia/lk:exists-l :optional ((n 0)))
(def-command exists-r #'claudia/lk:exists-r :required (term) :optional ((n 0)))
(def-command wl #'claudia/lk:wl :optional ((n 0)))
(def-command wr #'claudia/lk:wr :optional ((n 0)))
(def-command cl #'claudia/lk:cl :optional ((n 0)))
(def-command cr #'claudia/lk:cr :optional ((n 0)))
(def-command pl #'claudia/lk:pl :optional ((n 0) (m 1)))
(def-command pr #'claudia/lk:pr :optional ((n 0) (m 1)))
(def-command rewrite-l #'claudia/sequent:rewrite-l :required (rule) :optional ((n 0)))
(def-command rewrite-r #'claudia/sequent:rewrite-r :required (rule) :optional ((n 0)))
