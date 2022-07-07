(defpackage :claudia-test/rewrite
  (:use :cl
        :claudia/meta-data/interface
        :claudia/pattern/interface)
  (:export :test))
(in-package :claudia-test/rewrite)

(defun test ()
  (let* ((== (const '==))
         (zero (const 'zero))
         (plus (const 'plus))
         (succ (const 'succ))
         (succ0 (rule (x) (plus x zero) x))
         (succ1 (rule (x y) (plus x (succ y)) (succ (plus x y))))
         (target (formulas (== (succ (succ zero)) (plus (succ zero) (succ zero))))))
    (setf target (rewrite target succ1))
    (setf target (rewrite target succ0))
    (format t "~A~%" target)))
