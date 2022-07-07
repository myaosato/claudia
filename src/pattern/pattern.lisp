(defpackage :claudia/pattern/pattern
  (:use :cl
        :claudia/meta-data/interface)
  (:shadow :rewrite)
  (:export :rule
           :reduction))
(in-package :claudia/pattern/pattern)

(defun ref (var target map)
  (let ((x (gethash var map)))
    (cond ((null x)
           (setf (gethash var map) target)
           map)
          ((== x target)
           map)
          (t
           nil))))

(defun match (pattern target)
  (declare (type term pattern target))
  (labels ((%match (pattern target map)
             (cond ((typep pattern 'var)
                    (ref pattern target map))
                   ((typep pattern 'const)
                    (if (== pattern target)
                        map))
                   ((and (typep pattern 'func) (typep target 'func))
                    (let ((xs (func-terms pattern))
                          (ys (func-terms target)))
                      (if (and (= (length xs) (length ys))
                               (every (lambda (x y) (%match x y map)) xs ys))
                          map)))
                   (t
                    nil))))
    (%match pattern target (make-hash-table))))

(defun rewrite (pattern unifier)
  (declare (type term pattern))
  (cond ((typep pattern 'var)
         (gethash pattern unifier))
        ((typep pattern 'const)
         pattern)
        ((typep pattern 'func)
         (apply #'func (mapcar (lambda (x) (rewrite x unifier)) (func-terms pattern))))))
         
(defclass rule nil
  ((before :initarg :before :reader before :type func)
   (after :initarg :after :reader after :type term)))

(defmacro rule (before after)
  `(make-instance 'rule :before (terms ,before) :after (terms ,after)))

(defun reduction (rule target)
  (let ((unifier (match (before rule) target)))
    (when unifier
      (rewrite (after rule) unifier))))
