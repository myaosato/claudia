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

(defun make-unifier (pattern target)
  (labels ((%make-unifier (pattern target map)
             (cond ((typep pattern 'var)
                    (ref pattern target map))
                   ((typep pattern 'const)
                    (if (== pattern target)
                        map))
                   ((and (typep pattern 'func) (typep target 'func))
                    (let ((xs (func-terms pattern))
                          (ys (func-terms target)))
                      (if (and (= (length xs) (length ys))
                               (every (lambda (x y) (%make-unifier x y map)) xs ys))
                          map)))
                   (t
                    nil))))
    (%make-unifier pattern target (make-hash-table))))

(defun rewrite (pattern unifier)
  (declare (type term pattern))
  (cond ((typep pattern 'var)
         (gethash pattern unifier))
        ((typep pattern 'const)
         pattern)
        ((typep pattern 'func)
         (apply #'func (mapcar (lambda (x) (rewrite x unifier)) (func-terms pattern))))))
         
(defclass rule nil
  ((name :initarg :name :reader name)
   (before :initarg :before :reader before :type func)
   (after :initarg :after :reader after :type term)))
(defmethod print-object ((x rule) stream)
  (format stream "#<RULE ~A => ~A>" (before x) (after x)))

(defmacro rule (vars before after)
  `(let ,(mapcar (lambda (s) `(,s (var ',s))) vars)
     (make-instance 'rule :before (terms ,before) :after (terms ,after))))

(defun reduction (rule target)
  (let ((unifier (make-unifier (before rule) target)))
    (when unifier
      (rewrite (after rule) unifier))))
