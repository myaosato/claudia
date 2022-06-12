(defpackage :claudia/pprint
  (:use :cl)
  (:export :print-claudia-print-dispatch
           :def-claudia-print))
(in-package :claudia/pprint)

(defparameter print-claudia-print-dispatch (copy-pprint-dispatch nil))
(defmacro def-claudia-print ((class stream obj) &body body)
  `(set-pprint-dispatch ,class 
                        (lambda (,stream ,obj)
                          (let ((*print-pprint-dispatch* print-claudia-print-dispatch))
                            ,@body))
                        0 print-claudia-print-dispatch))
