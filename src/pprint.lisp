(defpackage :logic/pprint
  (:use :cl)
  (:export :print-logic-print-dispatch
           :def-logic-print))
(in-package :logic/pprint)

(defparameter print-logic-print-dispatch (copy-pprint-dispatch nil))
(defmacro def-logic-print ((class stream obj) &body body)
  `(set-pprint-dispatch ,class 
                        (lambda (,stream ,obj)
                          (let ((*print-pprint-dispatch* print-logic-print-dispatch))
                            ,@body))
                        0 print-logic-print-dispatch))
