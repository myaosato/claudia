(defpackage :claudia/print/interface
  (:use :cl)
  (:import-from :claudia/print/pprint
                :print-claudia-print-dispatch)
  (:import-from :claudia/print/print-object) ;; for just loading
  (:export :print-claudia-print-dispatch))
(in-package :claudia/print/interface)
