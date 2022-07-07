(defsystem "claudia-test"
  :class :package-inferred-system
  :depends-on ("claudia-test/all")
  :pathname "test/"
  :license "mit"
  :author "myaosato"
  :perform (test-op (op sys) (uiop:symbol-call :claudia-test/all :test)))
