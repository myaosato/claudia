(defsystem "claudia"
  :class :package-inferred-system
  :depends-on ("claudia/claudia")
  :pathname "src/"
  :license "mit"
  :version "0.1.0"
  :author "myaosato"
  :in-order-to ((test-op (test-op "claudia-test"))))
