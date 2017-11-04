
(in-package :cl-user)
(defpackage syntactic-optimization-asd
  (:use :cl :asdf))
(in-package :syntactic-optimization-asd)


(defsystem syntactic-optimization
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia :alexandria :iterate)
  :pathname "src"
  :components ((:file "package"))
  :description "provides a few compiler macros for syntactic optimizations e.g. (log (/ x 3) 3) -> (1- (log x 3))"
  :in-order-to ((test-op (test-op :syntactic-optimization.test))))
