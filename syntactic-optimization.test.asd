#|
  This file is a part of syntactic-optimization project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage syntactic-optimization.test-asd
  (:use :cl :asdf))
(in-package :syntactic-optimization.test-asd)


(defsystem syntactic-optimization.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of syntactic-optimization"
  :license "LLGPL"
  :depends-on (:syntactic-optimization
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval
 (read-from-string
  "(5am:run! :syntactic-optimization)"))
))
