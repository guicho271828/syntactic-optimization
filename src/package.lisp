#|
  This file is a part of syntactic-optimization project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage syntactic-optimization
  (:use :cl :trivia :alexandria :iterate))
(in-package :syntactic-optimization)

;; blah blah blah.

(named-readtables:in-readtable :fare-quasiquote)


(sb-ext:without-package-locks
  (define-compiler-macro log (&whole whole number &optional base)
    #|
    (log (/ x base) base) == (1- (log x base))
    (log (* x base) base) == (1+ (log x base))
    |#
    (match number
      (`(/ ,x ,(eql base))
        `(1- (log ,x ,base)))
      (`(* ,x ,(eql base))
        `(1+ (log ,x ,base)))
      (_
       whole))))


(defvar *optimize-nested-mapcar* nil)

(sb-ext:without-package-locks
  (define-compiler-macro mapcar (&whole whole function list &rest more-lists)
    ;; (mapcar fn1 args1* (mapcar fn2 args2*) args3*)
    ;; == (mapcar (lambda (args1* args2* args3*)
    ;;              (fn1 args1* (fn2 args2*) args3*))
    ;;            args1* args2* args3*)
    (if *optimize-nested-mapcar*
        (optimize-nested-mapcar whole function (cons list more-lists))
        whole)))

(defun optimize-nested-mapcar (whole function args)
  (iter (for arg in args)
        (with matched = nil)
        (match arg
          (`(mapcar ,fn2 ,@args2)
            (setf matched t)
            (let ((args-gs (make-gensym-list (length args2) "ARG")))
              (appending args-gs into lambda-args)
              (collecting `(funcall ,fn2 ,@args-gs) into inner)
              (appending args2 into outer)))
          (_
           (let ((args-gs (make-gensym "ARG")))
             (collecting args-gs into lambda-args)
             (collecting args-gs into inner)
             (collecting args into outer))))
        (finally
         (return
           (if matched
               `(mapcar (lambda ,lambda-args (funcall ,function ,@inner)) ,@outer)
               whole)))))

(defun fn (a b c)
  (+ (sin a) (cos b) (sin c)))

(defparameter *len* 100000)

(setf *optimize-nested-mapcar* nil)
(defun fn1 ()
  (mapcar #'fn
          (mapcar #'fn
                  (make-list *len* :initial-element 1)
                  (make-list *len* :initial-element 2)
                  (make-list *len* :initial-element 3))
          (mapcar #'fn
                  (make-list *len* :initial-element 4)
                  (make-list *len* :initial-element 5)
                  (make-list *len* :initial-element 6))
          (mapcar #'fn
                  (make-list *len* :initial-element 7)
                  (make-list *len* :initial-element 8)
                  (make-list *len* :initial-element 9))))

(setf *optimize-nested-mapcar* t)
(defun fn2 ()
  (mapcar #'fn
          (mapcar #'fn
                  (make-list *len* :initial-element 1)
                  (make-list *len* :initial-element 2)
                  (make-list *len* :initial-element 3))
          (mapcar #'fn
                  (make-list *len* :initial-element 4)
                  (make-list *len* :initial-element 5)
                  (make-list *len* :initial-element 6))
          (mapcar #'fn
                  (make-list *len* :initial-element 7)
                  (make-list *len* :initial-element 8)
                  (make-list *len* :initial-element 9))))

;; SYNTACTIC-OPTIMIZATION> (time (progn (fn1) nil))
;; Evaluation took:
;;   0.074 seconds of real time
;;   0.072000 seconds of total run time (0.072000 user, 0.000000 system)
;;   97.30% CPU
;;   222,508,480 processor cycles
;;   20,775,360 bytes consed
;;   
;; NIL
;; SYNTACTIC-OPTIMIZATION> (time (progn (fn2) nil))
;; Evaluation took:
;;   0.073 seconds of real time
;;   0.072000 seconds of total run time (0.072000 user, 0.000000 system)
;;   98.63% CPU
;;   216,970,028 processor cycles
;;   15,990,784 bytes consed
;;   
;; NIL
