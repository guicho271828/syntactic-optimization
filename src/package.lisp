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
  (let ((list (make-list *len* :initial-element 0)))
    (mapcar #'fn
            (mapcar #'fn 
                    (mapcar #'fn list list list)
                    (mapcar #'fn list list list)
                    (mapcar #'fn list list list))
            (mapcar #'fn 
                    (mapcar #'fn list list list)
                    (mapcar #'fn list list list)
                    (mapcar #'fn list list list))
            (mapcar #'fn 
                    (mapcar #'fn list list list)
                    (mapcar #'fn list list list)
                    (mapcar #'fn list list list)))))

(setf *optimize-nested-mapcar* t)
(defun fn2 ()
  (let ((list (make-list *len* :initial-element 0)))
    (mapcar #'fn
            (mapcar #'fn 
                    (mapcar #'fn list list list)
                    (mapcar #'fn list list list)
                    (mapcar #'fn list list list))
            (mapcar #'fn 
                    (mapcar #'fn list list list)
                    (mapcar #'fn list list list)
                    (mapcar #'fn list list list))
            (mapcar #'fn 
                    (mapcar #'fn list list list)
                    (mapcar #'fn list list list)
                    (mapcar #'fn list list list)))))

(time (fn1))
;; Evaluation took:
;;   0.150 seconds of real time
;;   0.144000 seconds of total run time (0.144000 user, 0.000000 system)
;;   [ Run times consist of 0.020 seconds GC time, and 0.124 seconds non-GC time. ]
;;   96.00% CPU
;;   452,522,933 processor cycles
;;   22,413,312 bytes consed
(time (fn2))
;; Evaluation took:
;;   0.126 seconds of real time
;;   0.124000 seconds of total run time (0.124000 user, 0.000000 system)
;;   98.41% CPU
;;   378,495,072 processor cycles
;;   3,211,264 bytes consed

(defvar *optimize-nested-map* nil)

(sb-ext:without-package-locks
  (define-compiler-macro map (&whole whole result-type function list &rest more-lists)
    ;; (map fn1 args1* (map fn2 args2*) args3*)
    ;; == (map (lambda (args1* args2* args3*)
    ;;              (fn1 args1* (fn2 args2*) args3*))
    ;;            args1* args2* args3*)
    (if (and result-type (constantp result-type) *optimize-nested-map*)
        (optimize-nested-map whole result-type function (cons list more-lists))
        whole)))

(defun optimize-nested-map (whole result-type function args)
  (iter (for arg in args)
        (with matched = nil)
        (match arg
          (`(map nil ,@_)
            (return-from optimize-nested-map whole))
          (`(map ,(guard result-type (constantp result-type)) ,fn2 ,@args2)
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
               `(map ,result-type (lambda ,lambda-args (funcall ,function ,@inner)) ,@outer)
               whole)))))

(setf *optimize-nested-map* nil)
(defun fn3 ()
  (let ((list (make-list *len* :initial-element 0)))
    (map 'vector #'fn
         (map 'vector #'fn 
              (map 'vector #'fn list list list)
              (map 'vector #'fn list list list)
              (map 'vector #'fn list list list))
         (map 'vector #'fn 
              (map 'vector #'fn list list list)
              (map 'vector #'fn list list list)
              (map 'vector #'fn list list list))
         (map 'vector #'fn 
              (map 'vector #'fn list list list)
              (map 'vector #'fn list list list)
              (map 'vector #'fn list list list)))))

(setf *optimize-nested-map* t)
(defun fn4 ()
  (let ((list (make-list *len* :initial-element 0)))
    (map 'vector #'fn
         (map 'vector #'fn 
              (map 'vector #'fn list list list)
              (map 'vector #'fn list list list)
              (map 'vector #'fn list list list))
         (map 'vector #'fn 
              (map 'vector #'fn list list list)
              (map 'vector #'fn list list list)
              (map 'vector #'fn list list list))
         (map 'vector #'fn 
              (map 'vector #'fn list list list)
              (map 'vector #'fn list list list)
              (map 'vector #'fn list list list)))))

(time (fn3))
;; Evaluation took:
;;   0.135 seconds of real time
;;   0.132000 seconds of total run time (0.132000 user, 0.000000 system)
;;   97.78% CPU
;;   404,698,822 processor cycles
;;   11,999,696 bytes consed
(time (fn4))
;; Evaluation took:
;;   0.133 seconds of real time
;;   0.136000 seconds of total run time (0.136000 user, 0.000000 system)
;;   102.26% CPU
;;   399,812,344 processor cycles
;;   2,405,648 bytes consed
