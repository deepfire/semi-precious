;;; -*- Mode: Lisp -*-

(defpackage :semi-precious.system
  (:use :cl :asdf))

(in-package :semi-precious.system)

#+sbcl (require :sb-introspect)

(defsystem :semi-precious
  :depends-on (:alexandria :pergamum :iterate)
  :components
  ((:file "packages")
;;;
   (:file "environment" :depends-on ("packages"))
   (:file "depsolver" :depends-on ("packages"))
   (:file "discrimination" :depends-on ("packages"))
   (:file "octree-1d" :depends-on ("packages"))
   (:file "dictionary" :depends-on ("packages"))
   (:file "state" :depends-on ("packages"))
   (:file "circular" :depends-on ("packages"))
   (:file "early-eval" :depends-on ("packages"))
   (:file "meta" :depends-on ("packages"))
;;;
   (:file "allocation-pool" :depends-on ("environment"))))
