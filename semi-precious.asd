;;; -*- Mode: Lisp -*-

(defpackage :semi-precious.system
  (:use :cl :asdf))

(in-package :semi-precious.system)

(defsystem :semi-precious
  :depends-on (:alexandria :pergamum :iterate)
  :components
  ((:file "depsolver")
   (:file "discrimination")
   (:file "octree-1d")
   (:file "dictionary")
   (:file "state")
   (:file "circular")))
