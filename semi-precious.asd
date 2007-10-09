(defpackage :semi-precious.system
  (:use :cl :asdf))

(in-package :semi-precious.system)

(defsystem :semi-precious
  :depends-on (:alexandria)
  :components
  ((:file "depsolver")
   (:file "discrimination")
   (:file "octree-1d")
   (:file "state")))
