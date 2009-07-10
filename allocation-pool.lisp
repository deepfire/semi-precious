;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ALLOCATION-POOL; Base: 10 -*-
;;;
;;;  (c) copyright 2009 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :allocation-pool)

(defclass pool-environment (reverse-environment)
  ((freelist :accessor env-freelist :type list :initarg :freelist))
  (:default-initargs :freelist nil))

(defclass dynamic-pool-environment (dynamic-environment pool-environment) ())

(define-condition allocation-condition (condition) ())
(define-condition allocation-error (allocation-condition error) ())
(define-simple-error allocation-error)

(defun make-dynamic-pool (set)
  (make-instance 'dynamic-pool-environment :freelist set))

(defgeneric allocate (env name)
  (:method ((o pool-environment) name)
    (if-let ((free (pop (the list (env-freelist o)))))
      (prog1 free
        (bind o name free))
      (allocation-error "~@<~S has been drained.~:@>" o))))

(defgeneric release (env value)
  (:method ((o pool-environment) value)
    (unbind-by-value o value)
    (push value (the list (env-freelist o)))))

(defmacro with-pool-allocation ((pool allocation) &body body)
  (with-gensyms (count pre-alloc-freelist)
    (once-only (pool allocation)
      `(let ((,count (length ,allocation))
             (,pre-alloc-freelist (the list (env-freelist ,pool))))
         (unless (<= ,count (length ,pre-alloc-freelist))
           (allocation-error "~@<Not enough free elements in ~S to satisfy allocation request for ~S elements.~:@>" ,pool ,count))
         (unwind-protect
              (progn
                (mapc (curry #'allocate ,pool) ,allocation) 
                ,@body)
           (mapcar (curry #'unbind-by-value ,pool) ,allocation)
           (setf (the list (env-freelist ,pool)) ,pre-alloc-freelist))))))

(defmacro with-pool-subset ((env &rest bound-set) &body body)
  "Execute BODY within context established by the most recently entered
WITH-ALLOCATOR form with NAME. The established context is used to determine
the result of EVAL-ALLOCATED form evaluations."
  (multiple-value-bind (decls body) (destructure-binding-form-body body)
    (with-gensyms (specials dynamic-renames)
      (once-only (env)
        `(with-dynamic-frame-bindings (,env ,@bound-set) (,specials ,dynamic-renames)
           ,@(when decls `((declare ,@decls)))
           (with-pool-allocation (,env (append ,specials ,dynamic-renames))
             ,@body))))))

(defun pool-allocate-dynamic (env name)
  "Allocate a dynamic NAME in the the most recently entered ALLOCATE-LET
dynamic pool, and a corresponding backing key from the global allocatable
value pool established by the most recently entered WITH-ALLOCATOR form
with NAME."
  (allocate env (allocate-dynamic-binding env name)))
