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

;;;
;;; Generic pool
;;;
(defclass pool (reverse-environment)
  ((freelist :accessor env-freelist :type list :initarg :freelist))
  (:default-initargs :freelist nil))

(define-condition allocation-condition (condition) ())
(define-condition allocation-error (allocation-condition error) ())
(define-simple-error allocation-error)

(defmethod copy-environment-to :after ((to pool) (from pool))
  (setf (env-freelist to) (copy-list (env-freelist from))))

(defun make-pool (set)
  "Pretending someone needs that?"
  (make-instance 'pool :freelist set))

(defgeneric pool-allocate (env name)
  (:method ((o pool) name)
    (if-let ((free (pop (the list (env-freelist o)))))
      (prog1 free
        (bind o name free))
      (allocation-error "~@<~S has been drained.~:@>" o))))

(defgeneric pool-release (env value)
  (:method ((o pool) value)
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
                (mapc (curry #'pool-allocate ,pool) ,allocation) 
                ,@body)
           (mapcar (curry #'unbind ,pool) ,allocation)
           (setf (the list (env-freelist ,pool)) ,pre-alloc-freelist))))))

;;;
;;; Pool-backed frame chain
;;;
(defclass pool-backed-frame-chain (frame-chain)
  ((pool :accessor env-pool :type pool :initarg :pool)))

(defun make-pool-backed-frame-chain (set)
  (make-instance 'pool-backed-frame-chain :pool (make-pool set)))

(defmethod copy-environment-to :after ((to pool-backed-frame-chain) (from pool-backed-frame-chain))
  (setf (env-pool to) (copy-environment (env-pool from))))

(defgeneric pool-evaluate (env name)
  (:method ((o pool-backed-frame-chain) (name symbol))
    (do-lookup (env-pool o) (if-let ((frame (find-frame o name)))
                              (lookup frame name)
                              name))))

(defmacro with-pool-subset ((env &rest bound-set) &body body)
  "Execute BODY within context established by the most recently entered
WITH-ALLOCATOR form with NAME. The established context is used to determine
the result of EVAL-ALLOCATED form evaluations."
  (multiple-value-bind (decls body) (destructure-binding-form-body body)
    (let* ((special-vars (apply #'append (mapcar #'rest (remove 'special decls :key #'car :test-not #'eq))))
           (ordinary-vars (set-difference bound-set special-vars)))
      (when-let ((unknown-vars (set-difference special-vars bound-set)))
        (environment-error "~@<Unknown variables were declared special: ~S~:@>~%" unknown-vars))
      (with-gensyms (bottom-frame specials renames)
        (once-only (env)
          `(with-fresh-frame (,env ,bottom-frame)
             (let* ((,specials ',special-vars)
                    (,renames ',(make-gensym-list (length ordinary-vars) "W-P-F-B")))
               (with-pool-allocation ((env-pool ,env) (append ,specials ,renames))
                 (mapcar (curry #'bind ,bottom-frame) ',ordinary-vars ,renames)
                 ,@body))))))))

(defun pool-allocate-binding (pool-env name)
  "Bind NAME to a freshly allocated POOL-ENV element in the the
most recently established POOL-ENV's frame."
  (let ((rename (gensym "POOL-ALLOC-BIND")))
    (values (prog1 (pool-allocate (env-pool pool-env) rename)
              (bind (bottom-frame pool-env) name rename))
            rename)))
