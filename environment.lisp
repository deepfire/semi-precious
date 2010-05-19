;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ENVIRONMENT; Base: 10 -*-
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

(in-package :environment)

(defclass environment () ())

(defclass alist-environment (environment)
  ((mapping :accessor env-mapping :type list :initarg :mapping))
  (:default-initargs :mapping nil))

(defclass hash-table-environment (environment)
  ((mapping :accessor env-mapping :type hash-table :initarg :mapping))
  (:default-initargs :mapping (make-hash-table :test 'eq)))

(defclass frame-chain (environment)
  ((frames :accessor env-frames :type list :initarg :frames))
  (:default-initargs :frames nil))

(defclass meta-environment (alist-environment) ())

(defclass immutable-environment (environment) ())

(defclass frame (alist-environment) ())
(defclass immutable-frame (frame immutable-environment) ())

(defclass reverse-environment (hash-table-environment)
  ((reverse-mapping :accessor env-reverse-mapping :type hash-table :initarg :reverse-mapping))
  (:default-initargs :reverse-mapping (make-hash-table :test 'equal)))

;;;
;;; Conditions
;;;
(define-condition environment-condition (condition) 
  ((env :accessor condition-env :initarg :env)))
(define-condition environment-error (environment-condition error) ())
(define-simple-error environment-error)
(define-reported-condition environment-name-already-bound (environment-condition cell-error) ()
  (:report (env #+sbcl sb-kernel::name #+ecl si::name #+ccl ccl::name)
           "~@<Name ~A already bound in ~A~:@>" #+sbcl sb-kernel::name #+ecl si::name #+ccl ccl::name env))
(define-reported-condition environment-immutable (environment-condition cell-error) ()
  (:report (env #+sbcl sb-kernel::name #+ecl si::name #+ccl ccl::name)
           "~@<Attempt to set value of name ~A in immutable environment ~A~:@>" #+sbcl sb-kernel::name #+ecl si::name #+ccl ccl::name env))
(define-reported-condition environment-name-not-bound (environment-condition cell-error) ()
  (:report (env #+sbcl sb-kernel::name #+ecl si::name #+ccl ccl::name)
           "~@<Name ~A not bound in ~A~:@>" #+sbcl sb-kernel::name #+ecl si::name #+ccl ccl::name env))
(define-reported-condition environment-value-not-bound (environment-condition cell-error) ()
  (:report (env #+sbcl sb-kernel::name #+ecl si::name #+ccl ccl::name)
           "~@<Value ~A not bound in ~A~:@>" #+sbcl sb-kernel::name #+ecl si::name #+ccl ccl::name env))
(define-reported-condition environment-frame-chain-empty (environment-condition) ()
  (:report (env)
           "~@<No frames established in ~S.~:@>" env))

;;;
;;; Generic
;;;
(defgeneric do-lookup (environment name)
  (:method ((o alist-environment) (name symbol))
    (when-let ((binding (assoc name (env-mapping o))))
      (values (cdr binding) t)))
  (:method ((o hash-table-environment) (name symbol))
    (gethash name (env-mapping o)))
  (:method ((o frame-chain) (name symbol))
    (when-let ((frame (find-frame o name)))
      (values (lookup frame name) t))))

(defgeneric lookup (environment name)
  (:method ((o environment) (name symbol))
    (multiple-value-bind (value boundp) (do-lookup o name)
      (if boundp 
          value
          (error 'environment-name-not-bound :env o :name name)))))

(defgeneric name-bound-p (environment name)
  (:method ((o environment) (name symbol))
    (nth-value 1 (do-lookup o name))))

(defgeneric bind (environment name value)
  (:documentation
   "Note the subtle difference between alist and hash-table environments.")
  (:method ((o alist-environment) (name symbol) value)
    (push (cons name value) (env-mapping o)))
  (:method ((o hash-table-environment) (name symbol) value)
    (setf (gethash name (env-mapping o)) value))
  (:method ((o frame-chain) (name symbol) value)
    (bind (or (find-frame o name) (bottom-frame o)) name value))
  (:method :after ((o reverse-environment) (name symbol) value)
    (setf (gethash value (env-reverse-mapping o)) name)))

(defgeneric do-unbind (environment name)
  (:documentation
   "Note how this isn't specified for frame chains.")
  (:method ((o alist-environment) (name symbol))
    (removef (env-mapping o) name :key #'car :count 1))
  (:method ((o hash-table-environment) (name symbol))
    (remhash name (env-mapping o)))
  (:method :before ((o reverse-environment) (name symbol))
    (remhash (gethash name (env-reverse-mapping o)) (env-mapping o))))

(defgeneric unbind (environment name)
  (:documentation
   "Note how this would fail for frame chains.")
  (:method :around ((o environment) (name symbol))
    (if (name-bound-p o name)
        (call-next-method)
        (error 'environment-name-not-bound :env o :name name)))
  (:method ((o environment) (name symbol))
    (do-unbind o name)))

(defgeneric set-value (environment name value)
  (:documentation
   "Change an already existing binding of NAME in ENVIRONMENT.")
  (:method :around ((o environment) (name symbol) value)
    (declare (ignore value))
    (if (name-bound-p o name)
        (call-next-method)
        (error 'environment-name-not-bound :env o :name name)))
  (:method ((o alist-environment) (name symbol) value)
    (setf (cdr (assoc name (env-mapping o))) value))
  (:method ((o hash-table-environment) (name symbol) value)
    (setf (gethash name (env-mapping o)) value))
  (:method :around ((o immutable-environment) (name symbol) value)
    (declare (ignore value))
    (error 'environment-immutable :env o :name name)))

(defsetf lookup set-value)

(defgeneric env-alist (environment)
  (:method ((o alist-environment))
    (copy-list (env-mapping o)))
  (:method ((o hash-table-environment))
    (hash-table-alist (env-mapping o))))

(defgeneric map-environment (environment fn)
  (:method ((o alist-environment) (fn function))
    (dolist (assoc (env-mapping o))
      (funcall fn (car assoc) (cdr assoc))))
  (:method ((o hash-table-environment) (fn function))
    (maphash fn (env-mapping o))))

(defgeneric copy-environment-to (to from)
  (:method ((to alist-environment) (from alist-environment))
    (setf (env-mapping to) (copy-alist (env-mapping from))))
  (:method ((to hash-table-environment) (from hash-table-environment))
    (setf (env-mapping to) (copy-hash-table (env-mapping from))))
  (:method ((to frame-chain) (from frame-chain))
    (setf (env-frames to) (mapcar #'copy-environment (env-frames from))))
  (:method :after ((to reverse-environment) (from reverse-environment))
    (setf (env-reverse-mapping to) (copy-hash-table (env-reverse-mapping from)))))

(defgeneric copy-environment (env)
  (:method ((o environment))
    (lret ((copy (make-instance (class-of o))))
      (copy-environment-to copy o))))

;;;
;;; Reverse-mapped access
;;;
(defgeneric unbind-by-value (environment value)
  ;; slow, iterative methods for generic
  (:method ((o hash-table-environment) (value symbol))
    (unless (iter (for (name iterval) in-hashtable (env-mapping o))
                  (when (equal value iterval)
                    (return (do-unbind o name))))
      (error 'environment-name-not-bound :env o :name value)))
  (:method :around ((o alist-environment) (value symbol))
    (unless (find value (env-mapping o) :key #'cdr :test #'equal)
      (error 'environment-name-not-bound :env o :name value)))
  (:method ((o alist-environment) value)
    (remove value (env-mapping o) :key #'cdr :test #'equal))
  (:method ((o reverse-environment) value)
    (multiple-value-bind (name bound-p) (gethash value (env-reverse-mapping o))
      (unless bound-p
        (error 'environment-name-not-bound :env o :name value))
      (remhash name (env-mapping o))
      (remhash value (env-reverse-mapping o)))))

(defgeneric lookup-name (environment value)
  (:method ((o reverse-environment) value)
    (multiple-value-bind (name bound-p) (gethash value (env-reverse-mapping o))
      (unless bound-p
        (error 'environment-name-not-bound :env o :name value))
      name)))

;;;
;;; Metaenvironments
;;;
(defvar *metaenv*)

(defmacro with-metaenvironment (&body body)
  `(let* ((*metaenv* (make-instance 'meta-environment)))
     (declare (special *metaenv*))
     ,@body))

(defgeneric establish-environment (name environment)
  (:method ((name symbol) (env environment))
    (bind *metaenv* name env)))

(defgeneric release-environment (environment)
  (:method ((o environment))
    (unbind-by-value *metaenv* o)))

(defun find-environment (name)
  (lookup *metaenv* name))

(defmacro with-environment ((name environment) &body body)
  (with-gensyms (old-environment-alist)
    (once-only (environment)
      `(let ((,old-environment-alist (env-mapping *metaenv*)))
         (unwind-protect
              (progn
                (establish-environment ,name ,environment)
                ,@body)
           (setf (env-mapping *metaenv*) ,old-environment-alist))))))

;;;
;;; Frame chains
;;;
(defmacro with-fresh-frame (env-and-params &body body)
  (with-gensyms (old-frame-list)
    (destructuring-bind (env &optional (frame (gensym))) (ensure-cons env-and-params)
      (once-only (env)
        `(let ((,old-frame-list (env-frames ,env)))
           (unwind-protect
                (let ((,frame (make-instance 'frame)))
                  (push ,frame (env-frames ,env))
                  ,@body)
             (setf (env-frames ,env) ,old-frame-list)))))))

(defmacro do-frame-bindings ((name value) frame &body body)
  `(iter (for (,name . ,value) in (env-alist ,frame))
         ,@body))

(defun bottom-frame (env)
  (if-let ((frame (first (env-frames env))))
    frame
    (environment-error "~@<No frames established in ~S.~:@>" env)))

(defgeneric find-frame (env name)
  (:method ((o frame-chain) (name symbol))
    (find name (env-frames o) :test (order name-bound-p 1 0))))

(defmacro with-fresh-frame-bindings ((env-and-params &rest bound-set) &body body)
  (destructuring-bind (env &optional (frame (gensym))) (ensure-cons env-and-params)
    `(with-fresh-frame (,env ,frame)
       (mapcar (curry #'bind ,frame) ',bound-set ',(make-list (length bound-set)))
       ,@body)))
