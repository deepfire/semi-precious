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

(defclass environment ()
  ((mapping :accessor env-mapping :initarg :mapping)))

(defclass alist-environment (environment)
  ((mapping :type list))
  (:default-initargs :mapping nil))

(defclass hash-table-environment (environment)
  ((mapping :type hash-table))
  (:default-initargs :mapping (make-hash-table :test 'eq)))

(defclass meta-environment (alist-environment)
  ())

(defclass dynamic-environment (hash-table-environment)
  ((dynamic-frames :accessor env-dynamic-frames :type list :initarg :dynamic-frames))
  (:default-initargs
   :dynamic-frames nil))

(defclass reverse-environment (hash-table-environment)
  ((reverse-mapping :accessor env-reverse-mapping :type hash-table :initarg :reverse-mapping))
  (:default-initargs :reverse-mapping (make-hash-table :test 'equal)))

(defclass dynamic-environment (alist-environment) ())

;;;
;;; Conditions
;;;
(define-condition environment-condition (condition) 
  ((env :accessor condition-env :initarg :env)))
(define-condition environment-error (environment-condition error) ())
(define-simple-error environment-error)
(define-reported-condition environment-name-already-bound (environment-condition cell-error) ()
  (:report (env #+sbcl sb-kernel::name #+ecl si::name) "~@<Name ~A already bound in ~A~:@>" #+sbcl sb-kernel::name #+ecl si::name env))
(define-reported-condition environment-name-not-bound (environment-condition cell-error) ()
  (:report (env #+sbcl sb-kernel::name #+ecl si::name) "~@<Name ~A not bound in ~A~:@>" #+sbcl sb-kernel::name #+ecl si::name env))
(define-reported-condition environment-value-not-bound (environment-condition cell-error) ()
  (:report (env #+sbcl sb-kernel::name #+ecl si::name) "~@<Value ~A not bound in ~A~:@>" #+sbcl sb-kernel::name #+ecl si::name env))

;;;
;;; Generic
;;;
(defgeneric bind (environment name value)
  (:method ((o alist-environment) (name symbol) value)
    (push (cons name value) (env-mapping o)))
  (:method ((o hash-table-environment) (name symbol) value)
    (setf (gethash name (env-mapping o)) value))
  (:method :after ((o reverse-environment) (name symbol) value)
    (setf (gethash value (env-reverse-mapping o)) name)))

(defgeneric name-bound-p (environment name)
  (:method ((o alist-environment) (name symbol))
    (not (null (assoc name (env-mapping o)))))
  (:method ((o hash-table-environment) (name symbol))
    (nth-value 1 (gethash name (env-mapping o)))))

(defgeneric do-unbind (environment name)
  (:method ((o alist-environment) (name symbol))
    (removef (env-mapping o) name :key #'car :count 1))
  (:method ((o hash-table-environment) (name symbol))
    (remhash name (env-mapping o)))
  (:method :before ((o reverse-environment) (name symbol))
    (remhash (gethash name (env-reverse-mapping o)) (env-mapping o))))

(defgeneric unbind (environment name)
  (:method :around ((o environment) (name symbol))
    (if (name-bound-p o name)
        (call-next-method)
        (error 'environment-name-not-bound :env o :name name)))
  (:method ((o environment) (name symbol))
    (do-unbind o name)))

(defgeneric lookup-value (environment name)
  (:method :around ((o environment) (name symbol))
    (if (name-bound-p o name)
        (call-next-method)
        (error 'environment-name-not-bound :env o :name name)))
  (:method ((o alist-environment) (name symbol))
    (cdr (assoc name (env-mapping o))))
  (:method ((o hash-table-environment) (name symbol))
    (gethash name (env-mapping o))))

(defgeneric set-value (environment name value)
  (:method :around ((o environment) (name symbol) value)
    (if (name-bound-p o name)
        (call-next-method)
        (error 'environment-name-not-bound :env o :name name)))
  (:method ((o alist-environment) (name symbol) value)
    (setf (cdr (assoc name (env-mapping o))) value))
  (:method ((o hash-table-environment) (name symbol) value)
    (setf (gethash name (env-mapping o)) value)))

(defsetf lookup-value set-value)

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

(defgeneric copy-environment (to from)
  (:method ((to alist-environment) (from alist-environment))
    (setf (env-mapping to) (copy-alist (env-mapping from))))
  (:method ((to hash-table-environment) (from hash-table-environment))
    (setf (env-mapping to) (copy-hash-table (env-mapping from))))
  (:method :after ((to dynamic-environment) (from dynamic-environment))
    (setf (env-dynamic-frames to) (mapcar #'copy-environment (env-dynamic-frames from))))
  (:method :after ((to reverse-environment) (from reverse-environment))
    (setf (env-reverse-mapping to) (copy-hash-table (env-reverse-mapping from)))))

;;;
;;; Reverse-mapped access
;;;
(defgeneric unbind-by-value (environment value)
  ;; slow, iterative methods for generic
  (:method :around ((o hash-table-environment) (value symbol))
    (unless (iter (for (nil iterval) in-hashtable (env-mapping o))
                  (when (equal value iterval)
                    (return t)))
      (error 'environment-name-not-bound :env o :name value)))
  (:method ((o hash-table-environment) value)
    (let ((name (iter (for (name iterval) in-hashtable (env-mapping o))
                      (when (equal value iterval)
                        (return name)))))
      (remhash name (env-mapping o))))
  (:method :around ((o alist-environment) (value symbol))
    (unless (find value (env-mapping o) :key #'cdr :test #'equal)
      (error 'environment-name-not-bound :env o :name value)))
  (:method ((o alist-environment) value)
    (remove value (env-mapping o) :key #'cdr :test #'equal))
  (:method :around ((o reverse-environment) (value symbol))
    (unless (nth-value 1 (gethash value (env-reverse-mapping o)))
      (error 'environment-name-not-bound :env o :name value)))
  (:method ((o reverse-environment) value)
    (let ((name (gethash value (env-reverse-mapping o))))
      (remhash name (env-mapping o))
      (remhash value (env-reverse-mapping o)))))

(defgeneric lookup-name (environment value)
  (:method :around ((o reverse-environment) (value symbol))
    (unless (nth-value 1 (gethash value (env-reverse-mapping o)))
      (error 'environment-name-not-bound :env o :name value)))
  (:method ((o reverse-environment) value)
    (gethash value (env-reverse-mapping o))))

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
  (lookup-value *metaenv* name))

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
;;; Dynamic environment
;;;
(defmacro with-fresh-dynamic-frame (env &body body)
  (with-gensyms (old-dynamic-frame-list new-dynamic-frame)
    (once-only (env)
      `(let ((,old-dynamic-frame-list (env-dynamic-frames ,env)))
         (unwind-protect
              (let ((,new-dynamic-frame (make-instance 'dynamic-environment)))
                (push ,new-dynamic-frame (env-dynamic-frames ,env))
                ,@body)
           (setf (env-dynamic-frames ,env) ,old-dynamic-frame-list))))))

(defgeneric find-dynamic-frame (env name)
  (:method ((o dynamic-environment) (name symbol))
    (find name (env-dynamic-frames o) :key (order name-bound-p 1 0))))

(defgeneric name-dynamic-p (environment name)
  (:method ((o dynamic-environment) (name symbol))
    (not (null (find-dynamic-frame o name)))))

(defgeneric evaluate-dynamic (env name)
  (:method ((o dynamic-environment) (name symbol))
    (lookup-value (or (find-dynamic-frame o name) o) name)))

(defgeneric dynamic (env name)
  (:method ((o dynamic-environment) (name symbol))
    (if-let ((dyn-frame (first (env-dynamic-frames o))))
      (lookup-value dyn-frame name)
      (environment-error "~@<Cannot look up a dynamic with no dynamic frame established.~:@>"))))

(defgeneric set-dynamic (env name value)
  (:method ((o dynamic-environment) (name symbol) value)
    (if-let ((dyn-frame (first (env-dynamic-frames o))))
      (bind dyn-frame name nil)
      (environment-error "~@<Cannot bind a dynamic with no dynamic frame established.~:@>"))))

(defsetf dynamic set-dynamic)

(defgeneric allocate-dynamic (env name)
  (:method ((o dynamic-environment) (name symbol))
    (if-let ((dyn-frame (first (env-dynamic-frames o))))
      (bind dyn-frame name nil)
      (environment-error "~@<Cannot allocate with no dynamic frame established.~:@>"))))

(defgeneric undo-dynamic (env name)
  (:method ((o dynamic-environment) (name symbol))
    (if-let ((dyn-frame (first (env-dynamic-frames o))))
      (unbind dyn-frame name)
      (environment-error "~@<Cannot undo a dynamic with no dynamic frame established.~:@>"))))

(defmacro with-dynamic-frame-bindings ((env &rest bound-set) (specials dynamic-renames) &body body)
  "Execute BODY within context established by ENV, which is used to
determine the result of EVAL-ALLOCATED form evaluations."
  (multiple-value-bind (decls body) (destructure-binding-form-body body)
    (let* ((special-vars (apply #'append (mapcar #'rest (remove 'special decls :key #'car :test-not #'eq))))
           (dynamic-vars (set-difference bound-set special-vars)))
      (when-let ((unknown-vars (set-difference special-vars bound-set)))
        (environment-error "~@<Unknown variables were declared special: ~S~:@>~%" unknown-vars))
      `(with-fresh-dynamic-frame ,env
         (let* ((,specials ',special-vars)
                (,dynamic-renames ',(make-gensym-list (length dynamic-vars) "DYNAMIC-LET")))
           (mapcar (curry #'set-dynamic ,env) ',dynamic-vars ,dynamic-renames)
           ,@body)))))

(defgeneric allocate-dynamic-binding (env name)
  (:method ((o dynamic-environment) (name symbol))
    (lret ((rename (gensym "ALLOC-DYN-BIND")))
      (set-dynamic o name rename))))
