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

(defclass top-level-environment (hash-table-environment)
  ((lexical-frames :accessor env-lexical-frames :type list :initarg :lexical-frames))
  (:default-initargs
   :lexical-frames nil))

(defclass reverse-environment (hash-table-environment)
  ((reverse-mapping :accessor env-reverse-mapping :type hash-table :initarg :reverse-mapping))
  (:default-initargs :reverse-mapping (make-hash-table :test 'equal)))

(defclass lexical-environment (alist-environment) ())

;;;
;;; Conditions
;;;
(define-condition environment-condition (condition) 
  ((env :accessor condition-env :initarg :env)))
(define-condition environment-error (environment-condition error) ())
(define-simple-error environment-error)
(define-reported-condition environment-name-already-bound (environment-condition cell-error) ()
  (:report (env name) "~@<Name ~A already bound in ~A~:@>" name env))
(define-reported-condition environment-name-not-bound (environment-condition cell-error) ()
  (:report (env name) "~@<Name ~A not bound in ~A~:@>" name env))
(define-reported-condition environment-value-not-bound (environment-condition cell-error) ()
  (:report (env name) "~@<Value ~A not bound in ~A~:@>" name env))

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

(defgeneric value (environment name)
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

(defsetf value set-value)

(defgeneric env-alist (environment)
  (:method ((o alist-environment))
    (copy-list (env-mapping o)))
  (:method ((o hash-table-environment))
    (hash-table-alist (env-mapping o))))

;;;
;;; Top-level environment
;;;
(defgeneric evaluate (environment name)
  (:method ((o top-level-environment) (name symbol))
    (value o (if (name-lexical-p o name) (lexical o name) name))))

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

(defgeneric name (environment value)
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
    (declare (special *metaenv*))
    (bind *metaenv* name env)))

(defgeneric release-environment (environment)
  (:method ((o environment))
    (declare (special *metaenv*))
    (unbind-by-value *metaenv* o)))

(defun find-environment (name)
  (declare (special *metaenv*))
  (value *metaenv* name))

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
;;; Lexicals
;;;
(defgeneric name-lexical-p (env name)
  (:method ((o top-level-environment) (name symbol))
    (when-let ((lex-frame (first (env-lexical-frames o))))
      (name-bound-p lex-frame name))))

(defmacro with-fresh-lexical-frame (env &body body)
  (with-gensyms (old-lexical-frame-list new-lexical-frame)
    (once-only (env)
      `(let ((,old-lexical-frame-list (env-lexical-frames ,env)))
         (unwind-protect
              (let ((,new-lexical-frame (make-instance 'lexical-environment)))
                (push ,new-lexical-frame (env-lexical-frames ,env))
                ,@body)
           (setf (env-lexical-frames ,env) ,old-lexical-frame-list))))))

(defgeneric allocate-lexical (env name)
  (:method ((o top-level-environment) (name symbol))
    (if-let ((lex-frame (first (env-lexical-frames o))))
      (bind lex-frame name nil)
      (environment-error "~@<Cannot allocate with no lexical frame established.~:@>"))))

(defgeneric undo-lexical (env name)
  (:method ((o top-level-environment) (name symbol))
    (if-let ((lex-frame (first (env-lexical-frames o))))
      (unbind lex-frame name)
      (environment-error "~@<Cannot undo a lexical with no lexical frame established.~:@>"))))

(defgeneric lexical (env name)
  (:method ((o top-level-environment) (name symbol))
    (if-let ((lex-frame (first (env-lexical-frames o))))
      (value lex-frame name)
      (environment-error "~@<Cannot evaluate a lexical with no lexical frame established.~:@>"))))

(defgeneric set-lexical (env name value)
  (:method ((o top-level-environment) (name symbol) value)
    (if-let ((lex-frame (first (env-lexical-frames o))))
      (bind lex-frame name value)
      (environment-error "~@<Cannot set a lexical with no lexical frame established.~:@>"))))

(defsetf lexical set-lexical)

(defmacro with-lexical-frame-bindings ((env &rest bound-set) (specials lexical-renames) &body body)
  "Execute BODY within context established by ENV, which is used to
determine the result of EVAL-ALLOCATED form evaluations."
  (multiple-value-bind (decls body) (destructure-binding-form-body body)
    (let* ((special-vars (apply #'append (mapcar #'rest (remove 'special decls :key #'car :test-not #'eq))))
           (lexical-vars (set-difference bound-set special-vars)))
      (when-let ((unknown-vars (set-difference special-vars bound-set)))
        (environment-error "~@<Unknown variables were declared special: ~S~:@>~%" unknown-vars))
      `(with-fresh-lexical-frame ,env
         (let* ((,specials ',specials)
                (,lexical-renames ',(make-gensym-list (length lexical-vars) "LEXICAL-LET")))
           (mapcar (curry #'set-lexical ,env) ',lexical-vars ,lexical-renames)
           ,@body)))))

(defgeneric allocate-lexical-binding (env name)
  (:method ((o top-level-environment) (name symbol))
    (lret ((rename (gensym "ALLOC-LEX-BIND")))
      (set-lexical o name rename))))
