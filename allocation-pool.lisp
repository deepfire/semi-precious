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

(define-condition allocation-condition (condition) ())
(define-condition allocation-error (allocation-condition error) ())
(define-simple-error allocation-error)

(defun allocate (name key &aux
                 (free-list (format-symbol (symbol-package name) "*FREE-~AS*" name))
                 (busy-list (format-symbol (symbol-package name) "*BUSY-~AS*" name)))
  "Allocate a KEY - VALUE pair from the allocatable value pool established by
the most recently entered WITH-ALLOCATION-POOL form with NAME."
  (if-let ((free (pop (the list (symbol-value free-list)))))
    (prog1 free
      (push (cons key free) (the list (symbol-value busy-list))))
    (allocation-error "~@<Allocation pool ~S has been drained.~:@>" name)))

(defun release (name value &aux
                (free-list (format-symbol (symbol-package name) "*FREE-~AS*" name))
                (busy-list (format-symbol (symbol-package name) "*BUSY-~AS*" name)))
  "Release the VALUE back into the allocatable value pool established by 
the most recently entered WITH-ALLOCATION-POOL form with NAME."
  (if (member value (the list (symbol-value busy-list)) :key #'cdr)
      (progn
        (push value (the list (symbol-value free-list)))
        (removef (the list (symbol-value busy-list)) value :key #'cdr))
      (allocation-error "~@<~S was not allocated in the ~A allocation pool.~:@>" value name)))

(defmacro with-allocator ((name set) &body body)
  "Execute BODY in a context, where ALLOCATE, RELEASE and EVAL-ALLOCATABLE
provide access to the NAME'd value allocation pool operating on SET."
  (let ((free-sym (format-symbol (symbol-package name) "*FREE-~AS*" name))
        (busy-sym (format-symbol (symbol-package name) "*BUSY-~AS*" name)))
    `(let* ((,free-sym ,set)
            (,busy-sym nil))
       (declare (special ,free-sym ,busy-sym) (type list ,free-sym ,busy-sym))
       ,@body)))

(defmacro allocate-let ((&rest (&optional name &rest bound-set)) &body body)
  "Execute BODY within context established by the most recently entered
WITH-ALLOCATOR form with NAME. The established context is used to determine
the result of EVAL-ALLOCATED form evaluations."
  (when (and bound-set (not name))
    (allocation-error "~@<Requested to bind allocatables with no pool specified.~:@>"))
  (multiple-value-bind (decls body) (destructure-binding-form-body body)
    (let* ((specials (apply #'append (mapcar #'rest (remove 'special decls :test-not #'eq))))
           (lexicals (set-difference bound-set specials)))
      (when-let ((unknown-vars (set-difference specials bound-set)))
        (allocation-error "~@<ALLOCATE-LET:~; Unknown variables were declared special: ~S~:@>~%" unknown-vars))
      (with-gensyms (allocation)
        (once-only (name)
          `(let* ((*lexicals* ',(mapcar #'cons lexicals (make-gensym-list (length lexicals) "ALLOCATE-LET")))
                  (,allocation (append (mapcar (compose (curry #'allocate ,name) #'cdr) *lexicals*)
                                       (mapcar (curry #'allocate ,name) ',specials))))
             (declare (special *lexicals*))
             (unwind-protect (progn ,@body)
               (mapcar (curry #'release ,name) ,allocation))))))))

(defun eval-allocated (name allocated &aux (busy-list (format-symbol (symbol-package name) "*BUSY-~AS*" name)))
  "Evaluate ALLOCATED in the context established by the most recently
entered WITH-ALLOCATOR form with NAME."
  (declare (special *lexicals*))
  (labels ((eval-cell (key)
             (if-let ((association (assoc key (the list (symbol-value busy-list)))))
               (cdr association)
               (allocation-error "~@<~A/~A was not allocated in the ~A allocation pool.~:@>" allocated key name)))
           (eval-maybe-lexical (allocated)
             (or (cdr (assoc allocated *lexicals*)) allocated)))
    (eval-cell (eval-maybe-lexical allocated))))
