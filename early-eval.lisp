;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: EARLY-EVAL; Base: 10 -*-
;;;
;;;  (c) copyright 2007-2008 by
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


(in-package :early-eval)

(eval-when (:compile-toplevel :execute)
  (enable-curry-reader)
  (enable-compose-reader))

(defstruct (early-operator (:conc-name eop-))
  name evalspec associativity identity)

(defparameter *eops* (make-hash-table))

(defun eop (name)
  (if-let ((eop (gethash name *eops*)))
	  (values (eop-evalspec eop) (eop-associativity eop) (eop-identity eop))))

(defun defeop (name evalspec associativity identity)
  (setf (gethash name *eops*) (make-early-operator :name name :evalspec evalspec
						   :associativity associativity :identity identity)))

(mapc [apply #'defeop] '((dpb     (t nil t) nil 0)
                         (logior   t        t   0)
                         (logand   t        t   -1)
                         (logxor   t        t   0)
                         (logeqv   t        t   -1)
                         (lognot   t        nil nil)
                         (logandc1 t        nil nil)
                         (logandc2 t        nil nil)
                         (logiorc1 t        nil nil)
                         (logiorc2 t        nil nil)
                         (lognor   t        nil nil)
                         (lognand  t        nil nil)))

;; possibly makes a 1+-long in the abbreviated case
(defun decode-eval-mask (mask form &optional acc)
  (if (typep mask 'cons)
      (decode-eval-mask (cdr mask) (cdr form) (cons (car mask) acc))
      (nreverse (nconc (make-list (length form) :initial-element mask) acc))))

(defun atom-immediate-p (form)
  (or (typep form 'integer) (typep form 'boolean) (typep form 'keyword)))

(defun constant-p (form)
  (or (atom-immediate-p form)
      (quoted-p form)))

(defun immediate-p (form)
  (or (atom-immediate-p form)
      (and (consp form)
	   (if-let* ((espec (eop (car form))) (evalmask (decode-eval-mask espec form)))
		    (every #'or-p (mapcar #'immediate-p (cdr form)) (mapcar #'null evalmask))))))

(defstruct (environment (:conc-name env-))
  (bindings (make-hash-table) :type hash-table)
  (byte nil))

(defmethod make-load-form ((o environment) &optional env)
  (make-load-form-saving-slots o :environment env))

(defun env-empty-p (env)
  (zerop (hash-table-count (env-bindings env))))

(defun sym-value (env symbol)
  (declare (type symbol symbol) (type environment env))
  (multiple-value-bind (value exist-p) (gethash symbol (env-bindings env))
    (unless exist-p
      (error "~@<Symbol ~S is not bound in ~S.~:@>" symbol env))
    value))

(defun clamp-val-to-env (env val)
  (declare (type (or null environment) env) (type (unsigned-byte 32) val))
  (dpb val (env-byte env) 0))

(defun eval-atom (form mask env)
  (declare (type (or number boolean keyword) form) (type integer mask)
	   (type (or null environment) env))
  (etypecase form
    (number (logand mask (if env (clamp-val-to-env env form) form)))
    (null 0)
    ((eql t) mask)
    (keyword (logand mask (sym-value env form)))))
  
(defun prepend/reduce-equiv (what fn to &key identity (test #'eql))
  "Reduce WHAT with FN, then APPEND whatever is produced with TO, unless
     that which is produced is an identity transform of identity,
     with regard to test."
  (let ((appendee (reduce fn what :initial-value identity)))
    (if (funcall test appendee identity)
        to
        (append (list appendee) to))))

(defun eval-if (predicate form)
  "XXX: rename to MAYBE-EVAL?"
  (if predicate
      (eval form)
      form))

(defun eeval (form &optional bitmasks envs)
  (flet ((emit-deferred-form (&aux (bitmask (or (car bitmasks) -1)))
           `(eval-atom ,form ,bitmask ,(car envs))))
    (cond
      ((atom form) (let ((imm-p (immediate-p form)))
                     (values (if imm-p (eval-atom form (or (car bitmasks) -1) (car envs)) (emit-deferred-form))
                             (not imm-p))))
      ((quoted-p form) form)
      ((null (eop (car form))) (values (emit-deferred-form) t))
      (t (multiple-value-bind (present-p assoc-op-p identity) (eop (car form))
           (declare (ignore present-p))
           (labels ((iterate (parms bitmasks envs)
                             (let* ((parm (car parms)) (quotedp (quoted-p parm))
                                    (bmask (car bitmasks)) (bitmasks (if quotedp bitmasks (cdr bitmasks)))
                                    (env (car envs)) (envs (if quotedp envs (cdr envs))))
                               (when parms
                                 (multiple-value-bind (tail var-p fold) (iterate (cdr parms) bitmasks envs)
                                   (multiple-value-bind (stail svar-p) (eeval parm `(,bmask) `(,env))
                                     (if (and (immediate-p parm) assoc-op-p)
                                         (values tail (or var-p svar-p) (cons stail fold))
                                         (values (cons stail tail) (or var-p svar-p) fold))))))))
             (multiple-value-bind (tail var-p fold) (iterate (cdr form) bitmasks envs)
               (let ((params (prepend/reduce-equiv fold (fdefinition (car form)) tail
                              :identity identity)))
                 (values (eval-if (not var-p) (list* (car form) params)) var-p)))))))))
