;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DISCRIMINATION; Base: 10 -*-
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


(in-package :discrimination)

(defclass discriminator ()
  ((id :accessor discriminator-id :initarg :id)
   (fn :accessor discriminator-fn :initarg :fn)))

(defmethod print-object ((d discriminator) stream)
  (labels ((slot (id) (if (slot-boundp d id) (slot-value d id) :unbound-slot)))
    (format stream "~@<#<~;~A:~A fn: ~S~;>~:@>"
	    (type-of d) (slot 'id) (slot 'fn))))

(define-condition discrimination-error (error)
  ((discriminator :accessor condition-discriminator :initarg :discriminator)))

(define-condition discrimination-value-unbound (discrimination-error)
  ((value :accessor condition-value :initarg :value))
  (:report (lambda (condition stream)
             (format stream "~@<discriminator ~S has no binding for value ~S~:@>"
                     (condition-discriminator condition) (condition-value condition)))))

(defclass binary-discriminator (discriminator)
  ((fn :type (function (*) boolean))
   (t-sub :accessor discriminator-t :initform nil :initarg :t)
   (nil-sub :accessor discriminator-nil :initform nil :initarg :nil)))

(defclass set-discriminator (discriminator)
  ((sub-set :reader discriminator-set :initform (make-hash-table))))

(defgeneric discriminator-sub (parent val)
  (:method ((parent binary-discriminator) val)
    (if val (discriminator-t parent) (discriminator-nil parent)))
  (:method ((parent set-discriminator) val)
    (gethash val (discriminator-set parent))))

(defgeneric (setf discriminator-sub) (new parent val)
  (:method (new (parent binary-discriminator) val)
    (declare (type boolean val))
    (if val
	(setf (discriminator-t parent) new)
	(setf (discriminator-nil parent) new)))
  (:method (new (parent set-discriminator) val)
    (setf (gethash val (discriminator-set parent)) new)))

(defgeneric discriminator-subs (d)
  (:method ((d binary-discriminator))
    (list (discriminator-t d) (discriminator-nil d)))
  (:method ((d set-discriminator))
    (maphash-values #'identity (discriminator-set d))))

(defun discriminate (d &rest params)
  (declare (type discriminator d))
  (let* ((value (apply (discriminator-fn d) params))
         (sub (discriminator-sub d value)))
    (typecase sub
      (discriminator (discriminate sub))
      (null (error 'discrimination-value-unbound :discriminator d :value value))
      (t sub))))

(defun discriminator-by-id-path (path at)
  (declare (type discriminator at))
  (if (eq (car path) (discriminator-id at)) at
      (if-let ((target (find (car path) (discriminator-subs at) :key #'discriminator-id)))
	      (discriminator-by-id-path (cdr path) target)
	      (error "No discriminator matching id ~S at ~S." (car path) at))))

(defun discriminator-by-value-path (path at)
  (declare (type discriminator at))
  (if (null path) at
      (if-let ((target (discriminator-sub at (car path))))
	      (discriminator-by-value-path (cdr path) target)
	      (error "No discriminator matching value ~S at ~S." (car path) at))))
