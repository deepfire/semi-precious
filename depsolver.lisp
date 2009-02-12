;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DEPSOLVER; Base: 10 -*-
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


(in-package :depsolver)

(defclass depobj ()
  ((id :reader depobj-id :initform (gensym))
   (edge-completion :accessor depobj-edge-completion :initarg :edge-completion)
   (node-completion :accessor depobj-node-completion :initarg :node-completion)
   (dep# :accessor %depobj-dep# :type hash-table :initform (make-hash-table :test #'eq))
   (rdep# :accessor %depobj-rdep# :type hash-table :initform (make-hash-table :test #'eq))))

(defun depend (x y &optional color)
  "Make X depend on Y."
  (declare (type depobj x y))
  (setf (gethash (depobj-id y) (%depobj-dep# x)) (cons y color)
	(gethash (depobj-id x) (%depobj-rdep# y)) x))

(defun undepend (x y)
  "Make X no longer depend on Y."
  (remhash (depobj-id y) (%depobj-dep# x))
  (remhash (depobj-id x) (%depobj-rdep# y)))

(defun satisfied-p (o)
  (declare (type depobj o))
  (zerop (hash-table-count (%depobj-dep# o))))

(defun color-satisfied-p (o color)
  (declare (type depobj o) (ignore color))
  (zerop (hash-table-count (%depobj-dep# o))))

(defun next-dep (o)
  "Pick the next depended-upon object off the list."
  (declare (type depobj o))
  (car (nth-value 2 (hash-table-next (%depobj-dep# o)))))

(defun map-dependencies (fn o)
  (maphash-values (lambda (x) (funcall fn (car x))) (%depobj-dep# o)))

(defun map-reverse-dependencies (fn o)
  (maphash-values fn (%depobj-rdep# o)))

(defun solve (o)
  (loop :until (satisfied-p o) :do
     (let ((dep (next-dep o)))
       (solve dep)
       (map-reverse-dependencies (lambda (rdep)
				   (let* ((rdep-dep (gethash (depobj-id dep) (%depobj-dep# rdep)))
					  (color (cdr rdep-dep)))
				     (if-let ((completion (depobj-edge-completion rdep)))
					     (funcall completion rdep dep color))
				     (remhash (depobj-id rdep) (%depobj-rdep# dep))
				     (remhash (depobj-id dep) (%depobj-dep# rdep))))
				 dep)))
  (if-let ((completion (depobj-node-completion o)))
	  (funcall completion o)))
