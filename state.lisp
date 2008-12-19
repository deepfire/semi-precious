;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: STATE; Base: 10 -*-
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


(in-package :state)

(defstruct (machine (:constructor %make-machine))
  current parameter change-fn
  (states (make-hash-table :test #'eq)))

(defun make-machine (parameter initial states &key (change-fn #'identity))
  (let ((machine (%make-machine :current initial :parameter parameter
				:change-fn change-fn)))
    (dolist (state states)
      (setf (gethash state (machine-states machine)) (cons nil nil)))
    machine))

(defun state-present-p (machine state)
  (nth-value 1 (gethash state (machine-states machine))))

(defun incoming (machine state)
  (car (gethash state (machine-states machine))))

(defun (setf incoming) (val machine state)
  (setf (car (gethash state (machine-states machine))) val))

(defun outgoing (machine state)
  (cdr (gethash state (machine-states machine))))

(defun (setf outgoing) (val machine state)
  (setf (cdr (gethash state (machine-states machine))) val))

(defun deftransition (machine from to action)
  (unless (and (state-present-p machine from) (state-present-p machine to))
    (error "DEFTRANSITION: ~S -> ~S identifies an inexistent state pair." from to))
  (push (cons to action) (outgoing machine from))
  (push from (incoming machine to)))

(defun direct-transition (machine from to)
  (if-let ((transition (assoc to (outgoing machine from))))
	  (list transition)))

(defun find-path-1 (machine from to)
  (if-let ((direct (direct-transition machine from to)))
	  direct
	  (if-let ((touchpoint (first (intersection (incoming machine to)
						    (mapcar #'car (outgoing machine from))))))
		  (nconc (direct-transition machine from touchpoint)
			 (direct-transition machine touchpoint to)))))

(defun state (machine)
  (machine-current machine))

(defun (setf state) (to machine)
  (let ((cur (machine-current machine)))
    (unless (eq cur to)
      (let ((from cur)
	    (path (find-path-1 machine (machine-current machine) to)))
	(unless path
	  (error "Unable to find path from ~S to ~S." from to))
	(dolist (state path)
	  (funcall (cdr state) (machine-parameter machine))
	  (funcall (machine-change-fn machine) from (car state))
	  (setf (machine-current machine) (car state)
		from (car state)))))))
