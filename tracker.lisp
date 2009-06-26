;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TRACKER; Base: 10 -*-
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

(in-package :tracker)

(defclass tracker-environment (hash-table-environment) ())
(defclass top-level-tracker-environment (top-level-environment tracker-environment) ())

(defun make-top-level-tracker ()
  (make-instance 'top-level-tracker-environment))

(defmethod bind :around ((o tracker-environment) (name symbol) value)
  (when (name-bound-p o name)
    (error 'environment-name-already-bound :name name :env o))
  (call-next-method))

(defun map-tracked-keys (env fn)
  "Map FN over the keys tracked in the pool established by the most recently
entered WITH-TRACKER form with NAME.
FN must be a function of three arguments, and it will be provided with
the key name, its value-finalizer pair and the list of references."
  (iter (for (name (global-key value-finalizer . references)) in-hashtable (env-mapping env))
        (funcall fn global-key value-finalizer references)))

(defun tracker-set-global-key-value-and-finalizer (env global-key finalizer value)
  "Set the global FINALIZER and its VALUE parameter for the value tracked
at the GLOBAL-KEY in the tracked pool established by the most recently
entered WITH-TRACKER form with NAME.
The VALUE will be passed to the FINALIZER, which might iterate over
per-reference values using MAP-TRACKER-KEY-REFERENCES.
FINALIZER must be a function of one argument."
  (setf (lookup-value env global-key) (list value finalizer)))

(defun tracker-add-global-key-value-and-finalizer (env global-key finalizer value)
  "Add KEY, and set its global FINALIZER and VALUE parameter in the
tracked pool established by the most recently entered WITH-TRACKER
form with NAME.
The VALUE will be passed to the FINALIZER, which might iterate over
per-reference values using MAP-TRACKER-KEY-REFERENCES.
FINALIZER must be a function of one argument."
  (bind env global-key (list value finalizer)))

(defun tracker-release-key-and-process-references (env global-key)
  "Release the KEY back into the tracker pool established by the most
recently entered WITH-TRACKER form with NAME, and call the associated
global finalizer."
  (destructuring-bind (value finalizer &rest references) (lookup-value env global-key)
    (declare (ignore references))
    (when finalizer
      (funcall finalizer value))
    (unbind env global-key)))

(defmacro with-tracked-set ((env &rest bound-set) &body body)
  "Execute BODY within context established by the most recently entered
WITH-ALLOCATOR form with NAME. The established context is used to determine
the result of EVAL-ALLOCATED form evaluations."
  (with-gensyms (specials lexical-renames globals)
    (once-only (env)
      `(with-lexical-frame-bindings (,env ,@bound-set) (,specials ,lexical-renames)
         (let ((,globals (append ,specials ,lexical-renames)))
           (unwind-protect
                (progn
                  (mapcar (rcurry (curry #'bind ,env) nil) ,globals)
                  ,@body)
             (mapcar (curry #'tracker-release-key-and-process-references ,env) ,globals)))))))

(defun map-tracker-key-references (env key fn)
  "Map FN over the reference values associated with KEY in the tracker pool
established by the most recently entered WITH-TRACKER form with NAME,
with respect to the lexical key environment, established by the most recently
entered TRACKER-LET form."
  (destructuring-bind (value finalizer &rest references) (evaluate env key)
    (declare (ignore value finalizer))
    (mapc fn references)))

(defun tracker-set-key-value-and-finalizer (env key finalizer value)
  "Set the global FINALIZER and its VALUE parameter for the value tracked
at the KEY in the tracked pool established by the most recently entered 
WITH-TRACKER form with NAME, with respect to the lexical key environment,
established by the most recently entered TRACKER-LET form.
The VALUE will be passed to the FINALIZER, which might iterate over
per-reference values using MAP-TRACKER-KEY-REFERENCES.
FINALIZER must be a function of one argument."
  (let ((cell (evaluate env key)))
    (setf (first cell) value
          (second cell) finalizer)))

(defun tracker-reference-key (env key value)
  "Set the VALUE reference associated with KEY in the tracker pool
established by the most recently entered WITH-TRACKER form with NAME, 
with respect to the lexical key environment, established by the most
recently entered TRACKER-LET form.
The VALUE will be accessible in the global finalizer, provided by the
TRACKER-SET-KEY-VALUE-AND-FINALIZER function in the context of the most
recently entered WITH-TRACKER form with NAME."
  (let ((cell (evaluate env key)))
    (push value (cddr cell))))
