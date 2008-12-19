;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CIRCULAR-BUFFER; Base: 10 -*-
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


(in-package :circular-buffer)

(defclass circular-buffer ()
  ((store :accessor store :type vector)
   (head :accessor head :type (integer 0) :initform 0)
   (tail :accessor tail :type (integer 0) :initform 0)
   (granularity :accessor granularity :type (integer 0) :initarg :granularity)
   (charge :accessor charge :type (integer 0) :initform 0))
  (:default-initargs
   :granularity 16))

(defmethod initialize-instance :after ((o circular-buffer) &rest rest &key size granularity)
  (declare (ignore rest) (type (integer 1) size))
  (let ((actual-size (+ (- size (mod size granularity)) granularity)))
    (setf (store o) (make-array actual-size :initial-element nil :adjustable t)
          (charge o) (- actual-size size))))

(defun circular-buffer-limit (o)
  (- (array-dimension (store o) 0) (charge o)))

(defun circular-buffer-size (o)
  (mod (+ (head o) (circular-buffer-limit o) (- (tail o))) (circular-buffer-limit o)))

(defun circular-buffer-clear (o)
  (setf (head o) 0 (tail o) 0)
  (adjust-array (store o) (circular-buffer-limit o) :initial-element nil))

(defun circular-buffer-prev (x o)
  (mod (1- x) (circular-buffer-limit o)))

(defun circular-buffer-next (x o)
  (mod (1+ x) (circular-buffer-limit o)))

(defun pointer-cap-by-size (o n)
  (min n (circular-buffer-size o)))

(defun headwards-pointer (o n)
  (mod (+ (tail o) n) (circular-buffer-limit o)))

(defun tailwards-pointer (o n)
  (mod (- (head o) n (- (circular-buffer-limit o))) (circular-buffer-limit o)))

(defun circular-buffer-elt (i o)
  (aref (store o) (headwards-pointer o (pointer-cap-by-size o i))))

(defun circular-buffer-elt-tailwards (i o)
  (aref (store o) (tailwards-pointer o (pointer-cap-by-size o (1+ i)))))

(defun (setf circular-buffer-elt) (val i o)
  (setf (aref (store o) (headwards-pointer o (pointer-cap-by-size o i))) val))

(defun (setf circular-buffer-elt-tailwards) (val i o)
  (setf (aref (store o) (tailwards-pointer o (pointer-cap-by-size o (1+ i)))) val))

(defun circular-buffer-shrink-store (o physaddr)
  (replace (store o) (store o) :start1 physaddr :start2 (1+ physaddr))
  (incf (charge o)))

(defun circular-buffer-extend-store (o physaddr)
  (when (zerop (charge o))
    (setf (store o) (adjust-array (store o) (+ (array-dimension (store o) 0) (granularity o))))
    (incf (charge o) (granularity o)))
  (decf (charge o))
  (replace (store o) (store o) :start1 (1+ physaddr) :start2 physaddr))

(defun circular-buffer-shrink (o n)
  (when (plusp (circular-buffer-size o))
    (let ((physaddr (tailwards-pointer o (pointer-cap-by-size o (1+ n)))))
      (circular-buffer-shrink-store o physaddr)
      (unless (< (head o) physaddr)
        (decf (head o))))))

(defun circular-buffer-extend (o n)
  (let ((physaddr (tailwards-pointer o (pointer-cap-by-size o (1+ n)))))
    (circular-buffer-extend-store o physaddr)
    (unless (< (head o) physaddr)
      (incf (head o)))))

(defun circular-buffer-push (obj o)
  (declare (type circular-buffer o))
  (setf (aref (store o) (head o)) obj
        (head o) (circular-buffer-next (head o) o))
  (when (= (head o) (tail o))
    (setf (aref (store o) (tail o)) nil
          (tail o) (circular-buffer-next (tail o) o))))

(defun circular-buffer-sublist-headwards (o from &optional (count (1+ from)))
  (declare (type (integer 0) count from))
  (let* ((from (pointer-cap-by-size o (1+ from)))
         (start (tailwards-pointer o from))
         (count (min count from)))         ;; cap the desire by start
    (iter (for i first start then (circular-buffer-next i o))
          (for j from 0 below count)
          (collect (aref (store o) i)))))

(defmacro do-circular-buffer ((var o &key start) &body body)
  (with-gensyms (i)
    (once-only (o)
      `(iter (for ,i first ,(if start `(headwards-pointer ,o ,start) `(tail ,o)) then (circular-buffer-next ,i ,o))
             (for ,var = (circular-buffer-elt ,i ,o))
             (until (= ,i (head ,o)))
             ,@body))))