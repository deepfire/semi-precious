(defpackage discrimination
  (:use :cl :alexandria)
  (:export
   discriminator binary-discriminator set-discriminator discriminator-sub discriminator-subs
   discriminate
   discriminator-by-id-path discriminator-by-value-path))

(in-package :discrimination)

(defclass discriminator ()
  ((id :accessor discriminator-id :initarg :id)
   (fn :accessor discriminator-fn :initarg :fn)))

(defmethod print-object ((d discriminator) stream)
  (labels ((slot (id) (if (slot-boundp d id) (slot-value d id) :unbound-slot)))
    (format stream "#<~A:~A fn: ~S>"
	    (type-of d) (slot 'id) (slot 'fn))))

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
  (let ((sub (discriminator-sub d (apply (discriminator-fn d) params))))
    (if (typep sub 'discriminator)
	(discriminate sub)
	sub)))

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
