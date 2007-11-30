(defpackage depsolver
  (:use :cl :alexandria :pergamum)
  (:export
   depobj depend satisfied-p map-reverse-dependencies solve))

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

(defun satisfied-p (o)
  (declare (type depobj o))
  (zerop (hash-table-count (%depobj-dep# o))))

(defun color-satisfied-p (o color)
  (declare (type depobj o))
  (zerop (hash-table-count (%depobj-dep# o))))

(defun next-dep (o)
  "Pick the next depended-upon object off the list."
  (declare (type depobj o))
  (car (nth-value 2 (hash-table-next (%depobj-dep# o)))))

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
