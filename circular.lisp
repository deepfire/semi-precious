(defpackage circular-buffer
  (:nicknames :circbuf)
  (:use :common-lisp :iterate)
  (:export
   #:circular-buffer
   #:circular-buffer-elt
   #:circular-buffer-shrink
   #:circular-buffer-extend
   #:circular-buffer-push
   #:circular-buffer-push
   #:circular-buffer-size
   #:circular-buffer-limit
   #:circular-buffer-sublist-headwards))

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
  (min (1+ n) (circular-buffer-size o)))

(defun pointer-reencircle (o n)
  (mod (- (head o) n (- (circular-buffer-limit o))) (circular-buffer-limit o)))

(defun circular-buffer-elt (i o)
  (aref (store o) (pointer-reencircle o (pointer-cap-by-size o i))))

(defun (setf circular-buffer-elt) (val i o)
  (setf (aref (store o) (pointer-reencircle o (pointer-cap-by-size o i))) val))

(defun circular-buffer-shrink-store (o physaddr)
  (iter (for i from physaddr to (1- (circular-buffer-limit o)))
        (setf (aref (store o) i) (aref (store o) (1+ i))))
  (incf (charge o)))

(defun circular-buffer-extend-store (o physaddr)
  (let ((old-dimension (circular-buffer-limit o)))
    (when (zerop (charge o))
      (setf (store o) (adjust-array (store o) (+ (array-dimension (store o) 0) (granularity o))))
      (incf (charge o) (granularity o)))
    (decf (charge o))
    (iter (for i from (1- old-dimension) downto physaddr)
          (setf (aref (store o) (1+ i)) (aref (store o) i)))))

(defun circular-buffer-shrink (o n)
  (when (plusp (circular-buffer-size o))
    (let ((physaddr (pointer-reencircle o (pointer-cap-by-size o n))))
      (circular-buffer-shrink-store o physaddr)
      (unless (< (head o) physaddr)
        (decf (head o))))))

(defun circular-buffer-extend (o n)
  (let ((physaddr (pointer-reencircle o (pointer-cap-by-size o n))))
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
  (let* ((from (pointer-cap-by-size o from))
         (start (pointer-reencircle o from))
         (count (min count from)))         ;; cap the desire by start
    (iter (for i first start then (circular-buffer-next i o))
          (for j from 0 below count)
          (collect (aref (store o) i)))))
