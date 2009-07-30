;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OCTREE-1D; Base: 10 -*-
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


(in-package :oct-1d)

;;; three node formats:
;;; leaf: ((unsigned-byte 32) . (value . (prev . next)))
;;; plug leaf: (:plug . (:plug . (prev . next)))
;;; internal: (node . node)
;;;
;;; the tree is sewn at leaves (see the leaf format)
;;; plug nodes describe unclaimed areas in properly split tree format
(defun make-leaf (addr val prev next)
  (cons addr (cons val (cons nil (cons nil (cons prev next))))))

(defun make-plug-leaf (b h)
  (cons :plug (cons :plug (cons b (cons h (cons nil nil))))))

(defstruct (tree (:copier %copy-tree) (:constructor %make-tree))
  (start 0 :type integer)
  (length 0 :type unsigned-byte)
  (root (make-plug-leaf 0 0)))

(define-condition tree-error (error)
  ((tree :reader cond-tree :initarg :tree)))

(define-condition invalid-tree-address (tree-error)
  ((address :reader cond-address :initarg :address))
  (:report (lambda (cond stream)
             (format stream "~@<Address ~D is out of bounds for tree ~S~:@>"
                     (cond-address cond) (cond-tree cond)))))

(defun make-tree (&key (start 0) length)
  (declare (type (integer (0)) length))
  (%make-tree :start start :length (ilog2-cover length)))

(defmacro leaf-addr (leaf)
  `(first ,leaf))

(defmacro leaf-val (leaf)
  `(second ,leaf))

(defun leafp (node)
  (not (consp (leaf-addr node))))

(defun leaf-plug-p (leaf)
  (eq (leaf-addr leaf) :plug))

(defmacro leaf-b (leaf)
  `(third ,leaf))

(defmacro leaf-h (leaf)
  `(fourth ,leaf))

(defun leaf-prev (leaf)
  (caddr (cddr leaf)))

(defun leaf-next (leaf)
  (cdddr (cddr leaf)))

(defun (setf leaf-prev) (val leaf)
  (setf (caddr (cddr leaf)) val))

(defun (setf leaf-next) (val leaf)
  (setf (cdddr (cddr leaf)) val))

(defun leftmost (tree)
  (labels ((iterate (sub)
	     (if (leafp sub)
		 sub
		 (iterate (car sub)))))
    (iterate (tree-root tree))))

(defun print-l (leaf)
  (format t "~S-~S, ~S+/-~S"
	  (leaf-addr leaf) (leaf-val leaf) (leaf-b leaf) (leaf-h leaf)))

(defun walkcheck (leaf)
  (format t "===========================================~%")
  (loop :for iter = leaf :then (leaf-next iter) :while iter
     :do (print-l iter) (terpri)))

(defun insert (addr val tree)
  (declare (optimize (debug 3) (safety 3) (space 0) (speed 0)) (type integer addr))
  (unless (and (>= addr (tree-start tree))
               (< addr (+ (tree-start tree) (tree-length tree))))
    (error 'invalid-tree-address :tree tree :address addr))
  (labels ((sew (p n)
	     (setf (leaf-prev n) p (leaf-next p) n))
	   (split-leaf-iterate (x y barrier half lsew rsew)
	     "return a subtree, with a nonviolating split.
	      that means inserting plugs, if we have to."
	     (setf (leaf-b x) barrier (leaf-h x) half
		   (leaf-b y) barrier (leaf-h y) half)
	     (let ((l-fit (< (leaf-addr x) barrier))
		   (r-fit (>= (leaf-addr y) barrier)))
	       (if (and l-fit r-fit) ;; barrier is descriptive, no need to subdivide
		   (progn
		     (loop :for lr = x :then l :for l = (pop lsew) :while l
                                                                   :do (sew l lr))
		     (loop :for rl = y :then r :for r = (pop rsew) :while r
                                                                   :do (sew rl r))
		     (sew x y)
		     (values x y))
		   (let* ((quarta (ash half -1))
			  (plug (make-plug-leaf (+ barrier (* quarta (if l-fit 1 -1)))
						quarta))
			  (ret (multiple-value-call #'cons
                                 (split-leaf-iterate
                                  x y (+ barrier (* quarta (if l-fit -1 1))) quarta
                                  (if l-fit lsew (cons plug lsew))
                                  (if r-fit rsew (cons plug rsew)))))
			  (subx plug) (suby ret))
		     (when l-fit (rotatef subx suby))
		     (values subx suby)))))
	   (split-or-replace-leaf (sub barrier half)
	     (cond ((leaf-plug-p sub)        ;; no information stored
		    (setf (leaf-addr sub) addr (leaf-val sub) val))
		   ((= (leaf-addr sub) addr) ;; overwriting
		    (setf (leaf-val sub) val))
		   (t
		    (destructuring-bind (saddr sval b h . (prev . next)) sub
		      (declare (ignorable b h))
		      (let ((x (make-leaf saddr sval nil nil))
			    (y (make-leaf addr val nil nil)))
			(if (> saddr addr)
			    (rotatef x y))
			(setf (values (car sub) (cdr sub))
			      (split-leaf-iterate x y barrier half (list prev) (list next))))))))
	   (iterate-to-leaf (sub barrier half)
             (if (leafp sub)
                 (split-or-replace-leaf sub barrier half)
                 (let ((quarta (ash half -1)))
                   (if (< addr barrier)
                       (iterate-to-leaf (car sub) (- barrier quarta) quarta)
                       (iterate-to-leaf (cdr sub) (+ barrier quarta) quarta))))))
    (let ((half (ash (tree-length tree) -1)))
      (iterate-to-leaf (tree-root tree) (+ (tree-start tree) half) half))))

(defun seek-plugs (sub fn)
  (let ((prev (funcall fn sub)))
    (if (leaf-plug-p prev)
	(seek-plugs prev fn)
	prev)))

(defun %tree-left (addr tree)
  (labels ((iterate (sub barrier half)
	     (if (leafp sub)
		 (if (leaf-plug-p sub)
		     (seek-plugs sub #'leaf-prev)
		     (if (< addr (leaf-addr sub))
			 (seek-plugs sub #'leaf-prev)
			 sub))
		 (let ((quarta (ash half -1)))
		   (if (< addr barrier)
		       (iterate (car sub) (- barrier quarta) quarta)
		       (iterate (cdr sub) (+ barrier quarta) quarta))))))
    (iterate (tree-root tree)
	     (+ (tree-start tree)
		(ash (tree-length tree) -1))
	     (ash (tree-length tree) -1))))

(defun tree-left (addr tree)
  "Find in TREE the most adjacent value-address pair with address less,
   or equal to ADDR, and return name and address as multiple values, or NIL."
  (let ((result (%tree-left addr tree)))
    (when result
      (values (cadr result) (car result)))))

(defun tree-right (addr tree)
  "Find in TREE the most adjacent value-address pair with address more
   than ADDR, and return name and address as multiple values, or NIL."
  (let* ((prev (%tree-left addr tree)))
    (if-let ((next (when prev (seek-plugs prev #'leaf-next))))
      (values (cadr next) (car next))
      (let ((leftmost (leftmost tree)))
        (values (cadr leftmost) (car leftmost))))))

(defun mapc-tree-values (fn tree)
  "Map FN over TREE values. Return NIL."
  (cond ((leafp tree)
	 (unless (leaf-plug-p tree) 
	   (funcall fn (leaf-val tree))))
	(t
	 (mapc-tree-values fn (car tree))
	 (mapc-tree-values fn (cdr tree)))))

(defmacro do-tree-values ((val tree) &body body)
  "Execute BODY for every value (lexically bound to VAL) in TREE."
  `(mapc-tree-values (lambda (,val)
                       ,@body)
		    (tree-root ,tree)))

(defun tree-list (tree &aux result)
  "Flatten TREE."
  (do-tree-values (val tree)
    (push val result))
  (nreverse result))