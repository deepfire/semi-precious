;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: INTREE; Base: 10 -*-
;;;
;;;  (c) copyright 2007-2010 by
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


(in-package :intree)

;;; three node formats:
;;; leaf: ((unsigned-byte 32) . (value . (prev . next)))
;;; plug leaf: (:plug . (:plug . (prev . next)))
;;; internal: (node . node)
;;;
;;; the tree is sewn at leaves (see the leaf format)
;;; plug nodes describe unclaimed areas in properly split tree format
(defstruct (base
             (:conc-name leaf-))
  (prev    nil :type (or null base))
  (next    nil :type (or null base)))

(defstruct (plug
             (:include base)
             (:constructor make-plug ())))

(defstruct (leaf
             (:include base)
             (:constructor make-leaf (measure value prev next)))
  (measure nil :type integer)
  value)

(defstruct (tree (:copier %copy-tree) (:constructor %make-tree))
  (start  0 :type integer)
  (length 0 :type unsigned-byte)                   
  (root   (make-plug) :type (or cons base)))

(define-condition tree-error (error)
  ((tree :reader cond-tree :initarg :tree)))

(define-condition invalid-tree-measure (tree-error)
  ((measure :reader cond-measure :initarg :measure))
  (:report (lambda (cond stream)
             (format stream "~@<Measure ~D is out of bounds for tree ~S~:@>"
                     (cond-measure cond) (cond-tree cond)))))

(defun make-tree (&key (start 0) length)
  (declare (type (integer (0)) length))
  (%make-tree :start start :length (ilog2-cover length)))

(defun insert (measure val tree)
  (declare (optimize (debug 3) (safety 3) (space 0) (speed 0)) (type integer measure))
  (unless (and (>= measure (tree-start tree))
               (< measure (+ (tree-start tree) (tree-length tree))))
    (error 'invalid-tree-measure :tree tree :measure measure))
  (labels ((sew (p n)
	     (setf (leaf-prev n) p (leaf-next p) n))
	   (split-leaf-iterate (x y barrier half left-thread right-thread)
	     "return a subtree, with a nonviolating split.
	      that means inserting plugs, if we have to."
	     (let ((l-fit (<  (leaf-measure x) barrier))
		   (r-fit (>= (leaf-measure y) barrier)))
               ;; see if barrier discriminates between X and Y
	       (if (and l-fit r-fit)
		   (progn ;; all right, x is to the left, y to the right, sew
		     (iter (for lr initially x then l)
                           (for l = (pop left-thread))
                           (while l)
                           (sew l lr))
		     (iter (for rl initially y then r)
                           (for r = (pop right-thread))
                           (while r)
                           (sew rl r))
		     (sew x y)
		     (cons x y))
                   ;; no, X and Y on the same side, need to refine further..
		   (let* ((quarta (ash half -1))
                          (subx (make-plug))
                          (suby (split-leaf-iterate x y (+ barrier (* quarta (if l-fit -1 1))) quarta
                                                    (if l-fit left-thread  (cons subx left-thread))
                                                    (if r-fit right-thread (cons subx right-thread)))))
		     (when l-fit
                       (rotatef subx suby))
		     (cons subx suby)))))
	   (split-leaf (sub barrier half)
             (let ((x (make-leaf (leaf-measure sub) (leaf-value sub) nil nil))
                   (y (make-leaf       measure            val        nil nil)))
               (when (> (leaf-measure sub) measure)
                 (rotatef x y))
               (split-leaf-iterate x y barrier half (list (leaf-prev sub)) (list (leaf-next sub)))))
	   (iterate-to-leaf (sub barrier half)
             (etypecase sub
               (plug (make-leaf measure val (leaf-prev sub) (leaf-next sub)))
               (leaf (if (= measure (leaf-measure sub))
                         (progn (setf (leaf-value sub) val)
                                sub)
                         (split-leaf sub barrier half)))
               (cons (let ((quarta (ash half -1)))
                       (if (< measure barrier)
                           (setf (car sub) (iterate-to-leaf (car sub) (- barrier quarta) quarta))
                           (setf (cdr sub) (iterate-to-leaf (cdr sub) (+ barrier quarta) quarta)))
                       sub)))))
    (let ((half (ash (tree-length tree) -1)))
      ;; we adopt a dirty, yet simple approach of excessively overwriting all our way down the tree
      ;; even if it is not needed
      (setf (tree-root tree)
            (iterate-to-leaf (tree-root tree) (+ (tree-start tree) half) half)))))

(defun seek-plugs (sub fn)
  (labels ((rec (sub)
             (let ((prev (funcall fn sub)))
               (if (plug-p prev)
                   (rec prev)
                   prev))))
    (rec sub)))

(defun stab (measure tree)
  "Return the points planted to the left and to the right of the
given MEASURE in TREE as multiple values."
  (labels ((rec (sub barrier half)
             (etypecase sub
               (cons (let ((quarta (ash half -1)))
                       (if (< measure barrier)
                           (rec (car sub) (- barrier quarta) quarta)
                           (rec (cdr sub) (+ barrier quarta) quarta))))
               (plug (values (seek-plugs sub #'leaf-prev)
                             (seek-plugs sub #'leaf-next)))
               (leaf (if (< measure (leaf-measure sub))
			 (values (seek-plugs sub #'leaf-prev)
                                 sub)
			 (values sub
                                 (seek-plugs sub #'leaf-next)))))))
    (let ((half (ash (tree-length tree) -1)))
      (rec (tree-root tree) (+ (tree-start tree) half) half))))

(defun leftmost (tree)
  (labels ((rec (sub)
	     (if (consp sub)
		 (rec (car sub))
		 sub)))
    (let ((r (rec (tree-root tree))))
      (if (plug-p r)
          (seek-plugs r #'leaf-next)
          r))))

(defun rightmost (tree)
  (labels ((rec (sub)
             (if (consp sub)
                 (rec (cdr sub))
                 sub)))
    (let ((r (rec (tree-root tree))))
      (if (plug-p r)
          (seek-plugs r #'leaf-prev)
          r))))

(defun tree-left (measure tree)
  "Find in TREE the most adjacent value-measure pair with measure less,
   or equal to MEASURE, and return value and measure as multiple values, or NIL."
  (when-let ((prev (nth-value 0 (stab measure tree))))
    (values (leaf-value prev) (leaf-measure prev))))

(defun tree-right (measure tree)
  "Find in TREE the most adjacent value-measure pair with measure more
   than MEASURE, and return value and measure as multiple values, or NIL."
  (when-let ((next (nth-value 1 (stab measure tree))))
    (values (leaf-value next) (leaf-measure next))))

(defun mapc-tree (fn tree)
  "Map FN over TREE measures and values.  Return NIL."
  (labels ((rec (tree)
             (typecase tree
               (leaf (funcall fn (leaf-measure tree) (leaf-value tree)))
               (cons (rec (car tree))
                     (rec (cdr tree))))))
    (rec tree))
  (values))

(defun mapc-tree-values (fn tree)
  "Map FN over TREE values.  Return NIL."
  (labels ((rec (tree)
             (typecase tree
               (leaf (funcall fn (leaf-value tree)))
               (cons (rec (car tree))
                     (rec (cdr tree))))))
    (rec tree))
  (values))

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