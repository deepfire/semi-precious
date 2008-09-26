(defpackage octree-1d
  (:nicknames :oct-1d)
  (:use :common-lisp :alexandria :pergamum)
  (:export
   #:tree #:make-tree #:invalid-tree-address
   #:insert
   #:resolve #:resolve-next
   #:do-tree-values
   #:tree-list))

(in-package :oct-1d)

;; three node formats:
;; leaf: ((unsigned-byte 32) . (value . (prev . next)))
;; plug leaf: (:plug . (:plug . (prev . next)))
;; internal: (node . node)
(defun make-leaf (addr val prev next)
  (cons addr (cons val (cons nil (cons nil (cons prev next))))))

(defun make-plug-leaf (b h)
  (cons :plug (cons :plug (cons b (cons h (cons nil nil))))))

(defstruct (tree (:copier %copy-tree) (:constructor %make-tree))
  (start 0 :type integer)
  (length 0 :type (unsigned-byte 32))
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
  (declare (type integer addr))
  (when (>= addr (tree-length tree))
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
	     (cond ((leaf-plug-p sub)
		    (setf (leaf-addr sub) addr (leaf-val sub) val))
		   ((= (leaf-addr sub) addr)
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
	   (iterate (sub barrier half)
             (if (leafp sub)
                 (split-or-replace-leaf sub barrier half)
                 (let ((quarta (ash half -1)))
                   (if (< addr barrier)
                       (iterate (car sub) (- barrier quarta) quarta)
                       (iterate (cdr sub) (+ barrier quarta) quarta))))))
    (iterate (tree-root tree)
	     (+ (tree-start tree)
		(ash (tree-length tree) -1))
	     (ash (tree-length tree) -1))))

(defun seek-plugs (sub fn)
  (let ((prev (funcall fn sub)))
    (if (leaf-plug-p prev)
	(seek-plugs prev fn)
	prev)))

(defun %resolve (addr tree)
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

(defun resolve (addr tree)
  (let ((result (%resolve addr tree)))
    (when result
      (values (cadr result) (car result)))))

(defun resolve-next (addr tree)
  (let* ((prev (%resolve addr tree))
	 (next (when prev (seek-plugs prev #'leaf-next))))
    (when next
      (values (cadr next) (car next)))))

(defun %do-tree-values (fn sub)
  (cond ((leafp sub)
	 (unless (leaf-plug-p sub)
	   (funcall fn (leaf-val sub))))
	(t
	 (%do-tree-values fn (car sub))
	 (%do-tree-values fn (cdr sub)))))

(defmacro do-tree-values ((val tree) &body body)
  `(%do-tree-values (lambda (,val)
		      ,@body)
		    (tree-root ,tree)))

(defun tree-list (tree &aux result)
  (do-tree-values (val tree)
    (push val result))
  (nreverse result))