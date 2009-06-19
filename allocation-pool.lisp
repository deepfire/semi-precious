;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ALLOCATION-POOL; Base: 10 -*-
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

(in-package :allocation-pool)

(define-condition allocation-condition (condition) ())
(define-condition allocation-error (allocation-condition error) ())
(define-simple-error allocation-error)

(defun pool-allocate (name key &aux
                      (free-list (format-symbol (symbol-package name) "*FREE-~AS*" name))
                      (busy-list (format-symbol (symbol-package name) "*BUSY-~AS*" name)))
  "Allocate a KEY - VALUE pair from the allocatable value pool established by
the most recently entered WITH-ALLOCATOR form with NAME."
  (if-let ((free (pop (the list (symbol-value free-list)))))
    (prog1 free
      (push (cons key free) (the list (symbol-value busy-list))))
    (allocation-error "~@<Allocation pool ~S has been drained.~:@>" name)))

(defun pool-release (name value &aux
                     (free-list (format-symbol (symbol-package name) "*FREE-~AS*" name))
                     (busy-list (format-symbol (symbol-package name) "*BUSY-~AS*" name)))
  "Release the VALUE back into the allocatable value pool established by 
the most recently entered WITH-ALLOCATOR form with NAME."
  (if (member value (the list (symbol-value busy-list)) :key #'cdr)
      (progn
        (push value (the list (symbol-value free-list)))
        (removef (the list (symbol-value busy-list)) value :key #'cdr))
      (allocation-error "~@<~S was not allocated in the ~A allocation pool.~:@>" value name)))

(defmacro with-allocator ((name set) &body body)
  "Execute BODY in a context, where POOL-ALLOCATE, POOL-RELEASE and EVAL-ALLOCATABLE
provide access to the NAME'd value allocation pool operating on SET."
  (let ((free-sym (format-symbol (symbol-package name) "*FREE-~AS*" name))
        (busy-sym (format-symbol (symbol-package name) "*BUSY-~AS*" name)))
    `(let* ((,free-sym ,set)
            (,busy-sym nil))
       (declare (special ,free-sym ,busy-sym) (type list ,free-sym ,busy-sym))
       ,@body)))

(defmacro allocate-let ((&optional name &rest bound-set) &body body)
  "Execute BODY within context established by the most recently entered
WITH-ALLOCATOR form with NAME. The established context is used to determine
the result of EVAL-ALLOCATED form evaluations."
  (when (and bound-set (not name))
    (allocation-error "~@<Requested to bind allocatables with no pool specified.~:@>"))
  (multiple-value-bind (decls body) (destructure-binding-form-body body)
    (let* ((specials (apply #'append (mapcar #'rest (remove 'special decls :key #'car :test-not #'eq))))
           (lexicals (set-difference bound-set specials))
           (lexicals-sym (format-symbol (symbol-package name) "*~A-LEXICALS*" name)))
      (when-let ((unknown-vars (set-difference specials bound-set)))
        (allocation-error "~@<ALLOCATE-LET:~; Unknown variables were declared special: ~S~:@>~%" unknown-vars))
      (with-gensyms (allocation)
        `(let* ((,lexicals-sym ',(mapcar #'cons lexicals (make-gensym-list (length lexicals) "ALLOCATE-LET")))
                (,allocation (append (mapcar (compose (curry #'pool-allocate ',name) #'cdr) ,lexicals-sym)
                                     (mapcar (curry #'pool-allocate ',name) ',specials))))
           (declare (special ,lexicals-sym))
           (unwind-protect (progn ,@body)
             (mapcar (curry #'pool-release ',name) ,allocation)))))))

(defun pool-allocate-lexical (name key &aux
                              (free-list (format-symbol (symbol-package name) "*FREE-~AS*" name))
                              (busy-list (format-symbol (symbol-package name) "*BUSY-~AS*" name))
                              (lexicals-sym (format-symbol (symbol-package name) "*~A-LEXICALS*" name)))
  "Allocate a KEY - VALUE pair from the allocatable value pool established by
the most recently entered WITH-ALLOCATOR form with NAME."
  (if (find key (the list (symbol-value lexicals-sym)) :key #'car)
      (allocation-error "~@<Lexical key ~S already allocated in pool ~S: ~S.~:@>" key name (the list (symbol-value lexicals-sym)))
      (if-let ((free (pop (the list (symbol-value free-list)))))
        (prog1 free
          (let ((lexical-rename (gensym "ALLOCATE-LEXICAL")))
            (push (cons key lexical-rename) (the list (symbol-value lexicals-sym)))
            (push (cons lexical-rename free) (the list (symbol-value busy-list)))))
        (allocation-error "~@<Allocation pool ~S has been drained.~:@>" name))))

(defun eval-allocated (name allocated &aux
                       (busy-list (format-symbol (symbol-package name) "*BUSY-~AS*" name))
                       (lexicals-sym (format-symbol (symbol-package name) "*~A-LEXICALS*" name)))
  "Evaluate ALLOCATED in the context established by the most recently
entered WITH-ALLOCATOR form with NAME."
  (labels ((eval-cell (key)
             (if-let ((association (assoc key (the list (symbol-value busy-list)))))
               (cdr association)
               (allocation-error "~@<~A/~A was not allocated in the ~A allocation pool.~:@>" allocated key name)))
           (eval-maybe-lexical (allocated)
             (or (cdr (assoc allocated (symbol-value lexicals-sym))) allocated)))
    (eval-cell (eval-maybe-lexical allocated))))

(defun track-key (name key &aux
                  (tracked-list (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
  "Allocate KEY in the tracked pool established by the most recently entered 
WITH-TRACKER form with NAME."
  (if-let ((binding (find key (the list (symbol-value tracked-list)) :key #'car)))
    (allocation-error "~@<Key ~S is already tracked in pool ~S.~:@>" key name)
    (caar (push (list key nil) (the list (symbol-value tracked-list))))))

(defun map-tracker-key-references (name key fn &aux
                                   (global-key (globalise-tracked-key name key))
                                   (tracked-list (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
  "Map FN over the reference values associated with KEY in the tracker pool
established by the most recently entered WITH-TRACKER form with NAME."
  (if-let ((binding (find global-key (the list (symbol-value tracked-list)) :key #'car)))
    (mapc fn (cddr binding))
    (allocation-error "~@<Key ~S is not tracked in pool ~S.~:@>" global-key name)))

(defun tracker-set-key-value-and-finalizer (name key finalizer value &aux
                                            (global-key (globalise-tracked-key name key))
                                            (tracked-list (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
  "Set the global FINALIZER and its VALUE parameter for the value tracked
at the KEY in the tracked pool established by the most recently entered 
WITH-TRACKER form with NAME.
The VALUE will be passed to the FINALIZER, which might iterate over
per-reference values using MAP-TRACKER-KEY-REFERENCES.
FINALIZER must be a function of one argument."
  (if-let ((binding (find global-key (the list (symbol-value tracked-list)) :key #'car)))
    (setf (cadr binding) (cons value finalizer))
    (allocation-error "~@<Key ~S/~S is not tracked in pool ~S.~:@>" key global-key name)))

(defun tracker-add-key-value-and-finalizer (name key finalizer value)
  "Add KEY, and set its global FINALIZER and VALUE parameter in the
tracked pool established by the most recently entered WITH-TRACKER
form with NAME.
The VALUE will be passed to the FINALIZER, which might iterate over
per-reference values using MAP-TRACKER-KEY-REFERENCES.
FINALIZER must be a function of one argument."
  (track-key name key)
  (tracker-set-key-value-and-finalizer name key finalizer value))

(defun tracker-reference-key (name key value &aux
                              (global-key (globalise-tracked-key name key))
                              (tracked-list (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
  "Set the VALUE reference associated with KEY in the tracker pool
established by the most recently entered WITH-TRACKER form with NAME.
The VALUE will be accessible in the global finalizer, provided by the
TRACKER-SET-KEY-VALUE-AND-FINALIZER function in the context of the most
recently entered WITH-TRACKER form with NAME."
  (if-let ((binding (find global-key (the list (symbol-value tracked-list)) :key #'car)))
    (push value (cddr binding))
    (allocation-error "~@<Key ~S/~S is not tracked in pool ~S.~:@>" key global-key name)))

(defun tracker-release-key-and-process-references (name key &aux
                                                   (tracked-list (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
  "Release the KEY back into the tracker pool established by the most
recently entered WITH-TRACKER form with NAME, and call the associated
global finalizer."
  (if-let ((track (assoc key (the list (symbol-value tracked-list)))))
    (destructuring-bind (value . finalizer) (second track)
      (when finalizer
        (funcall finalizer value))
      (removef (the list (symbol-value tracked-list)) key :key #'car))
    (allocation-error "~@<Key ~S is not tracked in the ~A tracker pool.~:@>" key name)))

(defmacro with-tracker (name &body body)
  "Execute BODY in a context, where TRACKER-ALLOCATE and TRACKER-RELEASE
provide access to the NAME'd value tracking pool."
  (let ((tracked-sym (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
    `(let ((,tracked-sym nil))
       (declare (special ,tracked-sym) (type list ,tracked-sym))
       ,@body)))

(defmacro tracker-let ((name &rest keys) &body body)
  "Execute BODY within context established by the most recently entered
WITH-TRACKER form with NAME. The established context is used to provide
conflict detection and finalization of the tracked keys."
  (when (and keys (not name))
    (allocation-error "~@<Requested to track keys with no pool specified.~:@>"))
  (multiple-value-bind (decls body) (destructure-binding-form-body body)
    (let* ((specials (apply #'append (mapcar #'rest (remove 'special decls :key #'car :test-not #'eq))))
           (lexicals (set-difference keys specials))
           (lexicals-sym (format-symbol (symbol-package name) "*TRACKED-~A-LEXICALS*" name)))
      (when-let ((unknown-vars (set-difference specials keys)))
        (allocation-error "~@<TRACKER-LET:~; Unknown variables were declared special: ~S~:@>~%" unknown-vars))
      (with-gensyms (allocation)
        `(let* ((,lexicals-sym ',(mapcar #'cons lexicals (make-gensym-list (length lexicals) "TRACKER-LET")))
                (,allocation (append (mapcar (compose (curry #'track-key ',name) #'cdr) ,lexicals-sym)
                                     (mapcar (curry #'track-key ',name) ',specials))))
           (declare (special ,lexicals-sym))
           (unwind-protect (progn ,@body)
             (mapcar (curry #'tracker-release-key-and-process-references ',name) ,allocation)))))))

(defun globalise-tracked-key (name key &aux
                              (lexicals-sym (format-symbol (symbol-package name) "*TRACKED-~A-LEXICALS*" name)))
  "Evaluate KEY in the context established by the most recently
entered WITH-ALLOCATOR and TRACKER-LET forms with NAME."
  (or (cdr (assoc key (symbol-value lexicals-sym))) key))