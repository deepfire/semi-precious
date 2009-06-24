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

(define-condition environment-condition (condition) 
  ((env :accessor condition-env :initarg :env)))
(define-condition environment-error (environment-condition error) ())
(define-reported-condition environment-name-not-bound (environment-condition cell-error) ()
  (:report (env name) "~@<Name ~A not bound in ~A~:@>" name env))
(define-reported-condition environment-value-not-bound (environment-condition cell-error) ()
  (:report (env name) "~@<Value ~A not bound in ~A~:@>" name env))

(define-condition allocation-condition (condition) ())
(define-condition allocation-error (allocation-condition error) ())
(define-simple-error allocation-error)

(defclass environment ()
  ((mapping :accessor env-mapping :type hash-table :initarg :mapping))
  (:default-initargs :mapping (make-hash-table :test 'eq)))

(defclass total-environment (environment)
  ((reverse-mapping :accessor env-reverse-mapping :type hash-table :initarg :reverse-mapping))
  (:default-initargs :reverse-mapping (make-hash-table :test 'equal)))

(defclass allocation-pool (total-environment)
  ((freelist :accessor pool-freelist :type list :initarg :freelist))
  (:default-initargs :freelist nil))

(defclass lexical-environment (environment)
  ((parent :accessor lexenv-parent :initarg :parent)))

(defgeneric bind (environment name value)
  (:method ((o environment) (name symbol) value)
    (setf (gethash name (env-mapping o)) value))
  (:method :after ((o total-environment) (name symbol) value)
    (setf (gethash value (env-reverse-mapping o)) name)))

(defgeneric unbind (environment name)
  (:method :around ((o environment) (name symbol))
    (unless (nth-value 1 (gethash name (env-mapping o)))
      (error 'environment-name-not-bound :env o :name name)))
  (:method ((o environment) (name symbol))
    (remhash name (env-mapping o)))
  (:method :before ((o total-environment) (name symbol))
    (remhash (gethash name (env-reverse-mapping o)) (env-mapping o))))

(defgeneric unbind-by-value (environment value)
  (:method :around ((o total-environment) (value symbol))
    (unless (nth-value 1 (gethash value (env-reverse-mapping o)))
      (error 'environment-name-not-bound :env o :name value)))
  (:method ((o total-environment) value)
    (let ((name (gethash value (env-reverse-mapping o))))
      (remhash name (env-mapping o))
      (remhash value (env-reverse-mapping o)))))

(defgeneric value (environment name)
  (:method :around ((o environment) (name symbol))
    (unless (nth-value 1 (gethash name (env-mapping o)))
      (error 'environment-name-not-bound :env o :name name)))
  (:method ((o environment) (name symbol))
    (gethash name (env-mapping o))))

(defgeneric name (environment value)
  (:method :around ((o total-environment) (value symbol))
    (unless (nth-value 1 (gethash value (env-reverse-mapping o)))
      (error 'environment-name-not-bound :env o :name value)))
  (:method ((o total-environment) value)
    (gethash value (env-reverse-mapping o))))

(defun pool-allocate (env key)
  "Allocate a KEY - VALUE pair from the allocatable value pool established by
the most recently entered WITH-ALLOCATOR form with NAME."
  (if-let ((free (pop (the list (pool-freelist env)))))
    (prog1 free
      (bind env key free))
    (allocation-error "~@<Allocation pool ~S has been drained.~:@>" name)))

(defun pool-release (env value)
  "Release the VALUE back into the allocatable value pool established by 
the most recently entered WITH-ALLOCATOR form with NAME."
  (unbind-by-value env value)
  (push value (the list (pool-freelist env))))

(defun allocated-environment (env)
  "Return the accumulated environment at the point of call, for the
allocatable value pool established by the most recently entered
WITH-ALLOCATOR form with NAME, as an association list."
  (hash-table-alist (env-mapping env)))

(defmacro with-allocator ((name set) &body body)
  "Execute BODY in a context, where POOL-ALLOCATE, POOL-RELEASE and EVAL-ALLOCATABLE
provide access to the NAME'd value allocation pool operating on SET."
  (let ((free-sym (format-symbol (symbol-package name) "*FREE-~AS*" name))
        (busy-sym (format-symbol (symbol-package name) "*BUSY-~AS*" name)))
    `(let* ((,free-sym ,set)
            (,busy-sym nil))
       (declare (special ,free-sym ,busy-sym) (type list ,free-sym ,busy-sym))
       ,@body)))

(defun lexical-p (name key &aux
                  (lexicals-sym (format-symbol (symbol-package name) "*~A-LEXICALS*" name)))
  "Determine whether KEY refers to a lexically allocated variable
within the most recently entered ALLOCATE-LET form with NAME."
  (not (null (assoc key (the list (symbol-value lexicals-sym))))))

(defun allocate-lexical-binding (name key &aux
                                 (lexicals-sym (format-symbol (symbol-package name) "*~A-LEXICALS*" name)))
  "Allocate a lexical KEY in the lexical environment established by
the most recently entered ALLOCATE-LET form with NAME."
  (if (find key (the list (symbol-value lexicals-sym)) :key #'car)
      (allocation-error "~@<Lexical key ~S already allocated in env ~S: ~S.~:@>" key name (the list (symbol-value lexicals-sym)))
      (lret ((lexical-rename (gensym "ALLOCATE-LEXICAL")))
        (push (cons key lexical-rename) (the list (symbol-value lexicals-sym))))))

(defun undo-lexical-binding (name key &aux
                             (lexicals-sym (format-symbol (symbol-package name) "*~A-LEXICALS*" name)))
  "Remove the lexical binding for KEY from the lexical environment
established by the most recently entered ALLOCATE-LET form with NAME."
  (if (find key (the list (symbol-value lexicals-sym)) :key #'car)
      (removef (the list (symbol-value lexicals-sym)) key :key #'car)
      (allocation-error "~@<Lexical key ~S is not allocated in env ~S: ~S.~:@>" key name (the list (symbol-value lexicals-sym)))))

(defun lexical-binding (name key &aux
                        (lexicals-sym (format-symbol (symbol-package name) "*~A-LEXICALS*" name)))
  "Return the value associated with KEY by the most recently entered
ALLOCATE-LET form with NAME, if any."
  (cdr (assoc key (the list (symbol-value lexicals-sym)))))

(defun set-lexical-binding (name key value &aux
                            (lexicals-sym (format-symbol (symbol-package name) "*~A-LEXICALS*" name)))
  "Set the value associated with KEY by the most recently entered
ALLOCATE-LET form with NAME, if any."
  (setf (cdr (assoc key (the list (symbol-value lexicals-sym)))) value))

(defsetf lexical-binding set-lexical-binding)

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

(defun allocate-lexically-bound-global (name key)
  "Allocate a lexical KEY in the the most recently entered ALLOCATE-LET
lexical pool, and a corresponding backing key from the global allocatable
value pool established by the most recently entered WITH-ALLOCATOR form
with NAME."
  (pool-allocate name (allocate-lexical-binding name key)))

(defun eval-allocated (name key &aux
                       (busy-list (format-symbol (symbol-package name) "*BUSY-~AS*" name)))
  "Evaluate KEY in the context established by the most recently entered
WITH-ALLOCATOR form with NAME."
  (labels ((eval-cell (key)
             (if-let ((association (assoc key (the list (symbol-value busy-list)))))
               (cdr association)
               (allocation-error "~@<~A was not allocated in the ~A allocation pool.~:@>" key name))))
    (eval-cell (or (lexical-binding name key) key))))

(defmacro with-tracker (name &body body)
  "Execute BODY in a context, where TRACKER-ALLOCATE and TRACKER-RELEASE
provide access to the NAME'd value tracking pool."
  (let ((tracked-sym (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
    `(let ((,tracked-sym nil))
       (declare (special ,tracked-sym) (type list ,tracked-sym))
       ,@body)))

(defun track-key (name key &aux
                  (tracked-list (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
  "Allocate KEY in the tracked pool established by the most recently entered 
WITH-TRACKER form with NAME."
  (if-let ((binding (find key (the list (symbol-value tracked-list)) :key #'car)))
    (allocation-error "~@<Key ~S is already tracked in pool ~S.~:@>" key name)
    (caar (push (list key nil) (the list (symbol-value tracked-list))))))

(defun map-tracked-keys (name fn &aux
                         (tracked-list (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
  "Map FN over the keys tracked in the pool established by the most recently
entered WITH-TRACKER form with NAME.
FN must be a function of three arguments, and it will be provided with
the key name, its value-finalizer pair and the list of references."
  (iter (for (name (global-key value-finalizer . references)) in (the list (symbol-value tracked-list)))
        (funcall fn global-key value-finalizer references)))

(defun tracker-set-global-key-value-and-finalizer (name global-key finalizer value &aux
                                                   (tracked-list (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
  "Set the global FINALIZER and its VALUE parameter for the value tracked
at the GLOBAL-KEY in the tracked pool established by the most recently
entered WITH-TRACKER form with NAME.
The VALUE will be passed to the FINALIZER, which might iterate over
per-reference values using MAP-TRACKER-KEY-REFERENCES.
FINALIZER must be a function of one argument."
  (if-let ((binding (find global-key (the list (symbol-value tracked-list)) :key #'car)))
    (setf (cadr binding) (cons value finalizer))
    (allocation-error "~@<Global key ~S is not tracked in pool ~S.~:@>" global-key name)))

(defun tracker-add-global-key-value-and-finalizer (name key finalizer value)
  "Add KEY, and set its global FINALIZER and VALUE parameter in the
tracked pool established by the most recently entered WITH-TRACKER
form with NAME.
The VALUE will be passed to the FINALIZER, which might iterate over
per-reference values using MAP-TRACKER-KEY-REFERENCES.
FINALIZER must be a function of one argument."
  (track-key name key)
  (tracker-set-global-key-value-and-finalizer name key finalizer value))

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
  (or (cdr (assoc key (the list (symbol-value lexicals-sym)))) key))

(defun map-tracker-key-references (name key fn &aux
                                   (global-key (globalise-tracked-key name key))
                                   (tracked-list (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
  "Map FN over the reference values associated with KEY in the tracker pool
established by the most recently entered WITH-TRACKER form with NAME,
with respect to the lexical key environment, established by the most recently
entered TRACKER-LET form."
  (if-let ((binding (find global-key (the list (symbol-value tracked-list)) :key #'car)))
    (mapc fn (cddr binding))
    (allocation-error "~@<Key ~S is not tracked in pool ~S.~:@>" global-key name)))

(defun tracker-set-key-value-and-finalizer (name key finalizer value &aux
                                            (global-key (globalise-tracked-key name key))
                                            (tracked-list (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
  "Set the global FINALIZER and its VALUE parameter for the value tracked
at the KEY in the tracked pool established by the most recently entered 
WITH-TRACKER form with NAME, with respect to the lexical key environment,
established by the most recently entered TRACKER-LET form.
The VALUE will be passed to the FINALIZER, which might iterate over
per-reference values using MAP-TRACKER-KEY-REFERENCES.
FINALIZER must be a function of one argument."
  (if-let ((binding (find global-key (the list (symbol-value tracked-list)) :key #'car)))
    (setf (cadr binding) (cons value finalizer))
    (allocation-error "~@<Key ~S/~S is not tracked in pool ~S.~:@>" key global-key name)))

(defun tracker-reference-key (name key value &aux
                              (global-key (globalise-tracked-key name key))
                              (tracked-list (format-symbol (symbol-package name) "*TRACKED-~A*" name)))
  "Set the VALUE reference associated with KEY in the tracker pool
established by the most recently entered WITH-TRACKER form with NAME, 
with respect to the lexical key environment, established by the most
recently entered TRACKER-LET form.
The VALUE will be accessible in the global finalizer, provided by the
TRACKER-SET-KEY-VALUE-AND-FINALIZER function in the context of the most
recently entered WITH-TRACKER form with NAME."
  (if-let ((binding (find global-key (the list (symbol-value tracked-list)) :key #'car)))
    (push value (cddr binding))
    (allocation-error "~@<Key ~S/~S is not tracked in pool ~S.~:@>" key global-key name)))
