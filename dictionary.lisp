;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DICTIONARY; Base: 10 -*-
;;;
;;;  (c) copyright 2008 by
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


(in-package :dictionary)

(defstruct (dictionary (:copier %copy-dictionary))
  (ids-to-values (make-array 16 :adjustable t :fill-pointer 0) :type vector)
  (symbols-to-ids (make-hash-table :test 'eq) :type hash-table)
  (growth-size 16 :type (integer (0))))

(defstruct (aliased-dictionary (:conc-name dictionary-) (:include dictionary) (:copier %copy-aliased-dictionary))
  (aliases-to-ids (make-hash-table :test 'eq) :type hash-table))

(define-condition unknown-symbol (cell-error) ())

(define-condition symbol-already-present (cell-error) ())

(define-condition dictionaries-not-form-subset-superset-relation (unknown-symbol)
  ((sub :accessor dictionary-mismatch-sub :initarg :sub)
   (super :accessor dictionary-mismatch-super :initarg :super))
  (:report (lambda (c s)
             (format s "~@<Dictionary ~S does not form a subset of ~S, symbol ~S is missing.~:@>"
                     (dictionary-mismatch-sub c) (dictionary-mismatch-super c) (cell-error-name c)))))

(declaim (inline dictionary-id-map))
(defun dictionary-id-map (dictionary)
  "Return the DICTIONARY's id to value map vector."
  (declare (type dictionary dictionary))
  (dictionary-ids-to-values dictionary))

(declaim (inline symbol-id))
(defun symbol-id (dictionary symbol &optional unaliasedp)
  "Return the id of SYMBOL in DICTIONARY."
  (declare (type dictionary dictionary) (type symbol symbol))
  (or (if (and (typep dictionary 'aliased-dictionary) (not unaliasedp))
          (gethash symbol (dictionary-aliases-to-ids dictionary))
          (gethash symbol (dictionary-symbols-to-ids dictionary)))
      (error 'cell-error :name symbol)))

(declaim (inline symbol-present-p))
(defun symbol-present-p (dictionary symbol &optional unaliasedp)
  "Determine if SYMBOL is present in DICTIONARY."
  (declare (type dictionary dictionary) (type symbol symbol))
  (nth-value 1 (if (and (typep dictionary 'aliased-dictionary) (not unaliasedp))
                   (gethash symbol (dictionary-aliases-to-ids dictionary))
                   (gethash symbol (dictionary-symbols-to-ids dictionary)))))

(declaim (inline id-value))
(defun id-value (dictionary id)
  "Return the value associated with ID in DICTIONARY."
  (declare (type dictionary dictionary) (type (integer 0) id))
  (aref (dictionary-ids-to-values dictionary) id))

(declaim (inline set-id-value))
(defun set-id-value (dictionary id new-val)
  "Set the value associated with ID in DICTIONARY to NEW-VAL."
  (declare (type dictionary dictionary) (type (integer 0) id))
  (setf (aref (dictionary-ids-to-values dictionary) id) new-val))

(defsetf id-value set-id-value)

(defun translation (dictionary symbol &optional unaliasedp)
  "Look up the value associated with SYMBOL in DICTIONARY."
  (declare (dictionary dictionary) (symbol symbol))
  (id-value dictionary (symbol-id dictionary symbol unaliasedp)))

(defun set-translation (dictionary symbol &optional new-val)
  "Change the value currently associated with SYMBOL in DICTIONARY to NEW-VAL."
  (declare (dictionary dictionary) (symbol symbol))
  (setf (id-value dictionary (symbol-id dictionary symbol)) new-val))

(defsetf translation set-translation)

(declaim (inline add-symbol-unchecked))
(defun add-symbol-unchecked (dictionary symbol value &aux
                             (values-vec (dictionary-ids-to-values dictionary))
                             (value (vector-push-extend value values-vec (dictionary-growth-size dictionary))))
  "Adds SYMBOL -> VALUE mapping into DICTIONARY.
Does not check if SYMBOL is already present."
  (when (typep dictionary 'aliased-dictionary)
    (setf (gethash symbol (dictionary-aliases-to-ids dictionary)) value))
  (setf (gethash symbol (dictionary-symbols-to-ids dictionary)) value))

(defun add-symbol (dictionary symbol value &optional (error-p t))
  "Add SYMBOL -> VALUE mapping into DICTIONARY, unless it is already present,
in which case an error is raised, unless ERROR-P is NIL."
  (cond ((null (symbol-present-p dictionary symbol))
         (add-symbol-unchecked dictionary symbol value))
        (error-p
         (error 'symbol-already-present :name symbol))))

(declaim (inline add-alias-unchecked))
(defun add-alias-unchecked (dictionary symbol alias &aux
                            (id (symbol-id dictionary symbol t)))
  "Adds an ALIAS to SYMBOL into DICTIONARY.
Do not check if SYMBOL is absent from DICTIONARY."
  (declare (type aliased-dictionary dictionary))
  (setf (gethash alias (dictionary-aliases-to-ids dictionary)) id))

(defun add-alias (dictionary symbol alias)
  "Adds an ALIAS to SYMBOL into DICTIONARY."
  (declare (type aliased-dictionary dictionary))
  (cond ((symbol-present-p dictionary symbol t)
         (add-alias-unchecked dictionary symbol alias))
        (t
         (error 'cell-error :name symbol))))

(defun copy-dictionary (d)
  "Return a copy of dictionary D."
  (declare (type dictionary d))
  (apply #'make-dictionary
         :ids-to-values (copy-array (dictionary-ids-to-values d)) 
         :symbols-to-ids (copy-hash-table (dictionary-symbols-to-ids d))
         (when (typep d 'aliased-dictionary)
           (list :aliases-to-ids (copy-hash-table (dictionary-aliases-to-ids d))))))

(defun dictionary-subset-p (d1 d2)
  "Determine whether D1 forms a subset of D2, symbol-wise."
  (declare (type dictionary d1 d2))
  (iter (for (key nil) in-hashtable (dictionary-symbols-to-ids d1))
        (unless (gethash key (dictionary-symbols-to-ids d2))
          (return-from dictionary-subset-p nil)))
  t)

(defun submerge-dictionary-to (d1 d2 &key (if-does-not-exist :error))
  "Replace all values in D1 with those named correspondingly in D2.
Defined keywords:
 :IF-DOES-NOT-EXIST - one of :ERROR or :CONTINUE
See the manual for details. Ha ha."
  (declare (type dictionary d1 d2))
  (iter (for (symbol id) in-hashtable (dictionary-symbols-to-ids d1))
        (if-let ((d2-id (gethash symbol (dictionary-symbols-to-ids d2))))
                (setf (id-value d1 id) (id-value d2 d2-id))
                (ecase if-does-not-exist
                  (:error (error 'dictionaries-not-form-subset-superset-relation
                           :sub d1 :super d2 :name symbol))
                  (:continue t)))))