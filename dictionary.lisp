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

(defpackage dictionary
  (:use :cl :alexandria :iterate)
  (:export
   ;; types
   #:dictionary
   ;; conditions
   #:unknown-symbol #:dictionaries-not-form-subset-superset-relation
   ;; accessors
   #:symbol-id #:id-value #:set-id-value #:symbol-present-p
   ;;
   #:add-symbol-unchecked #:add-symbol
   #:copy-dictionary
   #:dictionary-subset-p
   #:submerge-dictionary-to))

(in-package :dictionary)

(defstruct dictionary
  (ids-to-values (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (symbols-to-ids (make-hash-table :test 'eq) :type hash-table)
  (growth-size 16 :type (integer (0))))

(define-condition unknown-symbol (cell-error) ())

(define-condition symbol-already-present (cell-error) ())

(define-condition dictionaries-not-form-subset-superset-relation (unknown-symbol)
  ((sub :accessor dictionary-mismatch-sub :initarg :sub)
   (super :accessor dictionary-mismatch-super :initarg :super))
  (:report (lambda (c s)
             (format s "~@<Dictionary ~S does not form a subset of ~S, symbol ~S is missing.~:@>"
                     (dictionary-mismatch-sub c) (dictionary-mismatch-super c) (cell-error-name c)))))

(declaim (inline symbol-id))
(defun symbol-id (dictionary symbol)
  "Return the id of SYMBOL in DICTIONARY."
  (declare (type dictionary dictionary) (type symbol symbol))
  (or (gethash symbol (dictionary-symbols-to-ids dictionary))
      (error 'cell-error :name symbol)))

(declaim (inline symbol-present-p))
(defun symbol-present-p (dictionary symbol)
  "Determine if SYMBOL is present in DICTIONARY."
  (declare (type dictionary dictionary) (type symbol symbol))
  (nth-value 1 (gethash symbol (dictionary-symbols-to-ids dictionary))))

(declaim (inline id-value))
(defun id-value (dictionary id)
  "Return the value associated with ID in DICTIONARY."
  (declare (type dictionary dictionary) (type (integer 0) id))
  (aref (dictionary-ids-to-values dictionary) id))

(declaim (inline set-id-value))
(defun set-id-value (new-val dictionary id)
  "Set the value associated with ID in DICTIONARY to NEW-VAL."
  (declare (type dictionary dictionary) (type (integer 0) id))
  (setf (aref (dictionary-ids-to-values dictionary) id) new-val))

(defsetf id-value set-id-value)

(declaim (inline add-symbol-unchecked))
(defun add-symbol-unchecked (dictionary symbol value &aux (values-vec (dictionary-ids-to-values dictionary)))
  "Adds SYMBOL -> VALUE mapping into DICTIONARY.
   Does not check if SYMBOL is already present."
  (setf (gethash symbol (dictionary-symbols-to-ids dictionary))
        (1- (vector-push-extend value values-vec (dictionary-growth-size dictionary)))))

(defun add-symbol (dictionary symbol value)
  "Adds SYMBOL -> VALUE mapping into DICTIONARY.
   If SYMBOL is already present, an error is raised."
  (when (symbol-present-p dictionary symbol)
    (error 'symbol-already-present :name symbol))
  (add-symbol-unchecked dictionary symbol value))

(defun copy-dictionary (d)
  "Return a copy of dictionary D."
  (declare (type dictionary d))
  (make-dictionary :ids-to-values (copy-array (dictionary-ids-to-values d)) 
                   :symbols-to-ids (copy-hash-table (dictionary-symbols-to-ids d))))

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