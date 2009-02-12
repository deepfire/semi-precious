;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: META; Base: 10 -*-
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


(in-package :meta)

(defgeneric classification (type)
  (:method ((type (eql 'variable))) "Global variable")
  (:method ((type (eql 'function))) "Function")
  (:method ((type (eql 'macro))) "Macro")
  (:method ((type (eql 'class))) "Class"))

(defgeneric plural-suffix (type)
  (:method ((type (eql 'variable))) "s")
  (:method ((type (eql 'function))) "s")
  (:method ((type (eql 'macro))) "s")
  (:method ((type (eql 'class))) "es"))

(defun function-arglist (fn)
  "Determine the argument list of FN."
  (let ((arglist #+sbcl (sb-introspect:function-arglist fn)
                 #-sbcl nil))
    (subseq arglist 0 (position '&aux arglist))))

(defgeneric arglist (x type)
  (:method ((o symbol) type) nil)
  (:method ((o symbol) (type (eql 'function))) (function-arglist o))
  (:method ((o symbol) (type (eql 'macro)))    (function-arglist o)))

(defun downgraded-documentation-type (type)
  (if (eq type 'macro) 'function type))

(defun definition-source-pathname (name type)
  #+sbcl (sb-introspect:definition-source-pathname (first (sb-introspect:find-definition-sources-by-name name (make-keyword (symbol-name (downgraded-documentation-type type))))))
  #-sbcl nil)

(defun explore-package (package &aux (package (find-package package)))
  "Given a PACKAGE designator, return its external symbols, as well as
   variables, functions, macros and classes they designate, as multiple values."
  (let (externals
        variables functions macros classes)
    (do-external-symbols (sym package)
      (push sym externals)
      (when (boundp sym)
        (push sym variables))
      (if (macro-function sym)
          (push sym macros)
          (when (fboundp sym)
            (push sym functions)))
      (when (find-class sym nil)
        (push sym classes)))
    (values externals variables functions macros classes)))

(defun object-documentation (object type)
  (documentation object (downgraded-documentation-type type)))

(defun presentable-documentation (object type &aux (documentation (object-documentation object type)))
  "Return user-presentable documentation for OBJECT of TYPE and
   the presence of documentation as values."
  (values (or documentation (format nil "Undocumented ~(~A~)." type))
          (not (null documentation))))

(defun describe-symbol-block (stream type symbols describe-undocumented)
  (flet ((long-description-p (docstring) (find #\Newline docstring)))
    (iter (for sym in (sort symbols #'string< :key #'symbol-name))
          (destructuring-bind (word &rest params) (mapcar #'list (ensure-list (nfsubst (fif #'symbolp #'symbol-name #'write-to-string) (cons sym (arglist sym type)))))
            (format stream (format nil "~~@<~A ~~;~~{~~<~~A~~:_~~:@> ~~}~~:@>~~%" (car word)) params))
          (multiple-value-bind (documentation documented-p) (presentable-documentation sym type)
            (when (or documented-p describe-undocumented)
              (format stream "    ~<~W~:@>~:[~;~%~]~%" documentation (long-description-p documentation)))))))

(defgeneric blocking-parameters (discriminator type)
  (:method ((d (eql :first-letter)) type)
    (values (compose (rcurry #'aref 0) #'symbol-name)
            (list #'char< :key #'car)
            (constantly #.(make-string 1 :initial-element #\Newline))))
  (:method ((d (eql :definition-filename)) type)
    (values (rcurry #'definition-source-pathname type)
            (list #'string< :key (compose #'pathname-name #'car))
            (lambda (pathname) (format nil "~%===[ () ~A.~A:~%~%" (pathname-name pathname) (pathname-type pathname))))))

(defun describe-symbols (symbols type &key (stream t) (blocking-threshold 15) (blocking-discriminator :first-letter) (describe-undocumented t))
  "Describe SYMBOLS designating objects of TYPE to STREAM.

   If the amount of symbols exceeds BLOCKING-THRESHOLD, split the descriptions
   into blocks, prepended with BLOCK-ANNOTATION, and grouped by the criterion
   designated by BLOCKING-DISCRIMINATOR.

   Valid values are:
     for BLOCK-ANNOTATION:
       :NEWLINE, meaning a newline annotation,
       :DEFINITION-FILENAME, meaning the name where the respective symbol was
          defined as TYPE,
     for BLOCKING-DISCRIMINATOR:
       :FIRST-LETTER, meaning the first letter of symbol name,
       :DEFINITION-FILENAME, meaning the pathname where the respective symbol 
          was defined as TYPE."
  (format stream "~%~A~A:~%~%" (classification type) (plural-suffix type))
  (if (< (length symbols) blocking-threshold)
      (describe-symbol-block stream type symbols describe-undocumented)
      (let ((buckets (make-hash-table :test 'equal)))
        (multiple-value-bind (discriminator-function block-sort-parameters annotation-function) (blocking-parameters blocking-discriminator type)
          (dolist (sym symbols)
            (push sym (gethash (funcall discriminator-function sym) buckets)))
          (let ((buckets (apply #'sort (hash-table-alist buckets) block-sort-parameters)))
            (iter (for (key . bucket) in buckets)
                  (write-sequence (funcall annotation-function key) (xform-if (feq t) (constantly *standard-output*) stream))
                  (describe-symbol-block stream type bucket describe-undocumented)))))))

(defun describe-package (package &rest keys &key (stream t) (blocking-threshold 15) (blocking-discriminator :first-letter) (describe-undocumented t))
  "Describe symbols in PACKAGE to STREAM.

   If the amount of symbols exceeds BLOCKING-THRESHOLD, split the descriptions
   into blocks, prepended with BLOCK-ANNOTATION, and grouped by the criterion
   designated by BLOCKING-DISCRIMINATOR.

   Valid values are:
     for BLOCK-ANNOTATION:
       :NEWLINE, meaning a newline annotation,
       :DEFINITION-FILENAME, meaning the name where the respective symbol was
          defined as TYPE,
     for BLOCKING-DISCRIMINATOR:
       :FIRST-LETTER, meaning the first letter of symbol name,
       :DEFINITION-FILENAME, meaning the pathname where the respective symbol 
          was defined as TYPE."
  (declare (ignore blocking-threshold blocking-discriminator describe-undocumented))
  (destructuring-bind (exported &rest sym-sets) (multiple-value-list (explore-package package))
    (when exported
      (format stream "Package ~A, has ~D external symbols, among them:~%"
              (package-name package) (length exported))
      (iter (for sym-set in sym-sets)
            (for type in '(variable function macro class))
            (when sym-set
              (apply #'describe-symbols sym-set type :stream t keys))))))

(defun package-undocumented-symbols (package)
  "Return the list of PACKAGE's external symbols which have
   undocumented objects associated with them."
  (destructuring-bind (exported &rest sym-sets) (multiple-value-list (explore-package package))
    (declare (ignore exported))
    (lret (undocumented)
      (iter (for sym-set in sym-sets)
            (for type in '(variable function macro class))
            (appendf undocumented (remove-if (rcurry #'object-documentation type) sym-set))))))