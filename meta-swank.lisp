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

(defun may-imbue-stream-p (stream)
  (declare (ignorable stream))
  (swank::with-struct (swank::connection. swank::user-output swank::user-io swank::trace-output swank::repl-results) (swank::default-connection)
    (member stream (list swank::user-output swank::user-io swank::trace-output swank::repl-results))))

(defun imbue (stream object &optional colon-p at-sign-p)
  (declare (ignorable stream object) (ignore colon-p at-sign-p))
  (progn
    (assert (may-imbue-stream-p stream))
    (let ((id (and swank:*record-repl-results* (swank::save-presented-object object))))
      (finish-output stream)
      (swank::send-to-emacs `(:presentation-start ,id))
      (swank::send-to-emacs `(:write-string ,(prin1-to-string object)))
      (swank::send-to-emacs `(:presentation-end ,id))
      (finish-output stream))))
