;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DISCRIMINATION; Base: 10 -*-
;;;
;;;  (c) copyright 2007-2009 by
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

(in-package :discrimination)

;; Discrimination is a general method of organising process of identification
;; of a certain given phenomena within a set of possibilities, based on iterative
;; narrowing of the set of possible identities by performing queries about observable
;; properties of those phenomena.
;;
;; Discrimination trees are data structures which carry information
;; guiding this process of narrowing.  The nodes of that tree correspond to
;; possible states of the process of discrimination, and as such they
;; represent sets of remaining possible identities for the observed phenomena.
;; The leaves of the trees represent final identites.
;;
;; Every node, barring the leaves, has at least two edges coming out of it,
;; which represent a non-overlapping partition of the set of possibilities
;; corresponding to that node.  The choice of a particular edge is called an 'act of
;; discrimination' and is performed by a function attached to the node.
;;
;; As to phenomena which a discrimination tree is inherently unable to discern,
;; they are represented by implicit 'failure' edges corresponding to every
;; non-leaf node.


(defvar *discriminate-verbosely* nil
  "Report discrimination progress to *STANDARD-OUTPUT*.")

(define-condition discrimination-condition ()
  ((discriminator :accessor condition-discriminator :initarg :discriminator)))
(define-condition discrimination-error (error discrimination-condition) ())
(define-simple-error discrimination-error)

(define-reported-condition discrimination-value-unbound (discrimination-error)
  ((value :accessor condition-value :initarg :value))
  (:report (value)
           "~@<No binding for value ~S.~:@>" value))

(defun process-discrimination-tree-definition-node (node)
  (destructuring-bind (keyword node-value action &rest maybe-subnodes) node
    (unless (string= "NODE" (symbol-name keyword))
      (discrimination-error "~@<Malformed discrimination tree: node missing a keyword: ~S.~:@>" node))
    (unless (or (symbolp action)
                (and (consp action) (endp (cddr action)) (eq 'function (car action)) (or (symbolp (cadr action))
                                                                                         (and (consp (cadr action))
                                                                                              (eq 'lambda (caadr action))))))
      (discrimination-error "~@<Malformed discrimination tree: bad action specifier ~S in: ~S.  ~
                                Must be either a symbol, or a function specifier.~:@>" action node))
    (typecase action
      (cons
       (unless maybe-subnodes
         (discrimination-error "~@<Malformed discrimination tree: discriminating node missing subnodes: ~S.~:@>" node))
       `(list ,node-value ,action
              ,@(mapcar #'process-discrimination-tree-definition-node maybe-subnodes)))
      (symbol
       (when maybe-subnodes
         (discrimination-error "~@<Malformed discrimination tree: leaf node with subnodes: ~S.~:@>" node))
       `(list ,node-value ',action)))))

(defmacro make-discrimination-tree (&rest trees)
  (when (not (endp (rest trees)))
    (discrimination-error "~@<Malformed discrimination tree: more than one root: ~S.~:@>" trees))
  `,(process-discrimination-tree-definition-node (first trees)))

(defun discriminate (discrimination-tree object)
  (labels ((rec (node)
             (destructuring-bind (node-value action &rest subnodes) node
               (when *discriminate-verbosely*
                 (format t "~@<;; ~@;Discriminating at node ~A: ~A~:@>~%" node-value action))
               (if (symbolp action)
                   (prog1 action
                     (when *discriminate-verbosely*
                       (format t "~@<;; ~@;Found leaf at node ~A: ~A~:@>~%" node-value action)))
                   (let* ((discval (funcall action object))
                          (subnode (assoc discval subnodes)))
                     (when *discriminate-verbosely*
                       (format t "~@<;; ~@;Discriminator ~A at node ~A returned ~S.~:@>~%" action node-value discval))
                     (if subnode
                         (rec subnode)
                         (error 'discrimination-value-unbound :value discval)))))))
    (rec discrimination-tree)))