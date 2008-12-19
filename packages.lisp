(defpackage circular-buffer
  (:nicknames :circbuf)
  (:use :common-lisp :iterate :alexandria)
  (:export
   #:circular-buffer
   #:circular-buffer-elt #:circular-buffer-elt-tailwards
   #:circular-buffer-shrink #:circular-buffer-extend
   #:circular-buffer-push
   #:circular-buffer-size #:circular-buffer-limit
   #:circular-buffer-sublist-headwards
   #:do-circular-buffer))

(defpackage depsolver
  (:use :cl :alexandria :pergamum)
  (:export
   #:depobj #:depend #:undepend #:satisfied-p
   #:map-dependencies #:map-reverse-dependencies
   #:solve))

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

(defpackage discrimination
  (:use :cl :alexandria)
  (:export
   #:discriminator #:binary-discriminator #:set-discriminator #:discriminator-sub #:discriminator-subs
   #:discrimination-error #:discrimination-value-unbound #:discriminate
   #:discriminator-by-id-path #:discriminator-by-value-path))

(defpackage early-eval
  (:use :common-lisp :alexandria :pergamum)
  (:export
   #:environment #:make-environment #:environment-byte #:environment-bindings
   #:constant-p
   #:sym-value
   #:eeval))

(defpackage octree-1d
  (:nicknames :oct-1d)
  (:use :common-lisp :alexandria :pergamum)
  (:export
   #:tree #:make-tree #:invalid-tree-address
   #:insert
   #:tree-left #:tree-right
   #:mapc-tree-values #:do-tree-values
   #:tree-list))

(defpackage state
  (:use :cl :alexandria)
  (:export
   #:machine #:make-machine #:machine-parameter
   #:deftransition
   #:state))