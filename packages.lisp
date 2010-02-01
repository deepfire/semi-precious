(defpackage #:environment
  (:nicknames :env)
  (:use :common-lisp :alexandria :iterate :pergamum)
  (:export
   #:environment #:alist-environment #:hash-table-environment #:meta-environment #:immutable-environment
   #:frame-chain #:reverse-environment #:frame #:immutable-frame
   #:env-mapping #:env-frames #:env-reverse-mapping
   #:environment-condition #:environment-error #:simple-environment-error
   #:environment-name-already-bound #:environment-immutable #:environment-name-not-bound #:environment-value-not-bound #:environment-frame-chain-empty
   #:do-lookup #:lookup #:name-bound-p #:bind #:do-unbind #:unbind #:set-value #:env-alist
   #:map-environment #:copy-environment-to #:copy-environment
   #:unbind-by-value #:lookup-name
   #:with-metaenvironment #:establish-environment #:release-environment #:find-environment #:with-environment
   #:with-fresh-frame #:do-frame-bindings #:bottom-frame #:find-frame #:with-fresh-frame-bindings))

(defpackage #:allocation-pool
  (:nicknames :allocpool)
  (:use :common-lisp :alexandria :iterate :pergamum :environment)
  (:export
   #:pool-environment #:pool-backed-frame-chain
   #:allocation-condition #:allocation-error #:simple-allocation-error
   #:env-freelist #:env-pool
   #:make-pool #:pool-allocate #:pool-release #:with-pool-allocation
   #:make-pool-backed-frame-chain #:pool-evaluate #:with-pool-subset #:pool-allocate-binding))

(defpackage #:circular-buffer
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

(defpackage #:depsolver
  (:use :common-lisp :alexandria :pergamum)
  (:export
   #:depobj #:depend #:undepend #:satisfied-p
   #:map-dependencies #:map-reverse-dependencies
   #:solve))

(defpackage #:dictionary
  (:use :common-lisp :alexandria :iterate)
  (:export
   #:dictionary #:make-dictionary
   #:aliased-dictionary #:make-aliased-dictionary
   #:unknown-symbol #:dictionaries-not-form-subset-superset-relation
   #:dictionary-id-map #:symbol-id #:id-value #:set-id-value #:symbol-present-p #:translation #:set-translation
   #:add-symbol-unchecked #:add-symbol
   #:add-alias-unchecked #:add-alias
   #:copy-dictionary
   #:dictionary-subset-p
   #:submerge-dictionary-to))

(defpackage #:discrimination
  (:use :common-lisp :alexandria :pergamum)
  (:export
   #:discrimination-condition #:discrimination-error #:discrimination-value-unbound #:condition-value
   #:make-discrimination-tree
   #:discriminate
   #:*discriminate-verbosely*))

(defpackage #:early-eval
  (:use :common-lisp :alexandria :pergamum)
  (:export
   #:environment #:make-environment #:environment-byte #:environment-bindings
   #:constant-p
   #:sym-value
   #:eeval))

(defpackage #:meta
  (:use :common-lisp :alexandria :pergamum :iterate)
  (:export
   #:explore-package #:describe-package #:package-undocumented-symbols
   #:imbue))

(defpackage #:octree-1d
  (:nicknames :oct-1d)
  (:use :common-lisp :alexandria :pergamum)
  (:export
   #:tree #:make-tree #:invalid-tree-address
   #:insert
   #:tree-left #:tree-right
   #:mapc-tree-values #:do-tree-values
   #:tree-list))

(defpackage #:state
  (:use :common-lisp :alexandria)
  (:export
   #:machine #:make-machine #:machine-parameter
   #:transition-action #:set-transition-action
   #:state))