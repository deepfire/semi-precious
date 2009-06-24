(defpackage #:environment
  (:nicknames :env)
  (:use :common-lisp :alexandria :iterate :pergamum)
  (:export
   #:environment #:alist-environment #:hash-table-environment #:meta-environment #:top-level-environment #:reverse-environment #:lexical-environment
   #:env-mapping #:env-lexical-frames #:env-reverse-mapping
   #:environment-condition #:environment-error #:simple-environment-error #:environment-name-already-bound #:environment-name-not-bound #:environment-value-not-bound
   #:bind #:name-bound-p #:do-unbind #:unbind #:value #:set-value #:env-alist 
   #:evaluate
   #:unbind-by-value #:name
   #:with-metaenvironment #:establish-environment #:release-environment #:find-environment #:with-environment
   #:name-lexical-p #:with-fresh-lexical-frame #:allocate-lexical #:undo-lexical #:lexical #:set-lexical #:with-lexical-frame-bindings #:allocate-lexical-binding))

(defpackage #:allocation-pool
  (:nicknames :allocpool)
  (:use :common-lisp :alexandria :iterate :pergamum :environment)
  (:export
   #:pool-environment #:top-level-pool-environment
   #:allocation-condition #:allocation-error #:simple-allocation-error
   #:env-freelist
   #:make-top-level-pool
   #:allocate #:release #:with-pool-allocation #:pool-let #:pool-allocate-lexical))

(defpackage #:tracker
  (:use :common-lisp :alexandria :iterate :pergamum :environment)
  (:export
   #:tracker-environment #:top-level-tracker-environment
   #:make-top-level-tracker
   #:map-tracked-keys #:tracker-set-global-key-value-and-finalizer #:tracker-add-global-key-value-and-finalizer #:tracker-release-key-and-process-references
   #:tracker-let #:map-tracker-key-references #:tracker-set-key-value-and-finalizer #:tracker-reference-key))

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
   #:unknown-symbol #:dictionaries-not-form-subset-superset-relation
   #:dictionary-id-map #:symbol-id #:id-value #:set-id-value #:symbol-present-p #:translation #:set-translation
   #:add-symbol-unchecked #:add-symbol
   #:copy-dictionary
   #:dictionary-subset-p
   #:submerge-dictionary-to))

(defpackage #:discrimination
  (:use :common-lisp :alexandria)
  (:export
   #:discriminator #:binary-discriminator #:set-discriminator #:discriminator-sub #:discriminator-subs
   #:discrimination-condition #:discrimination-error #:discrimination-value-unbound #:discrimination-value-unbound-error #:discrimination-value-unbound-value #:discriminate
   #:discriminator-by-id-path #:discriminator-by-value-path))

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
   #:explore-package #:describe-package #:package-undocumented-symbols #:imbue))

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