;;;; com.clearly-useful.generic-collection-interface.asd

(asdf:defsystem #:com.clearly-useful.generic-collection-interface
  :serial t
  :description "generic collection interfaces for common lisp"
  :author "Jason Aeschliman <j.aeschliman@gmail.com>"
  :license "revised BSD"
  :version "0.2"
  :depends-on (#:com.clearly-useful.protocols
	       #:bordeaux-threads
	       #:lparallel)
  
  :components ((:file "package")
	       (:file "features")
	       (:file "protocols")
	       (:file "internal-utils")
	       (:file "macros")
	       (:file "methods")
	       (:file "conversions")
	       (:file "reduceable-base")
	       (:file "foldable-base")
	       (:file "base-methods")
	       (:file "default-methods")
	       (:file "conj")
	       (:file "parallel")
	       (:file "builtins")
               (:file "com.clearly-useful.generic-collection-interface"))

  :properties ((:com.clearly-useful
		:test :auto)))

