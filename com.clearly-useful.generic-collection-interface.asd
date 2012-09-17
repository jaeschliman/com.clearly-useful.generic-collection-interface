;;;; com.clearly-useful.generic-collection-interface.asd

(asdf:defsystem #:com.clearly-useful.generic-collection-interface
  :serial t
  :description "Describe com.clearly-useful.generic-collection-interface here"
  :author "Jason Aeschliman <j.aeschliman@gmail.com>"
  :license "revised BSD"
  :version "0.1"
  :depends-on (#:com.clearly-useful.protocols)
  :components ((:file "package")
	       (:file "protocols")
	       (:file "macros")
	       (:file "methods")
	       (:file "conversions")
	       (:file "base-methods")
	       (:file "default-methods")
	       (:file "builtins")
	           
               (:file "com.clearly-useful.generic-collection-interface")))

