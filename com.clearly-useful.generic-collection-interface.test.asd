;;;; com.clearly-useful.generic-collection-interface.test.asd

(asdf:defsystem #:com.clearly-useful.generic-collection-interface.test
  :serial t
  :description "Describe com.clearly-useful.generic-collection-interface.test here"
  :author "Jason Aeschliman <j.aeschliman@gmail.com>"
  :license "revised BSD"
  :version "0.2"
  :depends-on (#:com.clearly-useful.generic-collection-interface)
  
  :components ((:module #:test
		       :components
		       ((:file "test")))))

