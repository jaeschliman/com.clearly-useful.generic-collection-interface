;;;; package.lisp

(defpackage #:com.clearly-useful.generic-collection-interface
  (:use #:cl
	#:com.clearly-useful.protocols)

  (:export
   ;; the seq protocol
   #:seq
   #:seq-p
   #:head
   #:tail
   
   #:doseq
   #:seq-to-list)

  (:export
   ;; the associative protocol
   #:associative
   #:all-keys
   #:all-values
   #:contains-key-p
   #:value-for-key

   #:all-keys-and-values
   #:getkey)
  
  
  (:export
   ;; the countable and indexable protocols
   #:countable
   #:countable-p
   #:counted-p
   #:count-elements

   #:indexable
   #:indexable-p
   #:element-at

   #:doindexable))

