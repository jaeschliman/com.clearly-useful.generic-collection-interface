;;;; package.lisp

(defpackage #:com.clearly-useful.generic-collection-interface
  (:use #:cl
	#:com.clearly-useful.protocols
	#:lparallel)

  (:export
   ;;the collection protocol
   #:collection
   #:collection-p
   #:empty
   #:empty-p
   )
  
  (:export
   ;; the seqable and seq prototocols
   #:seqable
   #:seqable-p
   
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

   #:doindexable)

   (:export
   ;;utilities
   #:conj
   #:into)
   
  (:export
   ;; the reducable and foldable protocols
   #:reduceable
   #:reduceable-p
   #:coll-reduce
   #:reduced
   #:fold-left

   #:foldable
   #:foldable-p
   #:coll-fold
   #:fold )

  (:export
   ;; functions for constructing reducers and folders
   #:monoid
   #:reducer
   #:folder
   #:mapping
   #:filtering
   #:mapcatting)

  )

