;;;; package.lisp

(defpackage #:com.clearly-useful.generic-collection-interface
  (:use #:cl
        #:alexandria
	#:com.clearly-useful.protocols
	#:lparallel)
  
  (:nicknames #:com.clearly-useful.gci)
  
  (:export
   ;;the collection protocol
   #:collection
   #:collection-p
   #:empty
   #:empty-p
   #:in
   )
  
  (:export
   ;; the countable and indexable protocols
   #:counted-collection
   #:counted-collection-p
   #:counted-p
   #:len

   #:indexed-collection
   #:indexed-collection-p
   #:idx

   #:doindexable)

  (:export
   ;; the seqable and seq prototocols
   #:seqable
   #:seqable-p
   
   #:seq

   #:seq-p
   #:fst
   #:rst
   
   #:doseq
   #:seq-to-list
   #:to-list)

  (:export
   ;; the associative protocol
   #:associative-collection
   #:associative-collection-p
   #:keys
   #:vals
   #:key

   #:all-keys-and-values
   #:getkey)
  
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

