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

  ;;;;;;;;;;; the following are implemented
  ;; in the reducer and foldable packages
  ;; which aren't yet public.
  ;; their implementations will be
  ;; moving here asap. exporting for
  ;; now to ensure no symbol clashes in the
  ;; meantime.
  
   (:export
   ;;utilities
   #:empty-p
   #:conj
   #:into
   #:monoid
   
   )
   
  (:export
   ;; the reducable and foldable protocols
   #:reduceable
   #:reduceable-p
   #:coll-reduce
   #:reducer
   #:reduced
   #:fold-left

   #:foldable
   #:foldable-p
   #:coll-fold
   #:folder
   #:fold )

  (:export
   ;; fold/reducer utilities (should these be exported?)
   ;; maybe something that integrates seq/reducers etc...
   ;; defining macro or something
   #:defcurried  ;;(this isn't it)

   #:mapping
   #:filtering
   #:mapcatting
   
   
   #:fmap
   #:fmapcat
   #:ffilter
   #:fremove
   #:fflatten
   #:fflatten-if

   #:take-while
   #:take
   #:drop
   )
  )

