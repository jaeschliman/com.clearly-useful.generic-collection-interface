;;;; com.clearly-useful.generic-collection-interface.lisp

(in-package #:com.clearly-useful.generic-collection-interface)

(define-interface-package #:com.clearly-useful.collection-protocol
    "Interface to the collection protocol"
  #:collection
  #:collection-p
  #:empty
  #:empty-p
  #:in)

(define-interface-package #:com.clearly-useful.seq-protocol
    "Interface to the sequence protocol"
  #:collection
  #:collection-p
  #:empty
  #:empty-p
  #:in
  
  #:seqable
  #:seqable-p
  #:seq
  #:seq-p
  #:fst
  #:rst
  
  #:doseq
  #:seq-to-list)

(define-interface-package #:com.clearly-useful.counted-collection-protocol
    "Interface to the countable protocol"
  #:collection
  #:collection-p
  #:empty
  #:empty-p
  #:in
  
  #:counted-collection
  #:counted-p
  #:counted-p
  #:len)

(define-interface-package #:com.clearly-useful.indexed-collection-protocol
    "Interface to the indexable protocol"
  #:collection
  #:collection-p
  #:empty
  #:empty-p
  #:in
  
  #:counted-collection
  #:counted-collection-p
  #:counted-p
  #:len
  
  #:indexed-collection
  #:indexed-collection-p
  #:idx
  
  #:doindexable)

(define-interface-package #:com.clearly-useful.associative-collection-protocol
    "Interface to the associative protocol"
  #:collection
  #:collection-p
  #:empty
  #:empty-p
  #:in
  
  #:associative-collection
  #:associative-collection-p
  #:keys
  #:vals
  #:key
  
  #:all-keys-and-values
  #:getkey)

(define-interface-package #:com.clearly-useful.reduceable-protocol
    "Interface to the reduceable protocol"
  #:reduceable
  #:reduceable-p
  #:coll-reduce
  #:reduced
  #:fold-left
  #:reducer
  #:mapping
  #:filtering
  #:mapcatting
  #:monoid)

(define-interface-package #:com.clearly-useful.foldable-protocol
    "Interface to the foldable protocol"
  #:reduceable
  #:reduceable-p
  #:coll-reduce
  #:reduced
  #:fold-left
  #:reducer
  #:mapping
  #:filtering
  #:mapcatting
  #:monoid
  #:folder
  #:foldable-p
  #:foldable
  #:coll-fold
  #:fold)
