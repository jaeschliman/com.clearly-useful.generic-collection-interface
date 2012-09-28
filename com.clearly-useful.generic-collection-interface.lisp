;;;; com.clearly-useful.generic-collection-interface.lisp

(in-package #:com.clearly-useful.generic-collection-interface)

(define-interface-package com.clearly-useful.collection-protocol
    "The collection protocol"
  collection
  collection-p
  empty
  empty-p
  
  )

(define-interface-package com.clearly-useful.sequence-protocol
    ""
  collection
  collection-p
  empty
  empty-p

  seqable
  seqable-p
  seq
  seq-p
  head
  tail
  
  doseq
  seq-to-list
  
  )
(define-interface-package com.clearly-useful.countable-protocol
    ""
  collection
  collection-p
  empty
  empty-p
  
  countable
  countable-p
  counted-p
  count-elements
  )
(define-interface-package com.clearly-useful.indexable-protocol
    ""
  collection
  collection-p
  empty
  empty-p

  countable
  countable-p
  counted-p
  count-elements

  indexable
  indexable-p
  element-at

  doindexable
  
  )
(define-interface-package com.clearly-useful.associative-protocol
    ""
  collection
  collection-p
  empty
  empty-p

  associative
  all-keys
  all-values
  contains-key-p
  value-for-key

  all-keys-and-values
  getkey
  
  )
(define-interface-package com.clearly-useful.reduceable-protocol
    ""
  reduceable
  reduceable-p
  coll-reduce
  reduced
  fold-left
  reducer
  mapping
  filtering
  mapcatting
  monoid
  
  )
(define-interface-package com.clearly-useful.foldable-protocol
    ""
  reduceable
  reduceable-p
  coll-reduce
  reduced
  fold-left
  reducer
  mapping
  filtering
  mapcatting
  monoid
  folder
  foldable-p
  foldable
  coll-fold
  fold
  )

#|(define-interface-package com.clearly-useful.

    ""
  collection
  collection-p
  empty
  empty-p
  
  )
|#





