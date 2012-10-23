(in-package #:com.clearly-useful.generic-collection-interface)

(defprotocol collection
  "a collection knows if it is empty, and can produce an empty version of itself"
  (empty (o) "an empty version of this collection")
  (empty-p (o) "wether this collection is empty"))

(defprotocol countable
  "countable collection"
  (:require collection)
  (:eponymous-method t)
  (counted-p (o) "whether a collection is counted in constant time")
  (count-elements (o) "the number of elements in o"))

(defprotocol indexable
  "a thing that may be indexed"
  (:require collection)
  (:require countable)
  (:eponymous-method t)
  (element-at (o index) "the element of o at index, or raise an error"))

(defprotocol seqable
  "a thing that may be converted to a seq
default implementation provided for indexable,
associative and seq."
  (seq (o) "return a seq (or nil if count = 0) or error"))

(defprotocol seq
  "a list-like abstraction"
  (:require collection)
  (:require seqable)
  (head (o) "first element")
  (tail (o) "the rest of the collection or nil"))

;; note: should change the api to be more cl-like.
;; let value-for-key return 2 values instead of raising.

(defprotocol associative
  "a dictionary-like abstraction"
  (:require collection)
  (:eponymous-method t)
  (all-keys (o) "a seq of all keys in o")
  (all-values (o) "a seq of all values in o")
  (contains-key-p (o k) "whether o contains k")
  (value-for-key (o k) "returns the value of k in o or raises an error"))



(defprotocol reduceable
  ""
  ;;multi-arity & optional args not yet supported
  (coll-reduce (self fn seed))
  )
;;;;;;;

;; see https://github.com/clojure/clojure/blob/master/src/clj/clojure/core/reducers.clj

(defprotocol foldable
  ""
  (coll-fold (self n combinef reducef)))
