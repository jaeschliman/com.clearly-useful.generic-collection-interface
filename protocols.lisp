(in-package #:com.clearly-useful.generic-collection-interface)

(defprotocol collection
  "a collection knows if it is empty, and can produce an empty version of itself"
  (empty (o) "an empty version of this collection")
  (empty-p (o) "wether this collection is empty")
  (in (o object) "nil if this collection does not contain object"))

(defprotocol counted-collection
  "countable collection"
  (:require collection)
  (:eponymous-method t)
  (counted-p (o) "whether a collection is counted in constant time")
  (len (o) "the number of elements in o"))

(defprotocol indexed-collection
  "a thing that may be indexed"
  (:require collection)
  (:require counted-collection)
  (:eponymous-method t)
  (idx (o index) "the element of o at index, or raise an error"))

(defprotocol seqable
  "a thing that may be converted to a seq
default implementation provided for indexable,
associative and seq."
  (seq (o) "return a seq (or nil if count = 0) or error"))

(defprotocol seq
  "a list-like abstraction"
  (:require collection)
  (:require seqable)
  (fst (o) "first element")
  (rst (o) "the rest of the collection or nil"))

(defprotocol associative-collection
  "a dictionary-like abstraction"
  (:require collection)
  (:eponymous-method t)
  (keys (o) "a seq of all keys in o")
  (vals (o) "a seq of all values in o")
  (key (o k) "returns the value of k in o and t, or (values nil nil)"))



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
