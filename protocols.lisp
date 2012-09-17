(in-package #:com.clearly-useful.generic-collection-interface)

(defprotocol countable
  "countable collection"
  (counted-p (o) "whether a collection is counted in constant time")
  (count-elements (o) "the number of elements in o"))

(defprotocol indexable
  "a thing that may be indexed"
  (:require countable)
  (element-at (o index) "the element of o at index, or raise an error"))

(defprotocol seq
  "a list-like abstraction"
  (head (o) "first element")
  (tail (o) "the rest of the collection or nil"))

(defprotocol associative
  "a dictionary-like abstraction"
  (all-keys (o) "a seq of all keys in o")
  (all-values (o) "a seq of all values in o")
  (contains-key-p (o k) "whether o contains k")
  (value-for-key (o k) "returns the value of k in o or raises an error"))


