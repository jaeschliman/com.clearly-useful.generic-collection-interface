(in-package #:com.clearly-useful.generic-collection-interface)

;; countable

(defmethod counted-p (thing)
  (counted-p (counted-collection thing)))

(defmethod len (thing)
  (len (counted-collection thing)))

;; indexable

(defmethod idx (thing n)
  (idx (indexed-collection thing) n))

;; seq

(defmethod fst (thing)
  (fst (seq thing)))

(defmethod rst (thing)
  (rst (seq thing)))

;; associative

(defmethod keys (thing)
  (keys (associative-collection thing)))

(defmethod vals (thing)
  (vals (associative-collection thing)))

(defmethod key (thing k)
  (key (associative-collection thing) k))

