(in-package #:com.clearly-useful.generic-collection-interface)

;; countable

(defmethod counted-p (thing)
  (counted-p (countable thing)))

(defmethod count-elements (thing)
  (count-elements (countable thing)))

;; indexable

;;;; still not sure what to do with
;;;; indexable.

;; seq

(defmethod head (thing)
  (head (seq thing)))


(defmethod tail (thing)
  (tail (seq thing)))

;; associative

(defmethod all-keys (thing)
  (all-keys (associative thing)))

(defmethod all-values (thing)
  (all-values (associative thing)))

(defmethod contains-key-p (thing k)
  (contains-key-p (associative thing) k))

(defmethod value-for-key (thing k)
  (value-for-key (associative thing) k))

