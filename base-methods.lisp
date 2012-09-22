(in-package #:com.clearly-useful.generic-collection-interface)

;;; base methods


(defmethod seq (o)
  (etypecase o
    (associative (seq (all-keys-and-values o)))
    (indexable (%countable-to-seq o))
    (t (error "no method to convert ~S ~S to ~S"
	      (class-of o) o 'seq))))

(defmethod associative (o)
  (etypecase o
    (indexable
     (%countable-to-associative o))
    (seq (%seq-to-associative o))
    (t (error "no method to convert ~S ~S to ~S"
	      (class-of o) o 'associative))))

(defmethod indexable (o)
  (etypecase o
    (seq (%seq-to-countable o))
    (associative (%associative-to-indexable o))
    (t (error "no method to convert ~S ~S to ~S"
	      (class-of o) o 'indexable))))

(defmethod countable (o)
  (etypecase o
    (seq (%seq-to-countable o))
    (associative (%seq-to-countable (seq o)))
    (t (error "no method to convert ~S ~S to ~S"
	      (class-of o) o 'countable))))
