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


(defmethod coll-reduce (self fn seed)
   (etypecase self
     (indexable (if (counted-p self)
		    (reduce-indexable self fn seed)
		    (reduce-seq (seq self) fn seed)))
     (seq (reduce-seq self fn seed))
     (associative (reduce-seq (seq self) fn seed))
     (t (error "Don't know how to ~S ~S" 'coll-reduce self))))

(defmethod coll-fold (self n cm rd)
  (declare (ignore n))
  (fold-left rd self :initial-value (funcall cm)))
