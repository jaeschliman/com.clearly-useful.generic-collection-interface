(in-package #:com.clearly-useful.generic-collection-interface)

;;; base methods


(defmethod seq (o)
  (etypecase o
    (associative-collection (seq (all-keys-and-values o)))
    (indexed-collection (%indexable-to-seq o))
    (t (error "no method to convert ~S ~S to ~S"
	      (class-of o) o 'seq))))

(defmethod associative-collection (o)
  (etypecase o
    (indexed-collection
     (%indexable-to-associative o))
    (seq (%seq-to-associative o))
    (t (error "no method to convert ~S ~S to ~S"
	      (class-of o) o 'associative-collection))))

(defmethod indexed-collection (o)
  (etypecase o
    (seq (%seq-to-indexable o))
    (associative-collection (%associative-to-indexable o))
    (t (error "no method to convert ~S ~S to ~S"
	      (class-of o) o 'indexed-collection))))

(defmethod counted-collection (o)
  (etypecase o
    (seq (%seq-to-indexable o))
    (associative (%seq-to-indexable (seq o)))
    (t (error "no method to convert ~S ~S to ~S"
	      (class-of o) o 'counted-collection))))


(defmethod coll-reduce (self fn seed)
   (etypecase self
     (indexed-collection (if (counted-p self)
                             (reduce-indexable self fn seed)
                             (reduce-seq (seq self) fn seed)))
     (seq (reduce-seq self fn seed))
     (associative-collection (reduce-seq (seq self) fn seed))
     (t (error "Don't know how to ~S ~S" 'coll-reduce self))))

(defmethod coll-fold (self n cm rd)
  (declare (ignore n))
  (fold-left rd self :initial-value (funcall cm)))
