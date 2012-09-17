(in-package #:com.clearly-useful.generic-collection-interface)

(extend-type null
  seq
  (head (o) nil)
  (tail (o) nil)

  countable
  (counted-p (o) t)
  (count-elements (o) 0)

  indexable
  (element-at (o index) (%seq-nth-or-error o index))

  associative
  (all-keys (o) nil)
  (all-values (o) nil)
  (contains-key-p (o k) nil)
  (value-for-key (o k) (%seq-nth-or-error o k)))

(extend-type cons
  seq
  (head (o) (car o))
  (tail (o) (cdr o))

  countable
  (counted-p (o) (declare (ignorable o)) nil)
  (count-elements (o) (%count-seq o))

  indexable
  (element-at (o index) (%seq-nth-or-error o index))

  associative
  (all-keys (o) (make-%range :low 0 :high (count-elements o)))
  (all-values (o) o)
  (contains-key-p (o key)
		  (and (integerp key))
		  (<= -1 key (count-elements o)))
  (value-for-key (o index)
		 (%seq-nth-or-error o index)))


(extend-type vector
  seq
  (head (it) (elt it 0))
  (tail (it) (let ((len (1- (length it))))
	       (unless (zerop len)
		 (make-array len
			     :displaced-to it
			     :displaced-index-offset 1
			     :element-type
			     (array-element-type it)))))
  countable
  (counted-p (it) t)
  (count-elements (it) (length it))

  indexable
  (element-at (it n) (elt it n))
  
  associative
  (all-keys (it) (make-%range :low 0 :high (length it)))
  (all-values (it) it)
  (contains-key-p (it key)
		  (and (integerp key)
		       (<= -1 key (length it))))
  (value-for-key (it key)
		 (elt it key)))

(extend-type array
  seq
  (head (it) (row-major-aref it 0))
  (tail (it) (make-array (1- (array-total-size it))
			 :displaced-to it
			 :displaced-index-offset 1
			 :element-type
			 (array-element-type it)))

  countable
  (counted-p (it) t)
  (count-elements (it) (array-total-size it))

  indexable
  (element-at (it n) (row-major-aref it n))

  associative
  (all-keys (it) (make-%range :low 0 :high (array-total-size it)))
  (all-values (it) (make-array (array-total-size it)
			       :displaced-to it
			       :displaced-index-offset 0
			       :element-type
			       (array-element-type it))))


(extend-type hash-table
  countable
  (counted-p (it) t)
  (count-elements (it) (hash-table-count it))

  associative
  (all-keys (it)
	    (loop for k being
		 the hash-keys of it
		 collect k))
  (all-values (it)
	      (loop for v being
		   the hash-values of it
		   collect v))
  (contains-key-p (o k)
		  (nth-value 1 (gethash k o)))
  (value-for-key (o k)
		 (if (contains-key-p o k)
		     (prog1 (gethash k o))
		     (error "no such key ~S in ~S"
			    k o))))

(defmethod all-keys-and-values ((a hash-table))
  (loop for k being the hash-keys of a
       using (hash-value v)
       collect (list k v)))


