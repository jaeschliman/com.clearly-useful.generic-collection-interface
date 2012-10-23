(in-package #:com.clearly-useful.generic-collection-interface)

(extend-type null
  collection
  (empty (o) nil)
  (empty-p (o) t)
  
  seqable
  (seq (o) nil)
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
  (contains-key-p (o k) (declare (ignorable o k)) nil)
  (value-for-key (o k) (%seq-nth-or-error o k)) ;;to give the right error message

  reduceable
  (coll-reduce (self fn seed) (declare (ignorable self)
				       (ignorable fn)) seed))

(extend-type cons
  collection
  (empty (o) nil)
  (empty-p (o) nil)
  
  seqable
  (seq (o) o)
  seq
  (head (o) (car o))
  (tail (o) (cdr o))

  countable
  (counted-p (o) (declare (ignorable o)) nil)
  (count-elements (o) (%count-seq o))

  associative
  (all-keys (o) (make-%range :low 0 :high (count-elements o)))
  (all-values (o) o)
  (contains-key-p (o key)
		  (and (integerp key))
		  (<= -1 key (count-elements o)))
  (value-for-key (o index)
		 (%seq-nth-or-error o index))

  reduceable
  (coll-reduce (self fn seed)
	       (let ((r seed))
		 (doseq (v self r)
		   (setf r (funcall fn r v))))))


(extend-type vector
  collection
  (empty (it) (make-array 0
			  :element-type (array-element-type it)))
  (empty-p (it) (zerop (length it)))
  
  seqable
  (seq (it) (when (plusp (length it))
	      it))
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
		 (elt it key))

  reduceable
  (coll-reduce (self fn seed) (reduce fn self :initial-value seed))

  foldable
  #+com.clearly-useful.threading-supported
  (coll-fold (self n combinef reducef)
	     (fold-vector self n combinef reducef))

  #-com.clearly-useful.threading-supported
  (coll-fold (self n combinef reducef)
	     (declare (ignore n))
	     (reduce reducef self :initial-value (funcall combinef)))
  )

(extend-type array
  ;;should this retain the dimensionality of array
  ;;or return an empty vector?
  ;;keeping the rank around for now, seems
  ;;better to avoid information loss...
  collection
  (empty (it) (make-array (make-list (array-rank it)
				     :initial-element 0)
			  :element-type (array-element-type it)))
  (empty-p (it) (zerop (array-total-size it)))
  
  seqable
  (seq (it) (when (plusp (array-total-size it))
	      (make-array (array-total-size it)
			  :displaced-to it
			  :displaced-index-offset 0
			  :element-type
			  (array-element-type it))))
  
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
			       (array-element-type it)))

  reduceable
  (coll-reduce (self fn seed)
	       (reduce fn (make-array (array-total-size self)
				      :displaced-to self
				      :displaced-index-offset 0
				      :element-type (array-element-type self))
		       :initial-value seed)))



(defmethod all-keys-and-values ((a hash-table))
  (loop for k being the hash-keys of a
     using (hash-value v)
     collect (list k v)))


(extend-type hash-table
  collection
  (empty (it) (%empty-hash it))
  (empty-p (it) (zerop (hash-table-count it)))
  
  seqable
  (seq (it) (when (plusp (hash-table-count it))
	      (all-keys-and-values it)))
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
			    k o)))

  reduceable
  (coll-reduce (self fn seed)
	       (let ((r seed))
		 (maphash (lambda (k v)
			    (setf r (funcall fn r (list k v))))
			  self)
		 r)))




