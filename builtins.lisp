(in-package #:com.clearly-useful.generic-collection-interface)

(extend-type null
  collection
  (empty (o) nil)
  (empty-p (o) t)
  (in (o v) (declare (ignore v)) nil)
  
  seqable
  (seq (o) nil)
  seq
  (fst (o) nil)
  (rst (o) nil)

  counted-collection
  (counted-p (o) t)
  (len (o) 0)

  indexed-collection
  (idx (o index) (%seq-nth-or-error o index))

  associative-collection
  (keys (o) nil)
  (vals (o) nil)
  (key (o k) (declare (ignore k)) (values nil nil)) 

  reduceable
  (coll-reduce (self fn seed) (declare (ignorable self)
				       (ignorable fn)) seed))

(extend-type cons
  collection
  (empty (o) nil)
  (empty-p (o) nil)
  (in (o v)
      (doseq (x o nil)
        (when (equalp x v)
          (return-from in t))))
  
  seqable
  (seq (o) o)
  seq
  (fst (o) (car o))
  (rst (o) (seq (cdr o)))

  counted-collection
  (counted-p (o) (declare (ignorable o)) nil)
  (len (o) (%count-seq o))

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
  (in (it v) (find v it :test #'equalp))
  

;;;;;;this should return an actual list-like structure.
  ;;;;it was cool playing with the displaced-arrays, but
  ;;;;it's not clear, or practical
  seqable
  (seq (it) (when (plusp (length it))
	      it))

  
  seq
  (fst (it) (elt it 0))
  (rst (it) (let ((len (1- (length it))))
	       (unless (zerop len)
		 (make-array len
			     :displaced-to it
			     :displaced-index-offset 1
			     :element-type
			     (array-element-type it)))))
  counted-collection
  (counted-p (it) t)
  (len (it) (length it))

  indexed-collection
  (idx (it n) (elt it n))
  
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
  (in (it key)
      (and (non-negative-integer-p key)
           (< key (array-total-size it))))
  seqable
  (seq (it) (when (plusp (array-total-size it))
	      (make-array (array-total-size it)
			  :displaced-to it
			  :displaced-index-offset 0
			  :element-type
			  (array-element-type it))))
  
  counted-collection
  (counted-p (it) t)
  (len (it) (array-total-size it))

  indexed-collection
  (idx (it n) (row-major-aref it n))

  #|
  associative-collection
  (keys (it) (make-%range :low 0 :high (array-total-size it)))
  (vals (it) (make-array (array-total-size it)
			       :displaced-to it
			       :displaced-index-offset 0
			       :element-type
			       (array-element-type it)))
  
  (key (it key)
		 (if (contains-key-p it key)
		     (values (row-major-aref it key) t)
		     (values nil nil)))
|#
  
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
  (in (o k) (nth-value 1 (gethash k o)))
  
  seqable
  (seq (it) (when (plusp (hash-table-count it))
	      (all-keys-and-values it)))

  counted-collection
  (counted-p (it) t)
  (len (it) (hash-table-count it))

  associative-collection
  (keys (it) (hash-table-keys it))
  (vals (it) (hash-table-values it))
  (key (o k) (gethash k o))

  reduceable
  (coll-reduce (self fn seed)
	       (let ((r seed))
		 (maphash (lambda (k v)
			    (setf r (funcall fn r (list k v))))
			  self)
		 r)))




